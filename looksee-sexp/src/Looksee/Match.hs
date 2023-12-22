module Looksee.Match
  ( MatchErr (..)
  , LocMatchErr (..)
  , MatchM
  , SeqMatchM
  , runMatchM
  , annoM
  , memoM
  , matchM
  , listM
  , lookM
  , elemM
  , restM
  , altM
  , anySymM
  , symM
  , anyIntM
  , intM
  , anySciM
  , sciM
  , anyStrM
  , strM
  )
where

import Bowtie (Anno (..), Memo (..), pattern MemoP)
import Bowtie qualified as B
import Control.Monad (ap, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), ask, asks, local)
import Control.Monad.State (MonadState (..), StateT, runStateT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Looksee.Sexp (Atom (..), AtomType (..), Brace, SexpF (..), SexpType (..), Symbol (..))

data MatchErr e r
  = MatchErrType !SexpType
  | MatchErrNotEq !Atom
  | MatchErrListElem !Int
  | MatchErrListRem
  | MatchErrAlt !(Seq (Text, r))
  | MatchErrEmbed !e
  deriving stock (Eq, Ord, Show)

newtype LocMatchErr e s = LocMatchErr
  { unLocMatchErr :: Anno s (MatchErr e (LocMatchErr e s))
  }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype MatchM e s a = MatchM {unMatchM :: ReaderT (Memo SexpF s) (Either (LocMatchErr e s)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Memo SexpF s), MonadError (LocMatchErr e s))

data SeqMatchM e s a where
  SeqMatchPure :: a -> SeqMatchM e s a
  SeqMatchEmbed :: MatchM e s (SeqMatchM e s a) -> SeqMatchM e s a
  SeqMatchElem :: MatchM e s x -> (x -> SeqMatchM e s a) -> SeqMatchM e s a
  SeqMatchRest :: MatchM e s x -> (Seq x -> SeqMatchM e s a) -> SeqMatchM e s a

instance Functor (SeqMatchM e s) where
  fmap f = go
   where
    go = \case
      SeqMatchPure a -> SeqMatchPure (f a)
      SeqMatchEmbed mr -> SeqMatchEmbed (fmap go mr)
      SeqMatchElem mx k -> SeqMatchElem mx (go . k)
      SeqMatchRest mx k -> SeqMatchRest mx (go . k)

instance Applicative (SeqMatchM e s) where
  pure = SeqMatchPure
  (<*>) = ap

instance Monad (SeqMatchM e s) where
  return = pure
  r0 >>= f = go r0
   where
    go = \case
      SeqMatchPure a -> f a
      SeqMatchEmbed mr -> SeqMatchEmbed (fmap go mr)
      SeqMatchElem mx k -> SeqMatchElem mx (go . k)
      SeqMatchRest mx k -> SeqMatchRest mx (go . k)

runMatchM :: MatchM e s a -> Memo SexpF s -> Either (LocMatchErr e s) a
runMatchM = runReaderT . unMatchM

annoM :: MatchM e s a -> MatchM e s (Anno s a)
annoM m = asks (Anno . B.memoKey) <*> m

memoM :: MatchM e s (f (Memo f s)) -> MatchM e s (Memo f s)
memoM m = asks (MemoP . B.memoKey) <*> m

errM :: MatchErr e (LocMatchErr e s) -> MatchM e s a
errM e = do
  s <- ask
  throwError (LocMatchErr (Anno (B.memoKey s) e))

embedM :: e -> MatchM e s a
embedM = errM . MatchErrEmbed

matchM :: (SexpF (Memo SexpF s) -> Either (MatchErr e (LocMatchErr e s)) a) -> MatchM e s a
matchM f = do
  s <- ask
  case f (B.memoVal s) of
    Left e -> throwError (LocMatchErr (Anno (B.memoKey s) e))
    Right a -> pure a

data S s = S !Int !(Seq (Memo SexpF s))
  deriving stock (Eq, Ord, Show)

listM :: Brace -> SeqMatchM e s a -> MatchM e s a
listM = listFromM 0

-- helper for listFromM, but needs type sig
goSeqX :: SeqMatchM e s a -> StateT (S s) (MatchM e s) a
goSeqX = \case
  SeqMatchPure a -> pure a
  SeqMatchEmbed mr -> lift mr >>= goSeqX
  SeqMatchElem mx k -> do
    S i cs <- get
    let i' = i + 1
    case cs of
      Empty -> lift (errM (MatchErrListElem i'))
      c :<| cs' -> do
        put (S i' cs')
        x <- lift (local (const c) mx)
        goSeqX (k x)
  SeqMatchRest mx k -> goRestX mx k

-- helper for listFromM, but needs type sig
goRestX :: MatchM e s x -> (Seq x -> SeqMatchM e s a) -> StateT (S s) (MatchM e s) a
goRestX mx k = go Empty
 where
  go !acc = do
    S i cs <- get
    let i' = i + 1
    case cs of
      Empty -> goSeqX (k acc)
      c :<| cs' -> do
        put (S i' cs')
        x <- lift (local (const c) mx)
        go (acc :|> x)

listFromM :: Int -> Brace -> SeqMatchM e s a -> MatchM e s a
listFromM i0 b0 r = goStart
 where
  goStart = do
    s <- ask
    case B.memoVal s of
      SexpListF b cs0 | b == b0 -> do
        let s0 = S i0 (Seq.drop i0 cs0)
        (a, S _ cs1) <- runStateT (goSeqX r) s0
        case cs1 of
          Empty -> pure a
          _ -> errM MatchErrListRem
      _ -> errM (MatchErrType (SexpTypeList b0))

elemM :: MatchM e s a -> SeqMatchM e s a
elemM = (`SeqMatchElem` SeqMatchPure)

restM :: MatchM e s a -> SeqMatchM e s (Seq a)
restM = (`SeqMatchRest` SeqMatchPure)

altM :: [(Text, MatchM e s a)] -> MatchM e s a
altM = go Empty
 where
  go !acc = \case
    [] -> errM (MatchErrAlt acc)
    (l, m) : ms -> do
      s <- ask
      case runMatchM m s of
        Right a -> pure a
        Left e -> go (acc :|> (l, e)) ms

lookM :: Brace -> [(Text, MatchM e s (), SeqMatchM e s a)] -> MatchM e s a
lookM b0 as0 = goRoot
 where
  goRoot = do
    s <- ask
    case B.memoVal s of
      SexpListF b cs0 | b == b0 ->
        case cs0 of
          Empty -> errM (MatchErrListElem 0)
          hd :<| _ -> goAlt hd Empty as0
      _ -> errM (MatchErrType (SexpTypeList b0))
  goAlt hd !acc = \case
    [] -> errM (MatchErrAlt acc)
    (l, m, r) : as ->
      case runMatchM m hd of
        Right _ -> do
          s <- ask
          case runMatchM (listFromM 1 b0 r) s of
            Right a -> pure a
            Left e -> goAlt hd (acc :|> (l, e)) as
        Left e -> goAlt hd (acc :|> (l, e)) as

anySymM :: MatchM e s Symbol
anySymM = matchM $ \case
  SexpAtomF (AtomSym y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeSym))

symM :: Symbol -> MatchM e s ()
symM x =
  anySymM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomSym x)))

anyIntM :: MatchM e s Integer
anyIntM = matchM $ \case
  SexpAtomF (AtomInt y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeInt))

intM :: Integer -> MatchM e s ()
intM x =
  anyIntM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomInt x)))

anySciM :: MatchM e s Scientific
anySciM = matchM $ \case
  SexpAtomF (AtomSci y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeSci))

sciM :: Scientific -> MatchM e s ()
sciM x =
  anySciM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomSci x)))

anyStrM :: MatchM e s Text
anyStrM = matchM $ \case
  SexpAtomF (AtomStr y) -> Right y
  _ -> Left (MatchErrType (SexpTypeAtom AtomTypeStr))

strM :: Text -> MatchM e s ()
strM x =
  anyStrM >>= \y ->
    unless (y == x) (errM (MatchErrNotEq (AtomStr x)))
