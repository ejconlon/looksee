{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A simple text parser with decent errors
module Looksee
  ( Span (..)
  , LineColLookup ()
  , calculateLineCol
  , lookupLineCol
  , Label (..)
  , textSpan
  , Reason (..)
  , ErrF (..)
  , Err (..)
  , errSpan
  , errReason
  , AltPhase (..)
  , InfixPhase (..)
  , ParserT
  , Parser
  , parseT
  , parse
  , parseI
  , spanP
  , spanAroundP
  , throwP
  , altP
  , emptyP
  , explainP
  , endP
  , optP
  , lookP
  , branchP
  , commitP
  , labelP
  , textP
  , textP_
  , charP
  , charP_
  , breakP
  , someBreakP
  , splitP
  , split1P
  , split2P
  , leadP
  , lead1P
  , trailP
  , trail1P
  , infixRP
  , someInfixRP
  , takeP
  , dropP
  , takeExactP
  , dropExactP
  , takeWhileP
  , dropWhileP
  , takeWhile1P
  , dropWhile1P
  , takeAllP
  , dropAllP
  , takeAll1P
  , dropAll1P
  , betweenP
  , repeatP
  , repeat1P
  , sepByP
  , sepBy1P
  , sepBy2P
  , spaceP
  , stripP
  , stripStartP
  , stripEndP
  , measureP
  , unconsP
  , headP
  , signedWithP
  , signedP
  , intP
  , uintP
  , decP
  , udecP
  , sciP
  , usciP
  , numP
  , unumP
  , space1P
  , strip1P
  , stripStart1P
  , stripEnd1P
  , transP
  , scopeP
  , iterP
  , strP
  , doubleStrP
  , singleStrP
  , HasErrMessage (..)
  , errataE
  , renderE
  , printE
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad (ap, void)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT, gets, state)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (MonadWriter (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Bitraversable (Bitraversable (..))
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Ratio ((%))
import Data.Scientific (Scientific)
import Data.Scientific qualified as S
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void, absurd)
import Errata qualified as E
import Errata.Styles qualified as E
import Errata.Types qualified as E
import System.IO (stderr)

-- | A generic span, used for tracking ranges of offsets or (line, col)
data Span a = Span {spanStart :: !a, spanEnd :: !a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Auxiliary data structure to translate offsets to (line, col)
type LineColLookup = Vector (Int, Int)

-- | Construct an offset lookup from a document
calculateLineCol :: Text -> LineColLookup
calculateLineCol t = V.unfoldrN (T.length t) go ((0, 0), T.unpack t)
 where
  go (p@(!line, !col), xs) =
    case xs of
      [] -> Nothing
      x : xs' -> Just (p, if x == '\n' then ((line + 1, 0), xs') else ((line, col + 1), xs'))

-- | Returns 0-based (line, col) for the given offset.
-- Clamps to the valid range of offsets, returning (0, 0) for
-- empty text. Note that the valid range is from before the first
-- character to before the last, so a 3 character string has
-- three valid offsets (0, 1, and 2).
lookupLineCol :: Int -> LineColLookup -> (Int, Int)
lookupLineCol i v =
  if V.null v
    then (0, 0)
    else v V.! max 0 (min i (V.length v - 1))

-- | A parser label (for error reporting)
newtype Label = Label {unLabel :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | Create a span from the given text
textSpan :: Text -> Span Int
textSpan t = Span 0 (T.length t)

-- private
-- Parser state
data St = St
  { stHay :: !Text
  , stSpan :: !(Span Int)
  , stLabels :: !(Seq Label)
  }
  deriving stock (Eq, Ord, Show)

-- private
-- Returns list of possible break points with positions
-- (startStream, breakPt) (breakPt, endStream) (breakPt + needLen, endStream)
breakAllRP :: Text -> St -> [(St, St, St)]
breakAllRP needle (St hay (Span r0 r1) labs) = fmap go (T.breakOnAll needle hay)
 where
  go (hayA, hayB) =
    let aLen = T.length hayA
        endA = r0 + aLen
        needLen = T.length needle
        rngA = Span r0 endA
        rngX = Span endA r1
        rngB = Span (endA + needLen) r1
        stA = St hayA rngA labs
        stX = St (T.drop aLen hay) rngX labs
        stB = St (T.drop needLen hayB) rngB labs
    in  (stA, stX, stB)

-- private
breakRP :: Text -> St -> Maybe (St, St, St)
breakRP needle (St hay (Span r0 r1) labs) =
  let (hayA, hayB) = T.breakOn needle hay
  in  if T.null hayB
        then Nothing
        else
          let aLen = T.length hayA
              endA = r0 + aLen
              needLen = T.length needle
              rngA = Span r0 endA
              rngX = Span endA r1
              rngB = Span (endA + needLen) r1
              stA = St hayA rngA labs
              stX = St (T.drop aLen hay) rngX labs
              stB = St (T.drop needLen hayB) rngB labs
          in  Just (stA, stX, stB)

-- | Phase of alternative parsing (for error reporting)
data AltPhase = AltPhaseBranch | AltPhaseCont
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Phase of infix/split parsing (for error reporting)
data InfixPhase = InfixPhaseLeft | InfixPhaseRight | InfixPhaseCont
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Whether to hide an underlying error or not
data HideError = HideErrorNo | HideErrorYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Reason for parse failure
data Reason e r
  = ReasonCustom !e
  | ReasonExpect !Text !Text
  | ReasonDemand !Int !Int
  | ReasonLeftover !Int
  | ReasonAlt !(Seq (AltPhase, r))
  | ReasonInfix !(Seq (Int, InfixPhase, r))
  | ReasonFail !Text
  | ReasonLabeled !Label r
  | ReasonLook r
  | ReasonTakeNone
  | ReasonEmpty
  | ReasonExplained !Text !HideError r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''Reason
deriveBifoldable ''Reason
deriveBitraversable ''Reason

-- | Base functor for 'Err' containing the range and reason for the error
data ErrF e r = ErrF
  { efSpan :: !(Span Int)
  , efReason :: !(Reason e r)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''ErrF
deriveBifoldable ''ErrF
deriveBitraversable ''ErrF

-- | A parse error, which may contain multiple sub-errors
newtype Err e = Err {unErr :: ErrF e (Err e)}
  deriving stock (Eq, Ord, Show)

instance Functor Err where
  fmap f = go
   where
    go (Err (ErrF ra re)) = Err (ErrF ra (bimap f go re))

instance Foldable Err where
  foldr f = flip go
   where
    go (Err (ErrF _ re)) z = bifoldr f go z re

instance Traversable Err where
  traverse f = go
   where
    go (Err (ErrF ra re)) = fmap (Err . ErrF ra) (bitraverse f go re)

instance (Typeable e, Show e) => Exception (Err e)

type instance Base (Err e) = ErrF e

instance Recursive (Err e) where
  project = unErr

instance Corecursive (Err e) where
  embed = Err

-- | Span of a parse error
errSpan :: Err e -> Span Int
errSpan = efSpan . unErr

-- | Reason for a parse error
errReason :: Err e -> Reason e (Err e)
errReason = efReason . unErr

-- private
newtype T e m a = T {unT :: ExceptT (Err e) (StateT St m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState St, MonadError (Err e))

instance MonadTrans (T e) where
  lift = T . lift . lift

instance MFunctor (T e) where
  hoist mn (T x) = T (hoist (hoist mn) x)

deriving instance (MonadReader r m) => MonadReader r (T e m)

deriving instance (MonadWriter w m) => MonadWriter w (T e m)

-- private
runT :: T e m a -> St -> m (Either (Err e) a, St)
runT = runStateT . runExceptT . unT

-- private
mkErrT :: (Monad m) => Reason e (Err e) -> T e m (Err e)
mkErrT re = gets (\st -> Err (ErrF (stSpan st) re))

-- private
-- errT :: Monad m => Reason e (Err e) -> T e m a
-- errT = mkErrT >=> throwError

-- private
tryT :: (Monad m) => T e m r -> T e m (Either (Err e) r)
tryT t = get >>= \st -> lift (runT t st) >>= \(er, st') -> er <$ put st'

-- | The parser monad transformer
newtype ParserT e m a = ParserT {unParserT :: forall r. (Either (Err e) a -> T e m r) -> T e m r}

instance Functor (ParserT e m) where
  fmap f (ParserT g) = ParserT (\j -> g (j . fmap f))

instance Applicative (ParserT e m) where
  pure a = ParserT (\j -> j (Right a))
  (<*>) = ap

instance Monad (ParserT e m) where
  return = pure
  ParserT g >>= f = ParserT (\j -> g (\case Left e -> j (Left e); Right a -> let ParserT h = f a in h j))

instance (Monad m) => Alternative (ParserT e m) where
  empty = emptyP
  p1 <|> p2 = altP [p1, p2]
  many = fmap toList . repeatP
  some = fmap toList . repeat1P

-- | The parser monad
type Parser e = ParserT e Identity

instance MonadTrans (ParserT e) where
  lift ma = ParserT (\j -> lift ma >>= j . Right)

instance (MonadIO m) => MonadIO (ParserT e m) where
  liftIO ma = ParserT (\j -> liftIO ma >>= j . Right)

instance (Monad m) => MonadFail (ParserT e m) where
  fail = errP . ReasonFail . T.pack

instance (MonadReader r m) => MonadReader r (ParserT e m) where
  ask = ParserT (\j -> ask >>= j . Right)
  local f (ParserT g) = ParserT (local f . g)

instance (MonadState s m) => MonadState s (ParserT e m) where
  get = ParserT (\j -> lift get >>= j . Right)
  put s = ParserT (\j -> lift (put s) >>= j . Right)
  state f = ParserT (\j -> lift (state f) >>= j . Right)

instance (Semigroup a) => Semigroup (ParserT e m a) where
  p <> q = liftA2 (<>) p q

instance (Monoid a) => Monoid (ParserT e m a) where
  mempty = pure mempty

-- private
finishParserT :: (Monad m) => ParserT e m a -> St -> m (Either (Err e) a, St)
finishParserT (ParserT g) st =
  let t = g (either throwError pure)
  in  runT t st

-- private
getP :: (Monad m) => ParserT e m St
getP = ParserT (\j -> get >>= j . Right)

-- private
getsP :: (Monad m) => (St -> a) -> ParserT e m a
getsP f = ParserT (\j -> gets f >>= j . Right)

-- private
putP :: (Monad m) => St -> ParserT e m ()
putP st = ParserT (\j -> put st >>= j . Right)

-- private
stateP :: (Monad m) => (St -> (a, St)) -> ParserT e m a
stateP f = ParserT (\j -> state f >>= j . Right)

-- private
errP :: (Monad m) => Reason e (Err e) -> ParserT e m a
errP re = ParserT (\j -> mkErrT re >>= j . Left)

-- private
leftoverP :: (Monad m) => ParserT e m Int
leftoverP = getsP (\st -> let Span s e = stSpan st in e - s)

-- | Run a parser transformer. You must consume all input or this will error!
-- If you really don't care about the rest of the input, you can always
-- discard it with 'dropAllP'.
parseT :: (Monad m) => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p h = fmap fst (finishParserT (p <* endP) (St h (textSpan h) Empty))

-- | Run a parser (see 'parseT')
parse :: Parser e a -> Text -> Either (Err e) a
parse p h = runIdentity (parseT p h)

-- | Run a parser and print any errors that occur
parseI :: (HasErrMessage e) => Parser e a -> Text -> IO (Either (Err e) a)
parseI p h = do
  let ea = parse p h
  case ea of
    Left e -> printE "<interactive>" h e
    Right _ -> pure ()
  pure ea

-- | Get the span (in character offset) at the current point representing
-- the entire parseable range. At the start of parsing this will be `Span 0 n` for
-- an `n`-character document. The start offset will increase as input is consumed,
-- and the end offset will decrease as lookahead delimits the range. To evaluate
-- the "real" range of characters consumed by a parser, construct a span with the
-- starting offsets before and after executing a subparser (or use 'spanAroundP').
spanP :: (Monad m) => ParserT e m (Span Int)
spanP = getsP stSpan

-- | Incorporate span information into a parsed object.
spanAroundP :: (Monad m) => (Span Int -> a -> b) -> ParserT e m a -> ParserT e m b
spanAroundP f p = do
  Span start _ <- spanP
  a <- p
  Span end _ <- spanP
  pure (f (Span start end) a)

-- | Throw a custom parse error
throwP :: (Monad m) => e -> ParserT e m a
throwP = errP . ReasonCustom

-- | Succeed if this is the end of input
endP :: (Monad m) => ParserT e m ()
endP = do
  l <- leftoverP
  if l == 0
    then pure ()
    else errP (ReasonLeftover l)

-- | Makes parse success optional
optP :: (Monad m) => ParserT e m a -> ParserT e m (Maybe a)
optP (ParserT g) = ParserT $ \j -> do
  st0 <- get
  g $ \case
    Left _ -> put st0 >> j (Right Nothing)
    Right a -> j (Right (Just a))

-- private
subAltP
  :: (Monad m)
  => (Either (Err e) a -> T e m r)
  -> St
  -> Seq (AltPhase, Err e)
  -> [ParserT e m a]
  -> T e m r
subAltP j st0 = go
 where
  go !errs = \case
    [] -> mkErrT (if Seq.null errs then ReasonEmpty else ReasonAlt errs) >>= j . Left
    ParserT g : rest -> g $ \case
      Left e -> put st0 >> go (errs :|> (AltPhaseBranch, e)) rest
      Right r -> do
        es <- tryT (j (Right r))
        case es of
          Left e -> put st0 >> go (errs :|> (AltPhaseCont, e)) rest
          Right s -> pure s

-- | Parse with many possible branches
altP :: (Monad m, Foldable f) => f (ParserT e m a) -> ParserT e m a
altP falts = ParserT (\j -> get >>= \st0 -> subAltP j st0 Empty (toList falts))

-- | Fail with no results
emptyP :: (Monad m) => ParserT e m a
emptyP = ParserT (\j -> mkErrT ReasonEmpty >>= j . Left)

-- | If things fail and you can give a good message explaining why, this combinator will
-- annotate the error with your explanation. Returning 'True' with message will hide
-- the original error message in textual rendering.
explainP :: (Monad m) => (Reason e (Err e) -> Maybe (Text, Bool)) -> ParserT e m a -> ParserT e m a
explainP f (ParserT g) = ParserT $ \j -> g $ \ea ->
  case ea of
    Left e@(Err (ErrF _ re)) ->
      case f re of
        Nothing -> j ea
        Just (msg, hide) -> do
          sp <- gets stSpan
          let hide' = if hide then HideErrorYes else HideErrorNo
          let e' = Err (ErrF sp (ReasonExplained msg hide' e))
          j (Left e')
    Right _ -> j ea

-- | Lookahead - rewinds state if the parser succeeds, otherwise throws error
lookP :: (Monad m) => ParserT e m a -> ParserT e m a
lookP (ParserT g) = ParserT $ \j -> do
  st0 <- get
  g (\ea -> put st0 >> j (first (Err . ErrF (stSpan st0) . ReasonLook) ea))

-- private
subBranchP
  :: (Monad m)
  => (Either (Err e) a -> T e m r)
  -> St
  -> Seq (AltPhase, Err e)
  -> [(ParserT e m (), ParserT e m a)]
  -> T e m r
subBranchP j st0 = go
 where
  go !errs = \case
    [] -> mkErrT (if Seq.null errs then ReasonEmpty else ReasonAlt errs) >>= j . Left
    (ParserT gl, ParserT gx) : rest -> gl $ \case
      Left _ -> put st0 >> go errs rest
      Right _ -> do
        put st0
        gx $ \case
          Left e -> put st0 >> go (errs :|> (AltPhaseBranch, e)) rest
          Right r -> do
            es <- tryT (j (Right r))
            case es of
              Left e -> put st0 >> go (errs :|> (AltPhaseCont, e)) rest
              Right s -> pure s

-- | Branches guarded by lookahead. Use this for more concise errors.
-- 'altP' will happily tell you about each of the errors it encountered in
-- every branch, but this will quietly prune non-matching branches.
-- Tries until first success (in order), so you can tack on a fallthrough case even if
-- you tried a branch earlier.
branchP :: (Monad m, Foldable f) => f (ParserT e m (), ParserT e m a) -> ParserT e m a
branchP falts = ParserT (\j -> get >>= \st0 -> subBranchP j st0 Empty (toList falts))

-- | An alternative to 'branchP' that does not backtrack after committing to a branch.
commitP :: (Monad m, Foldable f) => f (ParserT e m (), ParserT e m a) -> ParserT e m a
commitP = go . toList
 where
  go = \case
    [] -> emptyP
    (p, q) : rest -> do
      mx <- optP (lookP p)
      case mx of
        Nothing -> go rest
        Just _ -> q

-- | Labels parse errors
labelP :: (Monad m) => Label -> ParserT e m a -> ParserT e m a
labelP lab (ParserT g) = ParserT $ \j ->
  g $ \case
    Left e -> mkErrT (ReasonLabeled lab e) >>= j . Left
    Right a -> j (Right a)

-- | Expect the given text at the start of the range
textP :: (Monad m) => Text -> ParserT e m Text
textP n = do
  o <- takeP (T.length n)
  if n == o
    then pure n
    else errP (ReasonExpect n o)

-- | Saves you from importing 'void'
textP_ :: (Monad m) => Text -> ParserT e m ()
textP_ = void . textP

-- | Expect the given character at the start of the range
charP :: (Monad m) => Char -> ParserT e m Char
charP = fmap T.head . textP . T.singleton

-- | Saves you from importing 'void'
charP_ :: (Monad m) => Char -> ParserT e m ()
charP_ = void . charP

-- | Split once on the delimiter (first argument), parsing everything before it with a narrowed range.
-- Chooses first split from START to END of range (see 'infixRP').
breakP :: (Monad m) => Text -> ParserT e m a -> ParserT e m a
breakP tx pa = fmap fst (infixRP tx pa (pure ()))

-- | Split once on the delimiter (first argument), parsing everything before it with a narrowed range.
-- Chooses splits from START to END of range (see 'someInfixRP').
someBreakP :: (Monad m) => Text -> ParserT e m a -> ParserT e m a
someBreakP tx pa = fmap fst (someInfixRP tx pa (pure ()))

-- private
splitTailP :: (Monad m) => Text -> ParserT e m a -> Seq a -> St -> ParserT e m (Seq a)
splitTailP tx pa = go
 where
  go !acc !st = do
    mz <- optInfixRP tx pa (pure ())
    case mz of
      Nothing -> optP pa >>= maybe (acc <$ putP st) (pure . (acc :|>))
      Just (a, st', _) -> go (acc :|> a) st'

-- | Split on the delimiter, parsing segments with a narrowed range, until parsing fails.
-- Returns the sequence of successes with state at the delimiter preceding the failure (or end of input),
-- Note that this will always succeed, sometimes consuming no input and yielding empty results.
splitP :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
splitP tx pa = getP >>= splitTailP tx pa Empty

-- | Like 'splitP' but ensures the sequence is at least length 1.
split1P :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
split1P tx pa = do
  mz <- optInfixRP tx pa (pure ())
  case mz of
    Nothing -> fmap Seq.singleton pa
    Just (a, st', _) -> splitTailP tx pa (Seq.singleton a) st'

-- | Like 'splitP' but ensures the sequence is at least length 2.
-- (This ensures there is at least one delimiter included.)
split2P :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
split2P tx pa = do
  a0 <- someBreakP tx pa
  mz <- optInfixRP tx pa (pure ())
  case mz of
    Nothing -> fmap (Empty :|> a0 :|>) pa
    Just (a1, st', _) -> splitTailP tx pa (Empty :|> a0 :|> a1) st'

-- | Like 'splitP' but ensures a leading delimiter
leadP :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
leadP tx pa = do
  mu <- optP (textP tx)
  case mu of
    Nothing -> pure Empty
    Just _ -> split1P tx pa

-- | Like 'split1P' but ensures a leading delimiter
lead1P :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
lead1P tx pa = textP tx >> split1P tx pa

-- | Like 'splitP' but ensures a trailing delimiter
trailP :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
trailP tx pa = do
  as <- splitP tx pa
  case as of
    Empty -> pure Empty
    _ -> as <$ textP tx

-- | Like 'split1P' but ensures a trailing delimiter
trail1P :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
trail1P tx pa = split1P tx pa <* textP tx

-- private
subInfixP
  :: (Monad m)
  => St
  -> ParserT e m a
  -> ParserT e m b
  -> (Either (Err e) (Maybe (a, St, b)) -> T e m r)
  -> [(St, St, St)]
  -> T e m r
subInfixP st0 pa pb j = go Empty
 where
  go !errs = \case
    [] -> do
      put st0
      case errs of
        Empty -> j (Right Nothing)
        _ -> mkErrT (ReasonInfix errs) >>= j . Left
    (stA, stX, stB) : sts -> do
      let startX = spanStart (stSpan stX)
      put stA
      unParserT (pa <* endP) $ \case
        Left errA -> go (errs :|> (startX, InfixPhaseLeft, errA)) sts
        Right a -> do
          put stB
          unParserT pb $ \case
            Left errB -> go (errs :|> (startX, InfixPhaseRight, errB)) sts
            Right b -> do
              ec <- tryT (j (Right (Just (a, stX, b))))
              case ec of
                Left errC -> go (errs :|> (startX, InfixPhaseCont, errC)) sts
                Right c -> pure c

-- private
optInfixRP :: (Monad m) => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (Maybe (a, St, b))
optInfixRP tx pa pb = ParserT (\j -> get >>= \st0 -> subInfixP st0 pa pb (optInfixFn j) (breakAllRP tx st0))

-- private
optInfixFn
  :: (Either (Err e) (Maybe (a, St, b)) -> T e m r)
  -> (Either (Err e) (Maybe (a, St, b)) -> T e m r)
optInfixFn j e = case e of
  Right _ -> j e
  Left _ -> j (Right Nothing)

-- private
requireInfixFn
  :: (Monad m)
  => (Either (Err e) (a, b) -> T e m r)
  -> (Either (Err e) (Maybe (a, St, b)) -> T e m r)
requireInfixFn j = \case
  Right mxab ->
    case mxab of
      Nothing -> mkErrT ReasonEmpty >>= j . Left
      Just (a, _, b) -> j (Right (a, b))
  Left e -> j (Left e)

-- | Right-associative infix parsing. Searches for the operator from START to END of range,
-- trying only the first break point.
infixRP :: (Monad m) => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (a, b)
infixRP tx pa pb = ParserT (\j -> get >>= \st0 -> subInfixP st0 pa pb (requireInfixFn j) (maybeToList (breakRP tx st0)))

-- | Right-associative infix parsing. Searches for the operator from START to END of range,
-- trying subsequent break points until success.
someInfixRP :: (Monad m) => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (a, b)
someInfixRP tx pa pb = ParserT (\j -> get >>= \st0 -> subInfixP st0 pa pb (requireInfixFn j) (breakAllRP tx st0))

-- | Take the given number of characters from the start of the range, or fewer if empty
takeP :: (Monad m) => Int -> ParserT e m Text
takeP i = stateP $ \st ->
  let h = stHay st
      (o, h') = T.splitAt i h
      l = T.length o
      r = stSpan st
      r' = r {spanStart = spanStart r + l}
      st' = st {stHay = h', stSpan = r'}
  in  (o, st')

-- | Take exactly the given number of characters from the start of the range, or error
takeExactP :: (Monad m) => Int -> ParserT e m Text
takeExactP i = do
  et <- stateP $ \st ->
    let h = stHay st
        (o, h') = T.splitAt i h
        l = T.length o
        r = stSpan st
        r' = r {spanStart = spanStart r + T.length o}
        st' = st {stHay = h', stSpan = r'}
    in  if l == i then (Right o, st') else (Left l, st)
  case et of
    Left l -> errP (ReasonDemand i l)
    Right a -> pure a

-- | Drop the given number of characters from the start of the range, or fewer if empty
dropP :: (Monad m) => Int -> ParserT e m Int
dropP = fmap T.length . takeP

-- | Drop exactly the given number of characters from the start of the range, or error
dropExactP :: (Monad m) => Int -> ParserT e m ()
dropExactP = void . takeExactP

-- | Take characters from the start of the range satisfying the predicate
takeWhileP :: (Monad m) => (Char -> Bool) -> ParserT e m Text
takeWhileP f = stateP $ \st ->
  let h = stHay st
      o = T.takeWhile f h
      l = T.length o
      h' = T.drop l h
      r = stSpan st
      r' = r {spanStart = spanStart r + l}
      st' = st {stHay = h', stSpan = r'}
  in  (o, st')

-- | Like 'takeWhileP' but ensures at least 1 character has been taken
takeWhile1P :: (Monad m) => (Char -> Bool) -> ParserT e m Text
takeWhile1P f = do
  mt <- stateP $ \st ->
    let h = stHay st
        o = T.takeWhile f h
        l = T.length o
        h' = T.drop l h
        r = stSpan st
        r' = r {spanStart = spanStart r + l}
        st' = st {stHay = h', stSpan = r'}
    in  if l == 0 then (Nothing, st) else (Just o, st')
  case mt of
    Nothing -> errP ReasonTakeNone
    Just a -> pure a

-- | Drop characters from the start of the range satisfying the predicate
dropWhileP :: (Monad m) => (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

-- | Like 'dropWhileP' but ensures at least 1 character has been dropped
dropWhile1P :: (Monad m) => (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

-- | Take the remaining range, leaving it empty
takeAllP :: (Monad m) => ParserT e m Text
takeAllP = stateP $ \st ->
  let h = stHay st
      r = stSpan st
      r' = r {spanStart = spanEnd r}
      st' = st {stHay = T.empty, stSpan = r'}
  in  (h, st')

-- | Like 'takeAllP' but ensures at least 1 character has been taken
takeAll1P :: (Monad m) => ParserT e m Text
takeAll1P = do
  mt <- stateP $ \st ->
    let h = stHay st
        r = stSpan st
        r' = r {spanStart = spanEnd r}
        st' = st {stHay = T.empty, stSpan = r'}
    in  if T.null h then (Nothing, st) else (Just h, st')
  case mt of
    Nothing -> errP (ReasonDemand 1 0)
    Just a -> pure a

-- | Drop the remaining range, leaving it empty
dropAllP :: (Monad m) => ParserT e m Int
dropAllP = fmap T.length takeAllP

-- | Like 'dropAllP' but ensures at least 1 character has been dropped
dropAll1P :: (Monad m) => ParserT e m Int
dropAll1P = fmap T.length takeAll1P

-- | Unwrap a monad transformer layer (see 'scopeP' for use)
transP :: (MonadTrans t, Monad m) => (forall a. t m a -> m a) -> ParserT e (t m) b -> ParserT e m b
transP f (ParserT g) = ParserT $ \j -> do
  st0 <- get
  (ea, st1) <- lift (f (runT (g (hoist lift . j)) st0))
  put st1
  either throwError pure ea

-- | Parse with some local state
scopeP :: (Monad m) => s -> ParserT e (StateT s m) a -> ParserT e m a
scopeP s0 = transP (`evalStateT` s0)

-- | Repeats the parser until it returns a 'Just' value
iterP :: ParserT e m (Maybe a) -> ParserT e m a
iterP p = go
 where
  go = p >>= maybe go pure

data StrState = StrState !Bool !(Seq Char)

-- | Parse a string with a custom quote character. Supports backslash-escaping.
strP :: (Monad m) => Char -> ParserT e m Text
strP d = do
  textP_ (T.singleton d)
  scopeP (StrState False Empty) $ iterP $ do
    c <- headP
    state $ \ss@(StrState esc buf) ->
      if c == d
        then
          if esc
            then (Nothing, StrState False (buf :|> c))
            else (Just (T.pack (toList buf)), ss)
        else
          if c == '\\'
            then
              if esc
                then (Nothing, StrState False (buf :|> c))
                else (Nothing, StrState True buf)
            else (Nothing, StrState False (buf :|> c))

-- | Parse a double-quoted string
doubleStrP :: (Monad m) => ParserT e m Text
doubleStrP = strP '"'

-- | Parse a single-quoted string
singleStrP :: (Monad m) => ParserT e m Text
singleStrP = strP '\''

-- | Parse between an opening delimiter (first parser) and a closing delimited (second parser)
betweenP :: ParserT e m x -> ParserT e m y -> ParserT e m a -> ParserT e m a
betweenP px py pa = px *> pa <* py

repeatTailP :: (Monad m) => ParserT e m a -> Seq a -> ParserT e m (Seq a)
repeatTailP p = go
 where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

-- | Repeat a parser until it fails, collecting the results.
repeatP :: (Monad m) => ParserT e m a -> ParserT e m (Seq a)
repeatP p = repeatTailP p Empty

-- | Like 'repeatP' but ensures at least one result.
repeat1P :: (Monad m) => ParserT e m a -> ParserT e m (Seq a)
repeat1P p = p >>= repeatTailP p . Seq.singleton

-- private
sepByTailP :: (Monad m) => ParserT e m () -> ParserT e m a -> Seq a -> ParserT e m (Seq a)
sepByTailP pu pa = go
 where
  go !acc = do
    ma <- optP (pu >> pa)
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

-- | Parse a sequence of items delimited by the first parser
sepByP :: (Monad m) => ParserT e m () -> ParserT e m a -> ParserT e m (Seq a)
sepByP pu pa = optP pa >>= maybe (pure Empty) (sepByTailP pu pa . Seq.singleton)

-- | Like 'sepByP' but ensures at least one result.
sepBy1P :: (Monad m) => ParserT e m () -> ParserT e m a -> ParserT e m (Seq a)
sepBy1P pu pa = pa >>= sepByTailP pu pa . Seq.singleton

-- | Like 'sepByP' but ensures at least two results (and at least one delimiter).
sepBy2P :: (Monad m) => ParserT e m () -> ParserT e m a -> ParserT e m (Seq a)
sepBy2P pu pa = do
  a0 <- pa
  pu
  a1 <- pa
  sepByTailP pu pa (Empty :|> a0 :|> a1)

-- | Consumes many spaces at the start of the range
spaceP :: (Monad m) => ParserT e m ()
spaceP = void (dropWhileP isSpace)

-- | Strips spaces before and after parsing
stripP :: (Monad m) => ParserT e m a -> ParserT e m a
stripP p = spaceP *> p <* spaceP

-- | Strips spaces before parsing
stripStartP :: (Monad m) => ParserT e m a -> ParserT e m a
stripStartP p = spaceP *> p

-- | Strips spaces after parsing
stripEndP :: (Monad m) => ParserT e m a -> ParserT e m a
stripEndP p = p <* spaceP

-- | Parses and returns the length of the consumed input along with the result
measureP :: (Monad m) => ParserT e m a -> ParserT e m (a, Int)
measureP p = do
  start <- getsP (spanStart . stSpan)
  a <- p
  end <- getsP (spanStart . stSpan)
  pure (a, end - start)

-- | Takes exactly 1 character from the start of the range, returning Nothing
-- if at end of input
unconsP :: (Monad m) => ParserT e m (Maybe Char)
unconsP = stateP $ \st ->
  let h = stHay st
      mxy = T.uncons h
  in  case mxy of
        Nothing -> (Nothing, st)
        Just (x, y) ->
          let r = stSpan st
              r' = r {spanStart = spanStart r + 1}
              st' = st {stHay = y, stSpan = r'}
          in  (Just x, st')

-- | Takes exactly 1 character from the start of the range, throwing error
-- if at end of input
headP :: (Monad m) => ParserT e m Char
headP = unconsP >>= maybe (errP (ReasonDemand 1 0)) pure

-- | Add signed-ness to any parser with a negate function
signedWithP :: (Monad m) => (a -> a) -> ParserT e m a -> ParserT e m a
signedWithP neg p = do
  ms <- optP (charP '-')
  case ms of
    Nothing -> p
    Just _ -> fmap neg p

-- | Add signed-ness to any numeric parser
signedP :: (Monad m, Num a) => ParserT e m a -> ParserT e m a
signedP = signedWithP negate

-- | Parse an signed integer
intP :: (Monad m) => ParserT e m Integer
intP = signedP uintP

-- | Parse an unsigned integer
uintP :: (Monad m) => ParserT e m Integer
uintP = T.foldl' (\n d -> n * 10 + fromIntegral (digitToInt d)) 0 <$> takeWhile1P isDigit

-- | Parse a signed decimal
decP :: (Monad m) => ParserT e m Rational
decP = signedP udecP

-- | Parse an unsigned decimal
udecP :: (Monad m) => ParserT e m Rational
udecP = do
  whole <- fmap fromInteger uintP
  hasDot <- fmap isJust (optP (charP '.'))
  if hasDot
    then do
      (numerator, places) <- measureP uintP
      let denominator = 10 ^ places
          part = numerator % denominator
      pure (whole + part)
    else pure whole

-- | Parse a signed scientific number
sciP :: (Monad m) => ParserT e m Scientific
sciP = signedP usciP

-- | Parse an unsigned scientific  number
usciP :: (Monad m) => ParserT e m Scientific
usciP = do
  whole <- uintP
  hasDot <- fmap isJust (optP (charP_ '.'))
  (frac, places) <- if hasDot then measureP uintP else pure (0, 0)
  hasEx <- fmap isJust (optP (charP_ 'e' <|> charP_ 'E'))
  ex <- if hasEx then fmap fromIntegral intP else pure 0
  let wholeS = S.scientific whole ex
      partS = S.scientific frac (ex - places)
  pure (wholeS + partS)

-- | Parse a signed integer/scientific number, defaulting to integer if possible.
numP :: (Monad m) => ParserT e m (Either Integer Scientific)
numP = signedWithP (bimap negate negate) unumP

-- | Parse an unsigned integer/scientific number, defaulting to integer if possible.
unumP :: (Monad m) => ParserT e m (Either Integer Scientific)
unumP = do
  whole <- uintP
  hasDot <- fmap isJust (optP (charP_ '.'))
  mayFracPlaces <- if hasDot then fmap Just (measureP uintP) else pure Nothing
  hasEx <- fmap isJust (optP (charP_ 'e' <|> charP_ 'E'))
  mayEx <- if hasEx then fmap (Just . fromIntegral) intP else pure Nothing
  case (mayFracPlaces, mayEx) of
    (Nothing, Nothing) -> pure (Left whole)
    _ -> do
      let (frac, places) = fromMaybe (0, 0) mayFracPlaces
          ex = fromMaybe 0 mayEx
          wholeS = S.scientific whole ex
          partS = S.scientific frac (ex - places)
      pure (Right (wholeS + partS))

-- | Like 'spaceP' but ensures at least 1 space removed
space1P :: (Monad m) => ParserT e m ()
space1P = void (dropWhile1P isSpace)

-- | Like 'stripP' but ensures at least 1 space removed
strip1P :: (Monad m) => ParserT e m a -> ParserT e m a
strip1P p = space1P *> p <* space1P

-- | Like 'stripStartP' but ensures at least 1 space removed
stripStart1P :: (Monad m) => ParserT e m a -> ParserT e m a
stripStart1P p = space1P *> p

-- | Like 'stripEndP' but ensures at least 1 space removed
stripEnd1P :: (Monad m) => ParserT e m a -> ParserT e m a
stripEnd1P p = p <* space1P

-- | Implement this to format custom errors. The list will be indented and joined with `unlines`.
class HasErrMessage e where
  getErrMessage :: (Int -> Text) -> e -> [Text]

instance HasErrMessage Void where
  getErrMessage = const absurd

-- private
indent :: Int -> [Text] -> [Text]
indent i = let s = T.replicate (2 * i) " " in fmap (s <>)

instance (HasErrMessage e) => HasErrMessage (Err e) where
  getErrMessage repPos = go
   where
    go (Err (ErrF (Span start end) re)) =
      let pos = "Error in range " <> repPos start <> "-" <> repPos end <> ":"
          body = case re of
            ReasonCustom e ->
              let hd = "Custom error:"
                  tl = indent 1 (getErrMessage repPos e)
              in  hd : tl
            ReasonExpect expected actual ->
              ["Expected text: '" <> expected <> "' but found: '" <> actual <> "'"]
            ReasonDemand expected actual ->
              ["Expected count: " <> T.pack (show expected) <> " but got: " <> T.pack (show actual)]
            ReasonLeftover count ->
              ["Expected end but had leftover count: " <> T.pack (show count)]
            ReasonAlt errs ->
              let hd = "Alternatives:"
                  tl = indent 1 $ do
                    (_, e) <- toList errs
                    "Tried:" : indent 1 (go e)
              in  hd : tl
            ReasonInfix errs ->
              let hd = "Infix/split failed:"
                  tl = indent 1 $ do
                    (i, _, e) <- toList errs
                    let x = "Tried position: " <> T.pack (show i)
                    x : indent 1 (go e)
              in  hd : tl
            ReasonFail msg -> ["User reported failure: " <> msg]
            ReasonLabeled lab e ->
              let hd = "Label: " <> unLabel lab
                  tl = indent 1 (go e)
              in  hd : tl
            ReasonLook e ->
              let hd = "Error in lookahead:"
                  tl = indent 1 (go e)
              in  hd : tl
            ReasonTakeNone -> ["Took/dropped no elements"]
            ReasonEmpty -> ["No parse results"]
            ReasonExplained msg hide e ->
              case hide of
                HideErrorNo ->
                  let tl = indent 1 (go e)
                  in  msg : tl
                HideErrorYes -> [msg]
      in  pos : body

-- | Create 'Errata' formatting a parse error
errataE :: (HasErrMessage e) => FilePath -> (Int -> (E.Line, E.Column)) -> Err e -> [E.Errata]
errataE fp mkP e =
  let (line, col) = mkP (spanStart (errSpan e))
      repP i = let (l, c) = mkP i in "(" <> T.pack (show l) <> ", " <> T.pack (show c) <> ")"
      msg = getErrMessage repP e
      block = E.blockSimple E.basicStyle E.basicPointer fp Nothing (line, col, col + 1, Nothing) (Just (T.unlines msg))
  in  [E.Errata Nothing [block] Nothing]

-- | Render a formatted error to text
renderE :: (HasErrMessage e) => FilePath -> Text -> Err e -> Text
renderE fp h e =
  let v = calculateLineCol h
      mkP i = let (l, c) = lookupLineCol i v in (l + 1, c + 1)
  in  TL.toStrict (E.prettyErrors h (errataE fp mkP e))

-- | Print a formatted error to stderr
printE :: (HasErrMessage e) => FilePath -> Text -> Err e -> IO ()
printE fp h e = TIO.hPutStrLn stderr (renderE fp h e)
