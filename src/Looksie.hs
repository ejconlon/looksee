{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A simple text parser with decent errors
module Looksie
  ( Label (..)
  , Range (..)
  , range
  , Reason (..)
  , ErrF (..)
  , Err (..)
  , errRange
  , errReason
  , AltPhase (..)
  , InfixPhase (..)
  , ParserT
  , Parser
  , parseT
  , parse
  , parseI
  , throwP
  , altP
  , emptyP
  , endP
  , optP
  , greedyP
  , greedy1P
  , lookP
  , labelP
  , textP
  , textP_
  , charP
  , charP_
  , splitNearP
  , splitAllP
  , splitAll1P
  , splitAll2P
  , leadP
  , lead1P
  , trailP
  , trail1P
  , infixRP
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
  , sepByP
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
  , repeatP
  , repeat1P
  , space1P
  , strip1P
  , stripStart1P
  , stripEnd1P
  , sepBy1P
  , sepBy2P
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

import Control.Applicative (Alternative (..), liftA2)
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
import Data.Maybe (fromMaybe, isJust)
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

-- private
type OffsetVec = Vector (Int, Int)

-- private
mkOffsetVec :: Text -> OffsetVec
mkOffsetVec t = V.unfoldrN (T.length t) go ((0, 0), T.unpack t)
 where
  go (p@(!line, !col), xs) =
    case xs of
      [] -> Nothing
      x : xs' -> Just (p, if x == '\n' then ((line + 1, 0), xs') else ((line, col + 1), xs'))

-- | A parser label (for error reporting)
newtype Label = Label {unLabel :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | Range in text character offset
data Range = Range {rangeStart :: !Int, rangeEnd :: !Int}
  deriving stock (Eq, Ord, Show)

-- | Create a range from the given text
range :: Text -> Range
range t = Range 0 (T.length t)

-- private
-- Parser state
data St = St
  { stHay :: !Text
  , stRange :: !Range
  , stLabels :: !(Seq Label)
  }
  deriving stock (Eq, Ord, Show)

breakRP :: Text -> St -> [(St, Int, St)]
breakRP needle (St hay (Range r0 r1) labs) = fmap go (T.breakOnAll needle hay)
 where
  go (hay1, hay2) =
    let end1 = r0 + T.length hay1
        needLen = T.length needle
        rng1 = Range r0 end1
        rng2 = Range (end1 + needLen) r1
        st1 = St hay1 rng1 labs
        st2 = St (T.drop needLen hay2) rng2 labs
    in  (st1, end1, st2)

-- TODO resurrect this with more efficient breaking
-- breakLP :: Text -> St -> [(St, Int, St)]
-- breakLP needle = undefined

-- | Phase of alternative parsing (for error reporting)
data AltPhase = AltPhaseBranch | AltPhaseCont
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Phase of infix/split parsing (for error reporting)
data InfixPhase = InfixPhaseLeft | InfixPhaseRight | InfixPhaseCont
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
  | ReasonLabelled !Label r
  | ReasonLook r
  | ReasonTakeNone
  | ReasonEmpty
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''Reason
deriveBifoldable ''Reason
deriveBitraversable ''Reason

-- | Base functor for 'Err' containing the range and reason for the error
data ErrF e r = ErrF
  { efRange :: !Range
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

-- | Range of a parse error
errRange :: Err e -> Range
errRange = efRange . unErr

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

deriving instance MonadReader r m => MonadReader r (T e m)

deriving instance MonadWriter w m => MonadWriter w (T e m)

-- private
runT :: T e m a -> St -> m (Either (Err e) a, St)
runT = runStateT . runExceptT . unT

-- private
mkErrT :: Monad m => Reason e (Err e) -> T e m (Err e)
mkErrT re = gets (\st -> Err (ErrF (stRange st) re))

-- private
-- errT :: Monad m => Reason e (Err e) -> T e m a
-- errT = mkErrT >=> throwError

-- private
tryT :: Monad m => T e m r -> T e m (Either (Err e) r)
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

instance Monad m => Alternative (ParserT e m) where
  empty = emptyP
  p1 <|> p2 = altP [p1, p2]
  many = fmap toList . greedyP
  some = fmap toList . greedy1P

-- | The parser monad
type Parser e = ParserT e Identity

instance MonadTrans (ParserT e) where
  lift ma = ParserT (\j -> lift ma >>= j . Right)

instance MonadIO m => MonadIO (ParserT e m) where
  liftIO ma = ParserT (\j -> liftIO ma >>= j . Right)

instance Monad m => MonadFail (ParserT e m) where
  fail = errP . ReasonFail . T.pack

instance MonadReader r m => MonadReader r (ParserT e m) where
  ask = ParserT (\j -> ask >>= j . Right)
  local f (ParserT g) = ParserT (local f . g)

-- instance MonadWriter w m => MonadWriter w (ParserT e m) where
--   writer aw = ParserT (\j -> writer aw >>= j . Right)
--   tell w = ParserT (\j -> tell w >>= j . Right)
--   listen p = ParserT (\j -> listen (runParserT p) >>= j . Right)
--   pass p = ParserT (\j -> pass (runParserT p) >>= j . Right)

instance MonadState s m => MonadState s (ParserT e m) where
  get = ParserT (\j -> lift get >>= j . Right)
  put s = ParserT (\j -> lift (put s) >>= j . Right)
  state f = ParserT (\j -> lift (state f) >>= j . Right)

-- private
finishParserT :: Monad m => ParserT e m a -> St -> m (Either (Err e) a, St)
finishParserT (ParserT g) st =
  let t = g (either throwError pure)
  in  runT t st

-- private
getP :: Monad m => ParserT e m St
getP = ParserT (\j -> get >>= j . Right)

-- private
getsP :: Monad m => (St -> a) -> ParserT e m a
getsP f = ParserT (\j -> gets f >>= j . Right)

-- private
putP :: Monad m => St -> ParserT e m ()
putP st = ParserT (\j -> put st >>= j . Right)

-- private
stateP :: Monad m => (St -> (a, St)) -> ParserT e m a
stateP f = ParserT (\j -> state f >>= j . Right)

-- private
errP :: Monad m => Reason e (Err e) -> ParserT e m a
errP re = ParserT (\j -> mkErrT re >>= j . Left)

-- private
leftoverP :: Monad m => ParserT e m Int
leftoverP = getsP (\st -> let Range s e = stRange st in e - s)

-- | Run a parser transformer. You must consume all input or this will error!
-- If you really don't care about the rest of the input, you can always
-- discard it with 'dropAllP'.
parseT :: Monad m => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p h = fmap fst (finishParserT (p <* endP) (St h (range h) Empty))

-- | Run a parser (see 'parseT')
parse :: Parser e a -> Text -> Either (Err e) a
parse p h = runIdentity (parseT p h)

-- | Run a parser and print any errors that occur
parseI :: HasErrMessage e => Parser e a -> Text -> IO (Either (Err e) a)
parseI p h = do
  let ea = parse p h
  case ea of
    Left e -> printE "<interactive>" h e
    Right _ -> pure ()
  pure ea

-- | Throw a custom parse error
throwP :: Monad m => e -> ParserT e m a
throwP = errP . ReasonCustom

-- | Succeed if this is the end of input
endP :: Monad m => ParserT e m ()
endP = do
  l <- leftoverP
  if l == 0
    then pure ()
    else errP (ReasonLeftover l)

-- | Makes parse success optional
optP :: Monad m => ParserT e m a -> ParserT e m (Maybe a)
optP (ParserT g) = ParserT $ \j -> do
  st0 <- get
  g $ \case
    Left _ -> put st0 *> j (Right Nothing)
    Right a -> j (Right (Just a))

-- private
subAltP
  :: Monad m
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
      Left e -> do
        -- traceM ("Alt unwinding branch (Left: " ++ show (length rest) ++ ")")
        -- traceShowM e
        put st0 *> go (errs :|> (AltPhaseBranch, e)) rest
      Right r -> do
        es <- tryT (j (Right r))
        case es of
          Left e -> do
            -- traceM ("Alt unwinding cont (Left: " ++ show (length rest) ++ ")")
            -- traceShowM e
            put st0 *> go (errs :|> (AltPhaseCont, e)) rest
          Right s -> do
            -- traceM "Alt success"
            -- traceM ("Skipped: " ++ show (length rest))
            pure s

-- | Parse with many possible branches
altP :: (Monad m, Foldable f) => f (ParserT e m a) -> ParserT e m a
altP falts = ParserT (\j -> get >>= \st0 -> subAltP j st0 Empty (toList falts))

-- | Fail with no results
emptyP :: Monad m => ParserT e m a
emptyP = ParserT (\j -> mkErrT ReasonEmpty >>= j . Left)

-- | Parse repeatedly until the parser fails
greedyP :: Monad m => ParserT e m a -> ParserT e m (Seq a)
greedyP p = go Empty
 where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

-- | Same as 'greedyP' but ensure at least one result
greedy1P :: Monad m => ParserT e m a -> ParserT e m (Seq a)
greedy1P p = liftA2 (:<|) p (greedyP p)

-- | Lookahead - rewinds state if the parser succeeds, otherwise throws error
lookP :: Monad m => ParserT e m a -> ParserT e m a
lookP (ParserT g) = ParserT $ \j -> do
  st0 <- get
  g (\ea -> put st0 *> j (first (Err . ErrF (stRange st0) . ReasonLook) ea))

-- | Labels parse errors
labelP :: Monad m => Label -> ParserT e m a -> ParserT e m a
labelP lab (ParserT g) = ParserT $ \j ->
  g $ \case
    Left e -> mkErrT (ReasonLabelled lab e) >>= j . Left
    Right a -> j (Right a)

-- | Expect the given text at the start of the range
textP :: Monad m => Text -> ParserT e m Text
textP n = do
  o <- takeP (T.length n)
  if n == o
    then pure n
    else errP (ReasonExpect n o)

-- | Saves you from importing 'void'
textP_ :: Monad m => Text -> ParserT e m ()
textP_ = void . textP

-- | Expect the given character at the start of the range
charP :: Monad m => Char -> ParserT e m Char
charP = fmap T.head . textP . T.singleton

-- | Saves you from importing 'void'
charP_ :: Monad m => Char -> ParserT e m ()
charP_ = void . charP

-- | Split once on the delimiter (first argument), parsing everything before it with a narrowed range.
-- Chooses splits from START to END of range (see 'infixRP').
splitNearP :: Monad m => Text -> ParserT e m a -> ParserT e m a
splitNearP tx pa = fmap fst (infixRP tx pa (pure ()))

-- TODO resurrect this with more efficient breaking
-- -- | Split once on the delimiter (first argument), parsing everything before it with a narrowed range.
-- -- Chooses splits from END to START of range (see 'infixRP').
-- splitFarP :: Monad m => Text -> ParserT e m a -> ParserT e m a
-- splitFarP tx pa = fmap fst (infixLP tx pa (pure ()))

-- | Split on every delimiter, parsing all segments with a narrowed range
splitAllP :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
splitAllP tx pa = go
 where
  go = do
    maas <- optInfixRP tx pa go
    case maas of
      Nothing -> fmap (maybe Empty Seq.singleton) (optP pa)
      Just (a, as) -> pure (a :<| as)

-- | Like 'splitAllP' but ensures the sequence is at least length 1
splitAll1P :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
splitAll1P tx pa = fmap (uncurry (:<|)) (infixRP tx pa (splitAllP tx pa))

-- | Like 'splitAllP' but ensures the sequence is at least length 2 (i.e. there was a delimiter)
splitAll2P :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
splitAll2P tx pa = fmap (uncurry (:<|)) (infixRP tx pa (splitAll1P tx pa))

-- | Like 'splitAllP' but ensures a leading delimiter
leadP :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
leadP tx pa = do
  mu <- optP (textP tx)
  case mu of
    Nothing -> pure Empty
    Just _ -> splitAll1P tx pa

-- | Like 'splitAll1P' but ensures a leading delimiter
lead1P :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
lead1P tx pa = textP tx *> splitAll1P tx pa

-- | Like 'splitAllP' but ensures a trailing delimiter
trailP :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
trailP tx pa = do
  as <- splitAllP tx pa
  case as of
    Empty -> pure Empty
    _ -> as <$ textP tx

-- | Like 'splitAll1P' but ensures a trailing delimiter
trail1P :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
trail1P tx pa = splitAll1P tx pa <* textP tx

-- private
subInfixP
  :: Monad m
  => St
  -> ParserT e m a
  -> ParserT e m b
  -> (Either (Err e) (Maybe (a, b)) -> T e m r)
  -> [(St, Int, St)]
  -> T e m r
subInfixP st0 pa pb j = go Empty
 where
  go !errs = \case
    [] -> do
      put st0
      case errs of
        Empty -> j (Right Nothing)
        _ -> mkErrT (ReasonInfix errs) >>= j . Left
    (stA, endA, stB) : sts -> do
      put stA
      unParserT (pa <* endP) $ \case
        Left errA -> go (errs :|> (endA, InfixPhaseLeft, errA)) sts
        Right a -> do
          put stB
          unParserT pb $ \case
            Left errB -> go (errs :|> (endA, InfixPhaseRight, errB)) sts
            Right b -> do
              ec <- tryT (j (Right (Just (a, b))))
              case ec of
                Left errC -> go (errs :|> (endA, InfixPhaseCont, errC)) sts
                Right c -> pure c

-- private
optInfixRP :: Monad m => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (Maybe (a, b))
optInfixRP tx pa pb = ParserT (\j -> get >>= \st0 -> subInfixP st0 pa pb j (breakRP tx st0))

-- private
requireInfix
  :: Monad m
  => (Either (Err e) (a, b) -> T e m r)
  -> (Either (Err e) (Maybe (a, b)) -> T e m r)
requireInfix j = \case
  Right mxab ->
    case mxab of
      Nothing -> mkErrT ReasonEmpty >>= j . Left
      Just xab -> j (Right xab)
  Left e -> j (Left e)

-- | Right-associative infix parsing. Searches for the operator from START to END of range.
infixRP :: Monad m => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (a, b)
infixRP tx pa pb = ParserT (\j -> get >>= \st0 -> subInfixP st0 pa pb (requireInfix j) (breakRP tx st0))

-- TODO resurrect this with more efficient breaking
-- -- | Left-associative infix parsing. Searches for the operator from END to START of range.
-- infixLP :: Monad m => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (a, b)
-- infixLP tx pa pb = ParserT (\j -> get >>= \st0 -> subInfixP st0 pa pb (requireInfix j) (breakLP tx st0))

-- | Take the given number of characters from the start of the range, or fewer if empty
takeP :: Monad m => Int -> ParserT e m Text
takeP i = stateP $ \st ->
  let h = stHay st
      (o, h') = T.splitAt i h
      l = T.length o
      r = stRange st
      r' = r {rangeStart = rangeStart r + l}
      st' = st {stHay = h', stRange = r'}
  in  (o, st')

-- | Take exactly the given number of characters from the start of the range, or error
takeExactP :: Monad m => Int -> ParserT e m Text
takeExactP i = do
  et <- stateP $ \st ->
    let h = stHay st
        (o, h') = T.splitAt i h
        l = T.length o
        r = stRange st
        r' = r {rangeStart = rangeStart r + T.length o}
        st' = st {stHay = h', stRange = r'}
    in  if l == i then (Right o, st') else (Left l, st)
  case et of
    Left l -> errP (ReasonDemand i l)
    Right a -> pure a

-- | Drop the given number of characters from the start of the range, or fewer if empty
dropP :: Monad m => Int -> ParserT e m Int
dropP = fmap T.length . takeP

-- | Drop exactly the given number of characters from the start of the range, or error
dropExactP :: Monad m => Int -> ParserT e m ()
dropExactP = void . takeExactP

-- | Take characters from the start of the range satisfying the predicate
takeWhileP :: Monad m => (Char -> Bool) -> ParserT e m Text
takeWhileP f = stateP $ \st ->
  let h = stHay st
      o = T.takeWhile f h
      l = T.length o
      h' = T.drop l h
      r = stRange st
      r' = r {rangeStart = rangeStart r + l}
      st' = st {stHay = h', stRange = r'}
  in  (o, st')

-- | Like 'takeWhileP' but ensures at least 1 character has been taken
takeWhile1P :: Monad m => (Char -> Bool) -> ParserT e m Text
takeWhile1P f = do
  mt <- stateP $ \st ->
    let h = stHay st
        o = T.takeWhile f h
        l = T.length o
        h' = T.drop l h
        r = stRange st
        r' = r {rangeStart = rangeStart r + l}
        st' = st {stHay = h', stRange = r'}
    in  if l == 0 then (Nothing, st) else (Just o, st')
  case mt of
    Nothing -> errP ReasonTakeNone
    Just a -> pure a

-- | Drop characters from the start of the range satisfying the predicate
dropWhileP :: Monad m => (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

-- | Like 'dropWhileP' but ensures at least 1 character has been dropped
dropWhile1P :: Monad m => (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

-- | Take the remaining range, leaving it empty
takeAllP :: Monad m => ParserT e m Text
takeAllP = stateP $ \st ->
  let h = stHay st
      r = stRange st
      r' = r {rangeStart = rangeEnd r}
      st' = st {stHay = T.empty, stRange = r'}
  in  (h, st')

-- | Like 'takeAllP' but ensures at least 1 character has been taken
takeAll1P :: Monad m => ParserT e m Text
takeAll1P = do
  mt <- stateP $ \st ->
    let h = stHay st
        r = stRange st
        r' = r {rangeStart = rangeEnd r}
        st' = st {stHay = T.empty, stRange = r'}
    in  if T.null h then (Nothing, st) else (Just h, st')
  case mt of
    Nothing -> errP (ReasonDemand 1 0)
    Just a -> pure a

-- | Drop the remaining range, leaving it empty
dropAllP :: Monad m => ParserT e m Int
dropAllP = fmap T.length takeAllP

-- | Like 'dropAllP' but ensures at least 1 character has been dropped
dropAll1P :: Monad m => ParserT e m Int
dropAll1P = fmap T.length takeAll1P

-- | Unwrap a monad transformer layer (see 'scopeP' for use)
transP :: (MonadTrans t, Monad m) => (forall a. t m a -> m a) -> ParserT e (t m) b -> ParserT e m b
transP f (ParserT g) = ParserT $ \j -> do
  st0 <- get
  (ea, st1) <- lift (f (runT (g (hoist lift . j)) st0))
  put st1
  either throwError pure ea

-- | Parse with some local state
scopeP :: Monad m => s -> ParserT e (StateT s m) a -> ParserT e m a
scopeP s0 = transP (`evalStateT` s0)

-- | Repeats the parser until it returns a 'Just' value
iterP :: ParserT e m (Maybe a) -> ParserT e m a
iterP p = go
 where
  go = p >>= maybe go pure

data StrState = StrState !Bool !(Seq Char)

-- | Parse a string with a custom quote character. Supports backslash-escaping.
strP :: Monad m => Char -> ParserT e m Text
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
doubleStrP :: Monad m => ParserT e m Text
doubleStrP = strP '"'

-- | Parse a single-quoted string
singleStrP :: Monad m => ParserT e m Text
singleStrP = strP '\''

-- | Parse between an opening delimiter (first parser) and a closing delimited (second parser)
betweenP :: ParserT e m x -> ParserT e m y -> ParserT e m a -> ParserT e m a
betweenP px py pa = px *> pa <* py

-- | Parse a sequence of items delimited by the first parser
sepByP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
sepByP c p = go
 where
  go = do
    ma <- optP p
    case ma of
      Nothing -> pure Empty
      Just a -> goNext (Empty :|> a)
  goNext !acc = do
    mc <- optP c
    case mc of
      Nothing -> pure acc
      Just _ -> do
        a <- p
        goNext (acc :|> a)

-- | Consumes many spaces at the start of the range
spaceP :: Monad m => ParserT e m ()
spaceP = void (dropWhileP isSpace)

-- | Strips spaces before and after parsing
stripP :: Monad m => ParserT e m a -> ParserT e m a
stripP p = spaceP *> p <* spaceP

-- | Strips spaces before parsing
stripStartP :: Monad m => ParserT e m a -> ParserT e m a
stripStartP p = spaceP *> p

-- | Strips spaces after parsing
stripEndP :: Monad m => ParserT e m a -> ParserT e m a
stripEndP p = p <* spaceP

-- | Parses and returns the length of the consumed input along with the result
measureP :: Monad m => ParserT e m a -> ParserT e m (a, Int)
measureP p = do
  start <- getsP (rangeStart . stRange)
  a <- p
  end <- getsP (rangeStart . stRange)
  pure (a, end - start)

-- | Takes exactly 1 character from the start of the range, returning Nothing
-- if at end of input
unconsP :: Monad m => ParserT e m (Maybe Char)
unconsP = stateP $ \st ->
  let h = stHay st
      mxy = T.uncons h
  in  case mxy of
        Nothing -> (Nothing, st)
        Just (x, y) ->
          let r = stRange st
              r' = r {rangeStart = rangeStart r + 1}
              st' = st {stHay = y, stRange = r'}
          in  (Just x, st')

-- | Takes exactly 1 character from the start of the range, throwing error
-- if at end of input
headP :: Monad m => ParserT e m Char
headP = unconsP >>= maybe (errP (ReasonDemand 1 0)) pure

-- | Add signed-ness to any parser with a negate function
signedWithP :: Monad m => (a -> a) -> ParserT e m a -> ParserT e m a
signedWithP neg p = do
  ms <- optP (charP '-')
  case ms of
    Nothing -> p
    Just _ -> fmap neg p

-- | Add signed-ness to any numeric parser
signedP :: (Monad m, Num a) => ParserT e m a -> ParserT e m a
signedP = signedWithP negate

-- | Parse an signed integer
intP :: Monad m => ParserT e m Integer
intP = signedP uintP

-- | Parse an unsigned integer
uintP :: Monad m => ParserT e m Integer
uintP = T.foldl' (\n d -> n * 10 + fromIntegral (digitToInt d)) 0 <$> takeWhile1P isDigit

-- | Parse a signed decimal
decP :: Monad m => ParserT e m Rational
decP = signedP udecP

-- | Parse an unsigned decimal
udecP :: Monad m => ParserT e m Rational
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
sciP :: Monad m => ParserT e m Scientific
sciP = signedP usciP

-- | Parse an unsigned scientific  number
usciP :: Monad m => ParserT e m Scientific
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
numP :: Monad m => ParserT e m (Either Integer Scientific)
numP = signedWithP (bimap negate negate) unumP

-- | Parse an unsigned integer/scientific number, defaulting to integer if possible.
unumP :: Monad m => ParserT e m (Either Integer Scientific)
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

-- | Repeat a parser until it fails, collecting the results.
repeatP :: Monad m => ParserT e m a -> ParserT e m (Seq a)
repeatP p = go Empty
 where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

-- | Like 'repeatP' but ensures at least 1
repeat1P :: Monad m => ParserT e m a -> ParserT e m (Seq a)
repeat1P p = liftA2 (:<|) p (repeatP p)

-- | Like 'spaceP' but ensures at least 1 space removed
space1P :: Monad m => ParserT e m ()
space1P = void (dropWhile1P isSpace)

-- | Like 'stripP' but ensures at least 1 space removed
strip1P :: Monad m => ParserT e m a -> ParserT e m a
strip1P p = space1P *> p <* space1P

-- | Like 'stripStartP' but ensures at least 1 space removed
stripStart1P :: Monad m => ParserT e m a -> ParserT e m a
stripStart1P p = space1P *> p

-- | Like 'stripEndP' but ensures at least 1 space removed
stripEnd1P :: Monad m => ParserT e m a -> ParserT e m a
stripEnd1P p = p <* space1P

-- | Like 'sepByP' but ensures at least 1 element
sepBy1P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
sepBy1P px pa = liftA2 (:<|) pa (fmap (fromMaybe Empty) (optP (px *> sepByP px pa)))

-- | Like 'sepBy1P' but ensures at least 2 elements (i.e. there was a delimiter)
sepBy2P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
sepBy2P px pa = liftA2 (:<|) (pa <* px) (sepBy1P px pa)

-- | Implement this to format custom errors. The list will be joined with `unlines`.
class HasErrMessage e where
  getErrMessage :: e -> [Text]

instance HasErrMessage Void where
  getErrMessage = absurd

-- private
indent :: Int -> [Text] -> [Text]
indent i = let s = T.replicate (2 * i) " " in fmap (s <>)

instance HasErrMessage e => HasErrMessage (Err e) where
  getErrMessage (Err (ErrF (Range start end) re)) =
    let pos = "Error in range: (" <> T.pack (show start) <> ", " <> T.pack (show end) <> ")"
        body = case re of
          ReasonCustom e ->
            let hd = "Custom error:"
                tl = indent 1 (getErrMessage e)
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
                  "Tried:" : indent 1 (getErrMessage e)
            in  hd : tl
          ReasonInfix errs ->
            let hd = "Infix/split failed:"
                tl = indent 1 $ do
                  (i, _, e) <- toList errs
                  let x = "Tried position: " <> T.pack (show i)
                  x : indent 1 (getErrMessage e)
            in  hd : tl
          ReasonFail msg -> ["User reported failure: " <> msg]
          ReasonLabelled lab e ->
            let hd = "Labelled parser: " <> unLabel lab
                tl = indent 1 (getErrMessage e)
            in  hd : tl
          ReasonLook e ->
            let hd = "Error in lookahead:"
                tl = indent 1 (getErrMessage e)
            in  hd : tl
          ReasonTakeNone -> ["Took/dropped no elements"]
          ReasonEmpty -> ["No parse results"]
    in  pos : body

-- | Create 'Errata' formatting a parse error
errataE :: HasErrMessage e => FilePath -> (Int -> (E.Line, E.Column)) -> Err e -> [E.Errata]
errataE fp mkP e =
  let (line, col) = mkP (rangeStart (errRange e))
      msg = getErrMessage e
      block = E.blockSimple E.basicStyle E.basicPointer fp Nothing (line, col, col + 1, Nothing) (Just (T.unlines msg))
  in  [E.Errata Nothing [block] Nothing]

-- | Render a formatted error to text
renderE :: HasErrMessage e => FilePath -> Text -> Err e -> Text
renderE fp h e =
  let ov = mkOffsetVec h
      mkP = if V.null ov then const (1, 1) else \i -> let (!l, !c) = ov V.! min i (V.length ov - 1) in (l + 1, c + 1)
  in  TL.toStrict (E.prettyErrors h (errataE fp mkP e))

-- | Print a formatted error to stderr
printE :: HasErrMessage e => FilePath -> Text -> Err e -> IO ()
printE fp h e = TIO.hPutStrLn stderr (renderE fp h e)
