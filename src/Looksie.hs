{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , expectP
  , searchNearP
  , searchFarP
  , searchAllP
  , searchAll1P
  , searchLeadP
  , searchLead1P
  , searchTrailP
  , searchTrail1P
  , infixRP
  , infixLP
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
  , headP
  , signedP
  , intP
  , uintP
  , decP
  , udecP
  , repeatP
  , repeat1P
  , space1P
  , strip1P
  , stripStart1P
  , stripEnd1P
  , sepBy1P
  , sepBy2P
  , HasErrMessage (..)
  , errataE
  , renderE
  , printE
  )
where

import Control.Applicative (Alternative (..), liftA2)
import Control.Exception (Exception)
import Control.Monad (ap, void, (>=>))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify', state)
import Control.Monad.Trans (MonadTrans (..))
-- import Control.Monad.Writer.Strict (MonadWriter (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Bitraversable (Bitraversable (..))
import Data.Char (intToDigit, isSpace)
import Data.Foldable (foldl', toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Maybe (fromMaybe, isJust)
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
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
  }
  deriving stock (Eq, Ord, Show)

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
  | ReasonLabeled !Label r
  | ReasonEmpty
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''Reason
deriveBifoldable ''Reason
deriveBitraversable ''Reason

-- | Base functor for 'Err' containing the range and reason for the error
data ErrF e r = ErrF {efRange :: !Range, efReason :: !(Reason e r)}
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

newtype T e m a = T {unT :: ExceptT (Err e) (StateT St m) a}
  deriving newtype (Functor, Applicative, Monad, MonadState St, MonadError (Err e))

instance MonadTrans (T e) where
  lift = T . lift . lift

runT :: T e m a -> St -> m (Either (Err e) a, St)
runT = runStateT . runExceptT . unT

mkErrT :: Monad m => Reason e (Err e) -> T e m (Err e)
mkErrT re = gets stRange >>= \ra -> throwError (Err (ErrF ra re))

errT :: Monad m => Reason e (Err e) -> T e m a
errT = mkErrT >=> throwError

tryT :: Monad m => T e m r -> T e m (Either (Err e) r)
tryT t = get >>= \st -> lift (runT t st) >>= \(er, st') -> er <$ put st'

-- | The parser monad transformer
-- This is essentially `Codensity (T e m) (Either (Err e) a)`
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
  liftIO = lift . liftIO

instance Monad m => MonadFail (ParserT e m) where
  fail = errP . ReasonFail . T.pack

-- TODO add
-- instance MonadReader r m => MonadReader r (ParserT e m) where
--   ask = lift ask
--   local f (ParserT g) = ParserT (local f . g)

-- TODO not sure about this instance
-- instance MonadWriter w m => MonadWriter w (ParserT e m) where
--   writer = lift . writer
--   tell = lift . tell
--   listen (ParserT g) = ParserT $ \j -> do
--     (ea, w) <- listen g
--     j (fmap (,w) ea)
--   pass (ParserT g) = ParserT $ \j -> do
--     ea <- pass (fmap helpPass g)
--     j ea

-- helpPass :: (Either (Err e) (a, w -> w), St) -> ((Either (Err e) a, St), w -> w)
-- helpPass (eaww, st') = either (\e -> ((Left e, st'), id)) (\(a, ww) -> ((Right a, st'), ww)) eaww

-- TODO not sure about this instance
-- instance MonadState s m => MonadState s (ParserT e m) where
--   get = lift get
--   put = lift . put
--   state = lift . state

-- private
-- This is essentially 'lowerCodensity'
runParserT :: Monad m => ParserT e m a -> T e m a
runParserT (ParserT g) = g (either throwError pure)

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
modifyP :: Monad m => (St -> St) -> ParserT e m ()
modifyP f = ParserT (\j -> modify' f >>= j . Right)

-- private
errP :: Monad m => Reason e (Err e) -> ParserT e m a
errP re = ParserT (\j -> mkErrT re >>= j . Left)

-- private
leftoverP :: Monad m => ParserT e m Int
leftoverP = getsP (\st -> let Range s e = stRange st in e - s)

-- | Run a parser transformer
parseT :: Monad m => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p h = fmap fst (runT (runParserT (p <* endP)) (St h (range h)))

-- | Run a parser
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
    [] -> put st0 *> mkErrT (ReasonAlt errs) >>= j . Left
    ParserT g : rest -> do
      es <- g $ \case
        Left err -> pure (Left (AltPhaseBranch, err))
        Right r -> fmap (first (AltPhaseCont,)) (tryT (j (Right r)))
      case es of
        Left e -> put st0 *> go (errs :|> e) rest
        Right s -> pure s

-- | Parse with many possible branches
altP :: (Monad m, Foldable f) => f (ParserT e m a) -> ParserT e m a
altP falts = ParserT (\j -> get >>= \st -> subAltP j st Empty (toList falts))

-- | Fail with no results
emptyP :: Monad m => ParserT e m a
emptyP = ParserT (\j -> get >>= \st -> j (Left (Err (ErrF (stRange st) ReasonEmpty))))

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
lookP (ParserT g) = ParserT (\j -> get >>= \st -> g (\ea -> put st *> j ea))

-- | Labels parse errors
labelP :: Monad m => Label -> ParserT e m a -> ParserT e m a
labelP lab (ParserT g) = ParserT $ \j -> g $ \case
  Left e -> mkErrT (ReasonLabeled lab e) >>= j . Left
  Right a -> j (Right a)

expectP :: Monad m => Text -> ParserT e m ()
expectP n = do
  o <- takeP (T.length n)
  if n == o
    then pure ()
    else errP (ReasonExpect n o)

searchNearP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (x, a)
searchNearP px pa = fmap (\(x, a, _) -> (x, a)) (infixRP px pa (pure ()))

searchFarP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (x, a)
searchFarP px pa = fmap (\(x, a, _) -> (x, a)) (infixLP px pa (pure ()))

searchAllP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
searchAllP px pa = go
 where
  go = do
    maas <- optInfixRP px pa go
    case maas of
      Nothing -> fmap (maybe Empty (:<| Empty)) (optP pa)
      Just (_, a, as) -> pure (a :<| as)

searchAll1P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
searchAll1P px pa = go
 where
  go = do
    maas <- optInfixRP px pa go
    case maas of
      Nothing -> fmap (:<| Empty) pa
      Just (_, a, as) -> pure (a :<| as)

searchLeadP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
searchLeadP px pa = do
  mu <- optP px
  case mu of
    Nothing -> pure Empty
    Just _ -> searchAll1P px pa

searchLead1P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
searchLead1P px pa = px *> searchAll1P px pa

searchTrailP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
searchTrailP px pa = do
  as <- searchAllP px pa
  case as of
    Empty -> pure Empty
    _ -> as <$ px

searchTrail1P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
searchTrail1P px pa = searchAll1P px pa <* px

moveStFwd :: St -> Maybe St
moveStFwd st =
  let (Range s e) = stRange st
  in  if s == e
        then Nothing
        else Just (St {stRange = Range (s + 1) e, stHay = T.tail (stHay st)})

initStBwd :: St -> St
initStBwd st0 =
  let e = rangeEnd (stRange st0)
  in  St {stRange = Range e e, stHay = ""}

moveStBwd :: St -> St -> Maybe St
moveStBwd st0 st =
  let s0 = rangeStart (stRange st0)
      hay0 = stHay st0
      (Range s e) = stRange st
  in  if s == s0
        then Nothing
        else Just (St {stRange = Range (s - 1) e, stHay = T.drop (s - 1) hay0})

subInfixP
  :: Monad m
  => (St -> Maybe St)
  -> ParserT e m x
  -> ParserT e m a
  -> ParserT e m b
  -> (Either (Err e) (Maybe (x, a, b)) -> T e m r)
  -> St
  -> Seq (Int, InfixPhase, Err e)
  -> T e m r
subInfixP = undefined

-- subInfixP mov px pa pb j st0 = goTry
--  where
--   goTry errs = do
--     (exl, _) <- get >>= runParserT (measureP px)
--     case exl of
--       Left _ -> goNext errs stStart
--       Right (x, lenX) -> do
--         let rng0 = stRange st0
--             srt0 = rangeStart rng0
--             hay0 = stHay st0
--             endA = rangeStart (stRange stStart)
--             lenA = endA - srt0
--             rngA = rng0 {rangeEnd = endA}
--             hayA = T.take lenA hay0
--             stA = st0 {stHay = hayA, stRange = rngA}
--             hayB = T.drop (lenA + lenX) hay0
--             srtB = endA + lenX
--             rngB = rng0 {rangeStart = srtB}
--             stB = st0 {stHay = hayB, stRange = rngB}
--         (ea, _) <- runParserT (pa <* endP) stA
--         case ea of
--           Left errA -> goNext (errs :|> (endA, InfixPhaseLeft, errA)) stStart
--           Right a -> do
--             (eb, stC) <- runParserT pb stB
--             case eb of
--               Left errB -> goNext (errs :|> (endA, InfixPhaseRight, errB)) stStart
--               Right b -> do
--                 q@(ec, _) <- j stC (Right (Just (x, a, b)))
--                 case ec of
--                   Left errC -> goNext (errs :|> (endA, InfixPhaseCont, errC)) stStart
--                   Right _ -> pure q
--   goNext !errs stStart =
--     case mov stStart of
--       Nothing ->
--         case errs of
--           Empty -> j st0 (Right Nothing)
--           _ -> j st0 (Left (Err (ErrF (stRange st0) (ReasonInfix errs))))
--       Just stNext -> goTry errs stNext

-- private
optInfixRP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m b -> ParserT e m (Maybe (x, a, b))
optInfixRP px pa pb = ParserT (\j -> get >>= \st0 -> subInfixP moveStFwd px pa pb j st0 Empty)

-- private
requireInfix
  :: Monad m
  => (Either (Err e) (x, a, b) -> T e m r)
  -> (Either (Err e) (Maybe (x, a, b)) -> T e m r)
requireInfix j = \case
  Right mxab ->
    case mxab of
      Nothing -> mkErrT ReasonEmpty >>= j . Left
      Just xab -> j (Right xab)
  Left e -> j (Left e)

-- | Right-associative infix parsing. Searches for the operator from START to END of range.
infixRP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m b -> ParserT e m (x, a, b)
infixRP px pa pb = ParserT $ \j -> do
  st0 <- get
  subInfixP moveStFwd px pa pb (requireInfix j) st0 Empty

-- | Left-associative infix parsing. Searches for the operator from END to START of range.
infixLP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m b -> ParserT e m (x, a, b)
infixLP px pa pb = ParserT $ \j -> do
  st0 <- get
  put (initStBwd st0)
  subInfixP (moveStBwd st0) px pa pb (requireInfix j) st0 Empty

takeP :: Monad m => Int -> ParserT e m Text
takeP i = stateP $ \st ->
  let h = stHay st
      (o, h') = T.splitAt i h
      l = T.length o
      r = stRange st
      r' = r {rangeStart = rangeStart r + l}
      st' = st {stHay = h', stRange = r'}
  in  (o, st')

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

dropP :: Monad m => Int -> ParserT e m Int
dropP = fmap T.length . takeP

dropExactP :: Monad m => Int -> ParserT e m ()
dropExactP = void . takeExactP

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
    Nothing -> errP (ReasonDemand 1 0)
    Just a -> pure a

dropWhileP :: Monad m => (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

dropWhile1P :: Monad m => (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

takeAllP :: Monad m => ParserT e m Text
takeAllP = stateP $ \st ->
  let h = stHay st
      r = stRange st
      r' = r {rangeStart = rangeEnd r}
      st' = st {stHay = T.empty, stRange = r'}
  in  (h, st')

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

dropAllP :: Monad m => ParserT e m Int
dropAllP = fmap T.length takeAllP

dropAll1P :: Monad m => ParserT e m Int
dropAll1P = fmap T.length takeAll1P

-- TODO Add "fold" for string literal parsing

-- data Step e a = StepErr !e | StepDone !a | StepEmpty | StepCont
--   deriving stock (Eq, Ord, Show)

-- data Res e a = ResErr !e | ResDone !a | ResEmpty
--   deriving stock (Eq, Ord, Show)

-- foldRes :: (s -> Maybe Char -> (Step e a, s)) -> Text -> (Res e a, Int, Text)
-- foldRes = undefined

-- foldP :: Monad m => (s -> Maybe Char -> (Step e a, s)) -> s -> ParserT e m a
-- foldP f = ParserT $ \st j -> do
--   let (res, newEnd, newHay) = foldRes f (stHay st)
--   undefined

-- strP :: Monad m => Char -> ParserT e m Text
-- strP = undefined

-- doubleStrP :: Monad m => ParserT e m Text
-- doubleStrP = undefined

-- singleStrP :: Monad m => ParserT e m Text
-- singleStrP = undefined

betweenP :: ParserT e m x -> ParserT e m y -> ParserT e m a -> ParserT e m a
betweenP px py pa = px *> pa <* py

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

spaceP :: Monad m => ParserT e m ()
spaceP = void (dropWhileP isSpace)

stripP :: Monad m => ParserT e m a -> ParserT e m a
stripP p = spaceP *> p <* spaceP

stripStartP :: Monad m => ParserT e m a -> ParserT e m a
stripStartP p = spaceP *> p

stripEndP :: Monad m => ParserT e m a -> ParserT e m a
stripEndP p = p <* spaceP

measureP :: Monad m => ParserT e m a -> ParserT e m (a, Int)
measureP p = do
  start <- getsP (rangeStart . stRange)
  a <- p
  end <- getsP (rangeStart . stRange)
  pure (a, end - start)

headP :: Monad m => ParserT e m Char
headP = fmap T.head (takeExactP 1)

signedP :: (Monad m, Num a) => ParserT e m a -> ParserT e m a
signedP p = do
  ms <- optP (expectP "-")
  case ms of
    Nothing -> p
    Just _ -> fmap negate p

intP :: Monad m => ParserT e m Integer
intP = signedP uintP

uintP :: Monad m => ParserT e m Integer
uintP = foldl' addDigit 0 <$> greedy1P digitP
 where
  addDigit n d = n * 10 + d
  digitP = altP (fmap (\i -> let j = T.singleton (intToDigit i) in (fromIntegral i <$ expectP j)) [0 .. 9])

decP :: Monad m => ParserT e m Rational
decP = signedP udecP

udecP :: Monad m => ParserT e m Rational
udecP = do
  whole <- fmap fromInteger uintP
  dot <- fmap isJust (optP (expectP "."))
  if dot
    then do
      (numerator, places) <- measureP uintP
      let denominator = 10 ^ places
          part = numerator % denominator
      pure (whole + part)
    else pure whole

repeatP :: Monad m => ParserT e m a -> ParserT e m (Seq a)
repeatP p = go Empty
 where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

repeat1P :: Monad m => ParserT e m a -> ParserT e m (Seq a)
repeat1P p = liftA2 (:<|) p (repeatP p)

space1P :: Monad m => ParserT e m ()
space1P = void (dropWhile1P isSpace)

strip1P :: Monad m => ParserT e m a -> ParserT e m a
strip1P p = space1P *> p <* space1P

stripStart1P :: Monad m => ParserT e m a -> ParserT e m a
stripStart1P p = space1P *> p

stripEnd1P :: Monad m => ParserT e m a -> ParserT e m a
stripEnd1P p = p <* space1P

sepBy1P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
sepBy1P px pa = liftA2 (:<|) pa (fmap (fromMaybe Empty) (optP (px *> sepByP px pa)))

sepBy2P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
sepBy2P px pa = liftA2 (:<|) (pa <* px) (sepBy1P px pa)

class HasErrMessage e where
  getErrMessage :: e -> [Text]

instance HasErrMessage Void where
  getErrMessage = absurd

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
            let hd = "Infix/search failed:"
                tl = indent 1 $ do
                  (i, _, e) <- toList errs
                  let x = "Tried position: " <> T.pack (show i)
                  x : indent 1 (getErrMessage e)
            in  hd : tl
          ReasonFail msg -> ["User reported failure: " <> msg]
          ReasonLabeled lab e ->
            let hd = "Labeled parser:" <> unLabel lab
                tl = indent 1 (getErrMessage e)
            in  hd : tl
          ReasonEmpty -> ["No parse results"]
    in  pos : body

errataE :: HasErrMessage e => FilePath -> (Int -> (E.Line, E.Column)) -> Err e -> [E.Errata]
errataE fp mkP e =
  let (line, col) = mkP (rangeStart (errRange e))
      msg = getErrMessage e
      block = E.blockSimple E.basicStyle E.basicPointer fp Nothing (line, col, col + 1, Nothing) (Just (T.unlines msg))
  in  [E.Errata Nothing [block] Nothing]

renderE :: HasErrMessage e => FilePath -> Text -> Err e -> Text
renderE fp h e =
  let ov = mkOffsetVec h
      mkP = if V.null ov then const (1, 1) else \i -> let (!l, !c) = ov V.! min i (V.length ov - 1) in (l + 1, c + 1)
  in  TL.toStrict (E.prettyErrors h (errataE fp mkP e))

printE :: HasErrMessage e => FilePath -> Text -> Err e -> IO ()
printE fp h e = TIO.hPutStrLn stderr (renderE fp h e)
