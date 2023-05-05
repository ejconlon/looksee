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
  , splitNearP
  , splitFarP
  , splitAllP
  , splitAll1P
  , leadP
  , lead1P
  , trailP
  , trail1P
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
import Control.Monad (ap, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (MonadWriter (..))
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

-- | The parser monad transformer
newtype ParserT e m a = ParserT {unParserT :: forall r. St -> (St -> Either (Err e) a -> m (Either (Err e) r, St)) -> m (Either (Err e) r, St)}

instance Functor (ParserT e m) where
  fmap f (ParserT g) = ParserT (\st j -> g st (\st' -> j st' . fmap f))

instance Applicative (ParserT e m) where
  pure a = ParserT (\st j -> j st (Right a))
  (<*>) = ap

instance Monad (ParserT e m) where
  return = pure
  ParserT g >>= f = ParserT (\st j -> g st (\st' -> \case Left e -> j st' (Left e); Right a -> let ParserT h = f a in h st' j))

instance Monad m => Alternative (ParserT e m) where
  empty = emptyP
  p1 <|> p2 = altP [p1, p2]
  many = fmap toList . greedyP
  some = fmap toList . greedy1P

-- | The parser monad
type Parser e = ParserT e Identity

instance MonadFail (ParserT e m) where
  fail = errP . ReasonFail . T.pack

instance MonadTrans (ParserT e) where
  lift ma = ParserT (\st j -> ma >>= j st . Right)

instance MonadReader r m => MonadReader r (ParserT e m) where
  ask = lift ask
  local f (ParserT g) = ParserT (\st j -> local f (g st j))

instance MonadWriter w m => MonadWriter w (ParserT e m) where
  writer = lift . writer
  tell = lift . tell
  listen (ParserT g) = ParserT $ \st j -> do
    ((ea, st'), w) <- listen (g st (\st' ea -> pure (ea, st')))
    j st' (fmap (,w) ea)
  pass (ParserT g) = ParserT $ \st j -> do
    (ea, st') <- pass (fmap helpPass (g st (\st' eaww -> pure (eaww, st'))))
    j st' ea

helpPass :: (Either (Err e) (a, w -> w), St) -> ((Either (Err e) a, St), w -> w)
helpPass (eaww, st') = either (\e -> ((Left e, st'), id)) (\(a, ww) -> ((Right a, st'), ww)) eaww

instance MonadIO m => MonadIO (ParserT e m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ParserT e m) where
  get = lift get
  put = lift . put
  state = lift . state

-- private
runParserT :: Applicative m => ParserT e m a -> St -> m (Either (Err e) a, St)
runParserT (ParserT g) st = g st (\st' ea -> pure (ea, st'))

-- private
getP :: ParserT e m St
getP = ParserT (\st j -> j st (Right st))

-- private
getsP :: (St -> a) -> ParserT e m a
getsP f = ParserT (\st j -> j st (Right (f st)))

-- private
putP :: St -> ParserT e m ()
putP st = ParserT (\_ j -> j st (Right ()))

-- private
stateP :: (St -> (a, St)) -> ParserT e m a
stateP f = ParserT (\st j -> let (a, st') = f st in j st' (Right a))

-- private
modifyP :: (St -> St) -> ParserT e m ()
modifyP f = ParserT (\st j -> j (f st) (Right ()))

-- private
errP :: Reason e (Err e) -> ParserT e m a
errP re = ParserT (\st j -> j st (Left (Err (ErrF (stRange st) re))))

-- private
leftoverP :: ParserT e m Int
leftoverP = getsP (\st -> let Range s e = stRange st in e - s)

-- | Run a parser transformer
parseT :: Applicative m => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p h = fmap fst (runParserT (p <* endP) (St h (range h)))

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
throwP :: e -> ParserT e m a
throwP = errP . ReasonCustom

-- | Succeed if this is the end of input
endP :: ParserT e m ()
endP = do
  l <- leftoverP
  if l == 0
    then pure ()
    else errP (ReasonLeftover l)

-- | Makes parse success optional
optP :: ParserT e m a -> ParserT e m (Maybe a)
optP (ParserT g) = ParserT $ \st0 j ->
  g st0 $ \st1 ea ->
    case ea of
      Left _ -> j st0 (Right Nothing)
      Right a -> j st1 (Right (Just a))

-- private
subAltP :: Monad m => St -> (St -> Either (Err e) a -> m (Either (Err e) r, St)) -> Seq (AltPhase, Err e) -> [ParserT e m a] -> m (Either (Err e) r, St)
subAltP st0 j = go
 where
  go !errs = \case
    [] -> j st0 (Left (Err (ErrF (stRange st0) (ReasonAlt errs))))
    p : rest -> unParserT p st0 $ \st1 er ->
      case er of
        Left err -> go (errs :|> (AltPhaseBranch, err)) rest
        Right r -> do
          q@(es, _) <- j st1 (Right r)
          case es of
            Left err -> go (errs :|> (AltPhaseCont, err)) rest
            Right _ -> pure q

-- | Parse with many possible branches
altP :: (Monad m, Foldable f) => f (ParserT e m a) -> ParserT e m a
altP falts = ParserT (\st0 j -> subAltP st0 j Empty (toList falts))

-- | Fail with no results
emptyP :: ParserT e m a
emptyP = ParserT (\st j -> j st (Left (Err (ErrF (stRange st) ReasonEmpty))))

-- | Parse repeatedly until the parser fails
greedyP :: ParserT e m a -> ParserT e m (Seq a)
greedyP p = go Empty
 where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

-- | Same as 'greedyP' but ensure at least one result
greedy1P :: ParserT e m a -> ParserT e m (Seq a)
greedy1P p = liftA2 (:<|) p (greedyP p)

-- | Lookahead - rewinds state if the parser succeeds, otherwise throws error
lookP :: ParserT e m a -> ParserT e m a
lookP (ParserT g) = ParserT (\st0 j -> g st0 (const (j st0)))

-- | Labels parse errors
labelP :: Label -> ParserT e m a -> ParserT e m a
labelP lab (ParserT g) = ParserT $ \st j -> g st $ \st' ea ->
  j st' $ case ea of
    Left e -> Left (Err (ErrF (stRange st) (ReasonLabeled lab e)))
    Right a -> Right a

expectP :: Text -> ParserT e m ()
expectP n = do
  o <- takeP (T.length n)
  if n == o
    then pure ()
    else errP (ReasonExpect n o)

splitNearP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (x, a)
splitNearP px pa = fmap (\(x, a, _) -> (a, x)) (infixRP pa px (pure ()))

splitFarP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (x, a)
splitFarP px pa = fmap (\(x, a, _) -> (x, a)) (infixRP px pa (pure ()))

splitAllP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
splitAllP px pa = go
 where
  go = fmap (maybe Empty (\(_, a, as) -> a :<| as)) (optInfixRP px pa go)

splitAll1P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
splitAll1P px pa = liftA2 (:<|) (fmap snd (splitNearP px pa)) (splitAllP px pa)

leadP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
leadP px pa = do
  l <- leftoverP
  if l == 0 then pure Empty else lead1P px pa

lead1P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
lead1P px pa = px *> splitAllP px pa

trailP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
trailP px pa = do
  l <- leftoverP
  if l == 0 then pure Empty else trail1P px pa

trail1P :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
trail1P px pa = fmap (\(_, sa, _) -> sa) (infixLP px (splitAllP px pa) endP)

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

subInfixP :: Monad m => (St -> Maybe St) -> ParserT e m x -> ParserT e m a -> ParserT e m b -> St -> (St -> Either (Err e) (Maybe (x, a, b)) -> m (Either (Err e) r, St)) -> Seq (Int, InfixPhase, Err e) -> St -> m (Either (Err e) r, St)
subInfixP mov px pa pb st0 j = goTry
 where
  goTry errs stStart = do
    (exl, _) <- runParserT (measureP px) stStart
    case exl of
      Left _ -> goNext errs stStart
      Right (x, lenX) -> do
        let rng0 = stRange st0
            srt0 = rangeStart rng0
            hay0 = stHay st0
            endA = rangeStart (stRange stStart)
            lenA = endA - srt0
            rngA = rng0 {rangeEnd = endA}
            hayA = T.take lenA hay0
            stA = st0 {stHay = hayA, stRange = rngA}
            hayB = T.drop (lenA + lenX) hay0
            srtB = endA + lenX
            rngB = rng0 {rangeStart = srtB}
            stB = st0 {stHay = hayB, stRange = rngB}
        (ea, _) <- runParserT (pa <* endP) stA
        case ea of
          Left errA -> goNext (errs :|> (endA, InfixPhaseLeft, errA)) stStart
          Right a -> do
            (eb, stC) <- runParserT pb stB
            case eb of
              Left errB -> goNext (errs :|> (endA, InfixPhaseRight, errB)) stStart
              Right b -> do
                q@(ec, _) <- j stC (Right (Just (x, a, b)))
                case ec of
                  Left errC -> goNext (errs :|> (endA, InfixPhaseCont, errC)) stStart
                  Right _ -> pure q
  goNext !errs stStart =
    case mov stStart of
      Nothing ->
        case errs of
          Empty -> j st0 (Right Nothing)
          _ -> j st0 (Left (Err (ErrF (stRange st0) (ReasonInfix errs))))
      Just stNext -> goTry errs stNext

-- private
optInfixRP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m b -> ParserT e m (Maybe (x, a, b))
optInfixRP px pa pb = ParserT (\st0 j -> subInfixP moveStFwd px pa pb st0 j Empty st0)

-- private
requireInfix :: (St -> Either (Err e) (x, a, b) -> m (r, St)) -> (St -> Either (Err e) (Maybe (x, a, b)) -> m (r, St))
requireInfix j st = \case
  Right mxab ->
    case mxab of
      Nothing -> j st (Left (Err (ErrF (stRange st) (ReasonInfix Empty))))
      Just xab -> j st (Right xab)
  Left e -> j st (Left e)

-- | Right-associative infix parsing. Searches for the operator from START to END of range.
infixRP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m b -> ParserT e m (x, a, b)
infixRP px pa pb = ParserT (\st0 j -> subInfixP moveStFwd px pa pb st0 (requireInfix j) Empty st0)

-- | Left-associative infix parsing. Searches for the operator from END to START of range.
infixLP :: Monad m => ParserT e m x -> ParserT e m a -> ParserT e m b -> ParserT e m (x, a, b)
infixLP px pa pb = ParserT (\st0 j -> subInfixP (moveStBwd st0) px pa pb st0 (requireInfix j) Empty (initStBwd st0))

takeP :: Int -> ParserT e m Text
takeP i = stateP $ \st ->
  let h = stHay st
      (o, h') = T.splitAt i h
      l = T.length o
      r = stRange st
      r' = r {rangeStart = rangeStart r + l}
      st' = st {stHay = h', stRange = r'}
  in  (o, st')

takeExactP :: Int -> ParserT e m Text
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

dropP :: Int -> ParserT e m Int
dropP = fmap T.length . takeP

dropExactP :: Int -> ParserT e m ()
dropExactP = void . takeExactP

takeWhileP :: (Char -> Bool) -> ParserT e m Text
takeWhileP f = stateP $ \st ->
  let h = stHay st
      o = T.takeWhile f h
      l = T.length o
      h' = T.drop l h
      r = stRange st
      r' = r {rangeStart = rangeStart r + l}
      st' = st {stHay = h', stRange = r'}
  in  (o, st')

takeWhile1P :: (Char -> Bool) -> ParserT e m Text
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

dropWhileP :: (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

dropWhile1P :: (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

takeAllP :: ParserT e m Text
takeAllP = stateP $ \st ->
  let h = stHay st
      r = stRange st
      r' = r {rangeStart = rangeEnd r}
      st' = st {stHay = T.empty, stRange = r'}
  in  (h, st')

takeAll1P :: ParserT e m Text
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

dropAllP :: ParserT e m Int
dropAllP = fmap T.length takeAllP

dropAll1P :: ParserT e m Int
dropAll1P = fmap T.length takeAll1P

betweenP :: ParserT e m x -> ParserT e m y -> ParserT e m a -> ParserT e m a
betweenP px py pa = px *> pa <* py

sepByP :: ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
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

spaceP :: ParserT e m ()
spaceP = void (dropWhileP isSpace)

stripP :: ParserT e m a -> ParserT e m a
stripP p = spaceP *> p <* spaceP

stripStartP :: ParserT e m a -> ParserT e m a
stripStartP p = spaceP *> p

stripEndP :: ParserT e m a -> ParserT e m a
stripEndP p = p <* spaceP

measureP :: ParserT e m a -> ParserT e m (a, Int)
measureP p = do
  start <- getsP (rangeStart . stRange)
  a <- p
  end <- getsP (rangeStart . stRange)
  pure (a, end - start)

headP :: ParserT e m Char
headP = fmap T.head (takeExactP 1)

signedP :: Num a => ParserT e m a -> ParserT e m a
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

repeatP :: ParserT e m a -> ParserT e m (Seq a)
repeatP p = go Empty
 where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

repeat1P :: ParserT e m a -> ParserT e m (Seq a)
repeat1P p = liftA2 (:<|) p (repeatP p)

space1P :: ParserT e m ()
space1P = void (dropWhile1P isSpace)

strip1P :: ParserT e m a -> ParserT e m a
strip1P p = space1P *> p <* space1P

stripStart1P :: ParserT e m a -> ParserT e m a
stripStart1P p = space1P *> p

stripEnd1P :: ParserT e m a -> ParserT e m a
stripEnd1P p = p <* space1P

sepBy1P :: ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
sepBy1P px pa = liftA2 (:<|) pa (fmap (fromMaybe Empty) (optP (px *> sepByP px pa)))

sepBy2P :: ParserT e m x -> ParserT e m a -> ParserT e m (Seq a)
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
            ["Expected text: " <> expected <> "' but found: '" <> actual <> "'"]
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
