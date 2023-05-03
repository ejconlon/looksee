{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lookahead.Main
  ( Range (..)
  , range
  , Reason (..)
  , ErrF (..)
  , Err (..)
  , errRange
  , errReason
  , AltPhase (..)
  , InfixPhase (..)
  , SplitType (..)
  , ParserT
  , Parser
  , parseT
  , parse
  , parseI
  , throwP
  , endP
  , optP
  , altP
  , greedyP
  , greedy1P
  , lookP
  , expectP
  , splitP
  , splitEndP
  , splitAllP
  , splitAll1P
  , leadP
  , lead1P
  , trailP
  , trail1P
  , infixP
  , takeP
  , dropP
  , takeWhileP
  , dropWhileP
  , betweenP
  , sepByP
  , spaceP
  , HasErrMessage (..)
  , errataE
  , renderE
  , printE
  , Value (..)
  , jsonParser
  , Arith (..)
  , arithParser
  )
where

import Control.Applicative (liftA2)
import Control.Exception (Exception)
import Control.Monad (ap, void)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
-- import Control.Monad.Writer.Strict (MonadWriter (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Bitraversable (Bitraversable (..))
import Data.Char (isAlpha, isSpace)
import Data.Foldable (foldl', toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq (..))
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

modifyError :: Monad m => (e -> x) -> ExceptT e m a -> ExceptT x m a
modifyError f m = lift (runExceptT m) >>= either (throwError . f) pure

type OffsetVec = Vector (Int, Int)

mkOffsetVec :: Text -> OffsetVec
mkOffsetVec t = V.unfoldrN (T.length t) go ((0, 0), T.unpack t)
 where
  go (p@(!line, !col), xs) =
    case xs of
      [] -> Nothing
      x : xs' -> Just (p, if x == '\n' then ((line + 1, 0), xs') else ((line, col + 1), xs'))

data Range = Range {rangeStart :: !Int, rangeEnd :: !Int}
  deriving stock (Eq, Ord, Show)

range :: Text -> Range
range t = Range 0 (T.length t)

data St = St
  { stHay :: !Text
  , stRange :: !Range
  , stLabels :: !(Seq Text)
  }
  deriving stock (Eq, Ord, Show)

data AltPhase = AltPhaseBranch | AltPhaseCont
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data InfixPhase = InfixPhaseLeft | InfixPhaseRight | InfixPhaseCont
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data SplitType = SplitTypeStart | SplitTypeEnd | SplitTypeAll
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Reason e r
  = ReasonCustom !e
  | ReasonExpect !Text !Text
  | ReasonDemand !Int !Int
  | ReasonLeftover !Int
  | ReasonAlt !Text !(Seq (Text, AltPhase, r))
  | ReasonInfix !Text !(Seq (Int, InfixPhase, r))
  | ReasonSplit !SplitType !Text !(Maybe (Int, r))
  | ReasonEmptyQuery
  | ReasonFail !Text
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''Reason
deriveBifoldable ''Reason
deriveBitraversable ''Reason

data ErrF e r = ErrF {efRange :: !Range, efReason :: !(Reason e r)}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''ErrF
deriveBifoldable ''ErrF
deriveBitraversable ''ErrF

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

errRange :: Err e -> Range
errRange = efRange . unErr

errReason :: Err e -> Reason e (Err e)
errReason = efReason . unErr

newtype ParserT e m a = ParserT {unParserT :: forall r. St -> (St -> Either (Err e) a -> m (Either (Err e) r, St)) -> m (Either (Err e) r, St)}

instance Functor (ParserT e m) where
  fmap f (ParserT g) = ParserT (\st j -> g st (\st' -> j st' . fmap f))

instance Applicative (ParserT e m) where
  pure a = ParserT (\st j -> j st (Right a))
  (<*>) = ap

instance Monad (ParserT e m) where
  return = pure
  ParserT g >>= f = ParserT (\st j -> g st (\st' -> \case Left e -> j st' (Left e); Right a -> let ParserT h = f a in h st' j))

type Parser e = ParserT e Identity

instance MonadFail (ParserT e m) where
  fail = stErrP . ReasonFail . T.pack

instance MonadTrans (ParserT e) where
  lift ma = ParserT (\st j -> ma >>= j st . Right)

instance MonadReader r m => MonadReader r (ParserT e m) where
  ask = lift ask
  local f (ParserT g) = ParserT (\st j -> local f (g st j))

-- instance MonadWriter w m => MonadWriter w (ParserT e m) where
--   writer = lift . writer
--   tell = lift . tell
--   listen (ParserT g) = ParserT (\st j -> _)
--   pass (ParserT g) = ParserT (\st j -> _)

instance MonadIO m => MonadIO (ParserT e m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ParserT e m) where
  get = lift get
  put = lift . put
  state = lift . state

-- private

runParserT :: Applicative m => ParserT e m a -> St -> m (Either (Err e) a, St)
runParserT (ParserT g) st = g st (\st' ea -> pure (ea, st'))

getP :: ParserT e m St
getP = ParserT (\st j -> j st (Right st))

getsP :: (St -> a) -> ParserT e m a
getsP f = ParserT (\st j -> j st (Right (f st)))

putP :: St -> ParserT e m ()
putP st = ParserT (\_ j -> j st (Right ()))

stateP :: (St -> (a, St)) -> ParserT e m a
stateP f = ParserT (\st j -> let (a, st') = f st in j st' (Right a))

modifyP :: (St -> St) -> ParserT e m ()
modifyP f = ParserT (\st j -> j (f st) (Right ()))

-- mkErrP :: Range -> Reason e (Err e) -> ParserT e m a
-- mkErrP ra re = ParserT (\st j -> j st (Left (Err (ErrF ra re))))

stErrP :: Reason e (Err e) -> ParserT e m a
stErrP re = ParserT (\st j -> j st (Left (Err (ErrF (stRange st) re))))

leftoverP :: ParserT e m Int
leftoverP = getsP (\st -> let Range s e = stRange st in e - s)

-- public

parseT :: Applicative m => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p h = fmap fst (runParserT (p <* endP) (St h (range h) Empty))

parse :: Parser e a -> Text -> Either (Err e) a
parse p h = runIdentity (parseT p h)

parseI :: HasErrMessage e => Parser e a -> Text -> IO (Maybe a)
parseI p h =
  case parse p h of
    Left e -> Nothing <$ printE "<interactive>" h e
    Right a -> pure (Just a)

throwP :: e -> ParserT e m a
throwP = stErrP . ReasonCustom

endP :: ParserT e m ()
endP = do
  l <- leftoverP
  if l == 0
    then pure ()
    else stErrP (ReasonLeftover l)

optP :: ParserT e m a -> ParserT e m (Maybe a)
optP (ParserT g) = ParserT $ \st0 j ->
  g st0 $ \st1 ea ->
    case ea of
      Left _ -> j st0 (Right Nothing)
      Right a -> j st1 (Right (Just a))

subAltP :: Monad m => Text -> St -> (St -> Either (Err e) a -> m (Either (Err e) r, St)) -> Seq (Text, AltPhase, Err e) -> [(Text, ParserT e m a)] -> m (Either (Err e) r, St)
subAltP lab st0 j = go
 where
  go !errs = \case
    [] -> j st0 (Left (Err (ErrF (stRange st0) (ReasonAlt lab errs))))
    (x, p) : rest -> unParserT p st0 $ \st1 er ->
      case er of
        Left err -> go (errs :|> (x, AltPhaseBranch, err)) rest
        Right r -> do
          q@(es, _) <- j st1 (Right r)
          case es of
            Left err -> go (errs :|> (x, AltPhaseCont, err)) rest
            Right _ -> pure q

altP :: Monad m => Foldable f => Text -> f (Text, ParserT e m a) -> ParserT e m a
altP lab falts = ParserT (\st0 j -> subAltP lab st0 j Empty (toList falts))

greedyP :: ParserT e m a -> ParserT e m (Seq a)
greedyP p = go Empty
 where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

greedy1P :: ParserT e m a -> ParserT e m (Seq a)
greedy1P p = liftA2 (:<|) p (greedyP p)

lookP :: ParserT e m a -> ParserT e m a
lookP (ParserT g) = ParserT (\st0 j -> g st0 (const (j st0)))

expectP :: Text -> ParserT e m ()
expectP n = do
  o <- takeP (T.length n)
  if n == o
    then pure ()
    else stErrP (ReasonExpect n o)

splitP :: Monad m => Text -> ParserT e m a -> ParserT e m a
splitP n p =
  if T.null n
    then stErrP ReasonEmptyQuery
    else do
      st0 <- getP
      let (h1, h2) = T.breakOn n (stHay st0)
      if T.null h2
        then stErrP (ReasonSplit SplitTypeStart n Nothing)
        else do
          let r = stRange st0
              e1 = rangeStart r + T.length h1
              st1 = st0 {stHay = h1, stRange = r {rangeEnd = e1}}
              l = T.length n
              st2 = st0 {stHay = T.drop l h2, stRange = r {rangeStart = e1 + l}}
          (ea1, _) <- lift (runParserT (p <* endP) st1)
          case ea1 of
            Left err1 -> stErrP (ReasonSplit SplitTypeStart n (Just (e1, err1)))
            Right a -> a <$ putP st2

splitEndP :: Monad m => Text -> ParserT e m a -> ParserT e m a
splitEndP n p = undefined

-- if T.null n
--   then stErrP ReasonEmptyQuery
--   else do
--     st0 <- getP
--     let (h1, h2) = T.breakOnEnd n (stHay st0)
--     if T.null h2
--       then stErrP (ReasonSplit SplitTypeEnd n Nothing)
--       else do
--         let r = stRange st0
--             e1 = rangeStart r + T.length h1
--             st1 = st0 {stHay = h1, stRange = r {rangeEnd = e1}}
--             l = T.length n
--             st2 = st0 {stHay = T.drop l h2, stRange = r {rangeStart = e1 + l}}
--         (ea1, _) <- lift (runParserT (p <* endP) st1)
--         case ea1 of
--           Left err1 -> stErrP (ReasonSplit SplitTypeEnd n (Just (e1, err1)))
--           Right a -> a <$ putP st2

zipWithOffset :: Int -> [Text] -> [(Int, Text)]
zipWithOffset l = go 0
 where
  go !o = \case
    [] -> []
    x : xs -> (o, x) : go (o + l + T.length x) xs

splitAllP :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
splitAllP n p = go
 where
  go =
    if T.null n
      then stErrP ReasonEmptyQuery
      else do
        hs <- getsP (T.splitOn n . stHay)
        let ohs = zipWithOffset (T.length n) hs
        end <- getsP (rangeEnd . stRange)
        vals <- goNext Empty ohs
        modifyP (\st -> st {stHay = "", stRange = (stRange st) {rangeStart = end}})
        pure vals
  goNext !acc = \case
    [] -> pure acc
    (x1, h1) : rest -> do
      st0 <- getP
      let s1 = rangeStart (stRange st0) + x1
      let e1 = s1 + T.length h1
      let st1 = st0 {stHay = h1, stRange = Range s1 e1}
      (ea1, _) <- lift (runParserT (p <* endP) st1)
      case ea1 of
        Left err1 -> stErrP (ReasonSplit SplitTypeAll n (Just (e1, err1)))
        Right a -> goNext (acc :|> a) rest

splitAll1P :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
splitAll1P n p = liftA2 (:<|) (splitP n p) (splitAllP n p)

leadP :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
leadP n p = do
  l <- leftoverP
  if l == 0 then pure Empty else lead1P n p

lead1P :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
lead1P n p = expectP n *> splitAllP n p

trailP :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
trailP n p = do
  l <- leftoverP
  if l == 0 then pure Empty else trail1P n p

trail1P :: Monad m => Text -> ParserT e m a -> ParserT e m (Seq a)
trail1P n p = splitEndP n (splitAllP n p) <* endP

subInfixP :: Monad m => Text -> ParserT e m a -> ParserT e m b -> St -> (St -> Either (Err e) (a, b) -> m (Either (Err e) r, St)) -> Seq (Int, InfixPhase, Err e) -> [(Text, Text)] -> m (Either (Err e) r, St)
subInfixP n pa pb st0 j = go
 where
  go !errs = \case
    [] -> j st0 (Left (Err (ErrF (stRange st0) (ReasonInfix n errs))))
    (h1, h2) : rest -> do
      let r = stRange st0
          e1 = rangeStart r + T.length h1
          st1 = st0 {stHay = h1, stRange = r {rangeEnd = e1}}
          l = T.length n
          st2 = st0 {stHay = T.drop l h2, stRange = r {rangeStart = e1 + l}}
      (ea1, _) <- runParserT (pa <* endP) st1
      case ea1 of
        Left err1 -> go (errs :|> (e1, InfixPhaseLeft, err1)) rest
        Right a -> do
          (ea2, st3) <- runParserT pb st2
          case ea2 of
            Left err2 -> go (errs :|> (e1, InfixPhaseRight, err2)) rest
            Right b -> do
              q@(ea3, _) <- j st3 (Right (a, b))
              case ea3 of
                Left err3 -> go (errs :|> (e1, InfixPhaseCont, err3)) rest
                Right _ -> pure q

infixP :: Monad m => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (a, b)
infixP n pa pb =
  if T.null n
    then stErrP ReasonEmptyQuery
    else ParserT (\st0 j -> subInfixP n pa pb st0 j Empty (T.breakOnAll n (stHay st0)))

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
    Left l -> stErrP (ReasonDemand i l)
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
  in  (o, st {stHay = h', stRange = r'})

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
    in  if l > 0 then (Just o, st') else (Nothing, st)
  case mt of
    Nothing -> stErrP (ReasonDemand 1 0)
    Just a -> pure a

dropWhileP :: (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

dropWhile1P :: (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

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
          ReasonCustom e -> getErrMessage e
          ReasonExpect expected actual -> ["Expected string: '" <> expected <> "' but found: '" <> actual <> "'"]
          ReasonDemand expected actual -> ["Expected num chars: " <> T.pack (show expected) <> " but got: " <> T.pack (show actual)]
          ReasonLeftover count -> ["Expected end but had leftover count: " <> T.pack (show count)]
          ReasonAlt name errs ->
            let hd = "Alternatives failed: " <> name
                tl = indent 1 $ do
                  (n, _, e) <- toList errs
                  let x = "Tried alternative: " <> n
                  x : indent 1 (getErrMessage e)
            in  hd : tl
          ReasonInfix op errs ->
            let hd = "Infix operator failed: " <> op
                tl = indent 1 $ do
                  (i, _, e) <- toList errs
                  let x = "Tried position: " <> T.pack (show i)
                  x : indent 1 (getErrMessage e)
            in  hd : tl
          ReasonSplit ty op merr ->
            let nm = case ty of
                  SplitTypeStart -> "Split"
                  SplitTypeEnd -> "Split end"
                  SplitTypeAll -> "Split all"
                hd = nm <> " operator failed: " <> op
            in  case merr of
                  Nothing -> [hd <> " - no results"]
                  Just (i, e) ->
                    let x = "Tried position: " <> T.pack (show i)
                        tl = indent 1 (x : indent 1 (getErrMessage e))
                    in  hd : tl
          ReasonEmptyQuery -> ["Empty query string"]
          ReasonFail msg -> ["User reported failure: " <> msg]
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

data Value = ValueNull | ValueString !Text | ValueArray !(Seq Value) | ValueObject !(Seq (Text, Value))
  deriving stock (Eq, Ord, Show)

jsonParser :: Parser Void Value
jsonParser = valP
 where
  valP = spaceP *> rawValP <* spaceP
  rawValP =
    altP
      "value"
      [ ("null", nullP)
      , ("str", strP)
      , ("array", arrayP)
      , ("object", objectP)
      ]
  nullP = ValueNull <$ expectP "null"
  rawStrP = betweenP (expectP "\"") (expectP "\"") (takeWhileP (/= '"'))
  strP = ValueString <$> rawStrP
  arrayP = ValueArray <$> betweenP (expectP "[") (expectP "]") (sepByP (expectP ",") valP)
  rawPairP = do
    s <- rawStrP
    spaceP
    expectP ":"
    spaceP
    v <- rawValP
    pure (s, v)
  pairP = spaceP *> rawPairP <* spaceP
  objectP = ValueObject <$> betweenP (expectP "{") (expectP "}") (sepByP (expectP ",") pairP)

data Arith
  = ArithNum !Int
  | ArithVar !Text
  | ArithNeg Arith
  | ArithMul Arith Arith
  | ArithAdd Arith Arith
  | ArithSub Arith Arith
  deriving stock (Eq, Ord, Show)

arithParser :: Parser Void Arith
arithParser = rootP
 where
  addDigit n d = n * 10 + d
  digitP = altP "digit" (fmap (\i -> let j = T.pack (show i) in (j, i <$ expectP j)) [0 .. 9])
  identP = takeWhile1P isAlpha
  numP = foldl' addDigit 0 <$> greedy1P digitP
  binaryP op f = uncurry f <$> infixP op rootP rootP
  unaryP op f = expectP op *> fmap f rootP
  rawRootP =
    altP
      "root"
      [ ("add", binaryP "+" ArithAdd)
      , ("sub", binaryP "-" ArithSub)
      , ("mul", binaryP "*" ArithMul)
      , ("neg", unaryP "-" ArithNeg)
      , ("paren", betweenP (expectP "(") (expectP ")") rootP)
      , ("num", ArithNum <$> numP)
      , ("var", ArithVar <$> identP)
      ]
  rootP = spaceP *> rawRootP <* spaceP
