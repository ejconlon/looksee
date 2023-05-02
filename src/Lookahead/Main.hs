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
  , Side (..)
  , ParserT
  , Parser
  , parseT
  , parse
  , throwP
  , mapErrorP
  , endP
  , optP
  , altP
  , greedyP
  , greedy1P
  , lookP
  , expectP
  , infixP
  , takeP
  , dropP
  , takeWhileP
  , dropWhileP
  , betweenP
  , sepByP
  , Value
  , jsonParser
  )
where

import Control.Applicative (liftA2)
import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (MonadWriter)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Bitraversable (Bitraversable (..))
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Error.Diagnose qualified as D
import System.IO (stderr)

modifyError :: Monad m => (e -> x) -> ExceptT e m a -> ExceptT x m a
modifyError f m = lift (runExceptT m) >>= either (throwError . f) pure

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

data Side = SideLeft | SideRight
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Reason e r
  = ReasonCustom !e
  | ReasonExpect !Text !Text
  | ReasonDemand !Int !Int
  | ReasonLeftover !Int
  | ReasonAlt !(Seq (Text, r))
  | ReasonInfix !Text !(Seq (Int, Side, r))
  | ReasonEmptyInfix
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

newtype ParserT e m a = ParserT {unP :: ExceptT (Err e) (StateT St m) a}
  deriving newtype (Functor, Applicative, Monad)

type Parser = ParserT Void Identity

instance Monad m => MonadFail (ParserT e m) where
  fail = errP . ReasonFail . T.pack

instance MonadTrans (ParserT e) where
  lift = ParserT . lift . lift

deriving instance MonadReader r m => MonadReader r (ParserT e m)

deriving instance MonadWriter w m => MonadWriter w (ParserT e m)

deriving instance MonadIO m => MonadIO (ParserT e m)

instance MonadState s m => MonadState s (ParserT e m) where
  get = lift get
  put = lift . put
  state = lift . state

-- private

runParserT :: ParserT e m a -> St -> m (Either (Err e) a, St)
runParserT p = runStateT (runExceptT (unP p))

errP :: Monad m => Reason e (Err e) -> ParserT e m a
errP re = do
  ra <- ParserT (gets stRange)
  ParserT (throwError (Err (ErrF ra re)))

leftoverP :: Monad m => ParserT e m Int
leftoverP = do
  Range s e <- ParserT (gets stRange)
  return (e - s)

-- public

parseT :: Monad m => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p h = fmap fst (runParserT (p <* endP) (St h (range h) Empty))

parse :: Parser a -> Text -> Either (Err Void) a
parse p h = runIdentity (parseT p h)

throwP :: Monad m => e -> ParserT e m a
throwP = errP . ReasonCustom

mapErrorP :: Monad m => (e -> x) -> ParserT e m a -> ParserT x m a
mapErrorP f p = ParserT (modifyError (fmap f) (unP p))

endP :: Monad m => ParserT e m ()
endP = do
  l <- leftoverP
  if l == 0
    then pure ()
    else errP (ReasonLeftover l)

optP :: Monad m => ParserT e m a -> ParserT e m (Maybe a)
optP p = do
  st0 <- ParserT get
  (ea, st1) <- lift (runParserT p st0)
  case ea of
    Left _ -> pure Nothing
    Right a -> Just a <$ ParserT (put st1)

altP :: Monad m => Foldable f => f (Text, ParserT e m a) -> ParserT e m a
altP = go . toList
 where
  go xps = do
    st0 <- ParserT get
    goNext st0 Empty xps
  goNext st0 !errs = \case
    [] -> errP (ReasonAlt errs)
    (x, p) : xps' -> do
      (ea, st1) <- lift (runParserT p st0)
      case ea of
        Left err -> goNext st0 (errs :|> (x, err)) xps'
        Right a -> a <$ ParserT (put st1)

greedyP :: Monad m => ParserT e m a -> ParserT e m (Seq a)
greedyP p = go Empty
 where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

greedy1P :: Monad m => ParserT e m a -> ParserT e m (Seq a)
greedy1P p = liftA2 (:<|) p (greedyP p)

lookP :: Monad m => ParserT e m a -> ParserT e m a
lookP p = do
  st0 <- ParserT get
  (ea, _) <- lift (runParserT p st0)
  case ea of
    Left err -> ParserT (throwError err)
    Right a -> pure a

expectP :: Monad m => Text -> ParserT e m ()
expectP n = do
  o <- takeP (T.length n)
  if n == o
    then pure ()
    else errP (ReasonExpect n o)

infixP :: Monad m => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (a, b)
infixP n pa pb = go
 where
  go =
    if T.null n
      then errP ReasonEmptyInfix
      else do
        st0 <- ParserT get
        goNext st0 Empty (T.breakOnAll n (stHay st0))
  goNext st0 !eacc = \case
    [] -> errP (ReasonInfix n eacc)
    (h1, h2) : rest -> do
      let r = stRange st0
          e1 = rangeStart r + T.length h1
          st1 = st0 {stHay = h1, stRange = r {rangeEnd = e1}}
          st2 = st0 {stHay = h2, stRange = r {rangeStart = e1 + T.length n}}
      (ea1, _) <- lift (runParserT (pa <* endP) st1)
      case ea1 of
        Left err1 -> goNext st0 (eacc :|> (e1, SideLeft, err1)) rest
        Right a -> do
          (ea2, st3) <- lift (runParserT pb st2)
          case ea2 of
            Left err2 -> goNext st0 (eacc :|> (e1, SideRight, err2)) rest
            Right b -> (a, b) <$ ParserT (put st3)

takeP :: Monad m => Int -> ParserT e m Text
takeP i = ParserT $ state $ \st ->
  let h = stHay st
      (o, h') = T.splitAt i h
      l = T.length o
      r = stRange st
      r' = r {rangeStart = rangeStart r + l}
      st' = st {stHay = h', stRange = r'}
  in  (o, st')

takeExactP :: Monad m => Int -> ParserT e m Text
takeExactP i = do
  et <- ParserT $ state $ \st ->
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
takeWhileP f = ParserT $ state $ \st ->
  let h = stHay st
      o = T.takeWhile f h
      l = T.length o
      h' = T.drop l h
      r = stRange st
      r' = r {rangeStart = rangeStart r + l}
  in  (o, st {stHay = h', stRange = r'})

takeWhile1P :: Monad m => (Char -> Bool) -> ParserT e m Text
takeWhile1P f = do
  mt <- ParserT $ state $ \st ->
    let h = stHay st
        o = T.takeWhile f h
        l = T.length o
        h' = T.drop l h
        r = stRange st
        r' = r {rangeStart = rangeStart r + l}
        st' = st {stHay = h', stRange = r'}
    in  if l > 0 then (Just o, st') else (Nothing, st)
  case mt of
    Nothing -> errP (ReasonDemand 1 0)
    Just a -> pure a

dropWhileP :: Monad m => (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

dropWhile1P :: Monad m => (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

betweenP :: Monad m => ParserT e m x -> ParserT e m y -> ParserT e m a -> ParserT e m a
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

class HasErrMessage e where
  errMessage :: e -> Text

instance HasErrMessage Void where
  errMessage = absurd

reportE :: HasErrMessage e => (Int -> D.Position) -> Err e -> D.Report Text
reportE mkP (Err (ErrF (Range s _) re)) =
  let msg = case re of
        ReasonCustom e -> errMessage e
        ReasonExpect _ _ -> undefined
        ReasonDemand _ _ -> undefined
        ReasonLeftover _ -> undefined
        ReasonAlt _ -> undefined
        ReasonInfix _ _ -> undefined
        ReasonEmptyInfix -> undefined
        ReasonFail _ -> undefined
      pos = mkP s
  in  D.Err Nothing msg [(pos, D.This "^")] []

diagnoseE :: HasErrMessage e => FilePath -> Text -> Err e -> D.Diagnostic Text
diagnoseE fp h e = undefined

printE :: HasErrMessage e => FilePath -> Text -> Err e -> IO ()
printE fp h e = D.printDiagnostic stderr True True 2 D.defaultStyle (diagnoseE fp h e)

data Value = ValueNull | ValueString !Text | ValueArray !(Seq Value) | ValueObject !(Seq (Text, Value))
  deriving stock (Eq, Ord, Show)

jsonParser :: Parser Value
jsonParser = valP
 where
  spaceP = void (dropWhileP isSpace)
  valP = spaceP *> rawValP <* spaceP
  rawValP =
    altP
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
