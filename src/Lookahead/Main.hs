{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lookahead.Main
  ( Range (..)
  , range
  , Err (..)
  , Reason (..)
  , Side (..)
  , P
  , parse
  , throwP
  , mapErrorP
  , endP
  , optP
  , altP
  , greedyP
  , greedy1P
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
  ) where

import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (toList)
import Control.Exception (Exception)
import Control.Monad.State (State, MonadState (..), runState, gets)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Data.Sequence (Seq (..))
-- import qualified Data.Sequence as Seq
import Data.Char (isSpace)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)
import Control.Monad.Trans (lift)

modifyError :: Monad m => (e -> x) -> ExceptT e m a -> ExceptT x m a
modifyError f m = lift (runExceptT m) >>= either (throwError . f) pure

data Range = Range { rangeStart :: !Int, rangeEnd :: !Int }
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

data Reason r e =
    ReasonCustom !e
  | ReasonExpect !Text !Text
  | ReasonLeftover !Int
  | ReasonAlt !(Seq (Text, r))
  | ReasonInfix !Text !(Seq (Int, Side, r))
  | ReasonEmptyInfix
  deriving stock (Eq, Ord, Show)

deriveBifunctor ''Reason
deriveBifoldable ''Reason
deriveBitraversable ''Reason

data Err e = Err
  { errRange :: !Range
  , errReason :: !(Reason (Err e) e)
  }
  deriving stock (Eq, Ord, Show)

instance Functor Err where
  fmap f = go where
    go (Err ra re) = Err ra (bimap go f re)

instance (Typeable e, Show e) => Exception (Err e)

newtype P e a = P { unP :: ExceptT (Err e) (State St) a }
  deriving newtype (Functor, Applicative, Monad)

-- private

runP :: P e a -> St -> (Either (Err e) a, St)
runP p = runState (runExceptT (unP p))

errP :: Reason (Err e) e -> P e a
errP re = do
  ra <- P (gets stRange)
  P (throwError (Err ra re))

leftoverP :: P e Int
leftoverP = do
  Range s e <- P (gets stRange)
  return (e - s)

-- public

parse :: P e a -> Text -> Either (Err e) a
parse p h = fst (runP (p <* endP) (St h (range h) Empty))

throwP :: e -> P e a
throwP = errP . ReasonCustom

mapErrorP :: (e -> x) -> P e a -> P x a
mapErrorP f p = P (modifyError (fmap f) (unP p))

endP :: P e ()
endP = do
  l <- leftoverP
  if l == 0
    then pure ()
    else errP (ReasonLeftover l)

optP :: P e a -> P e (Maybe a)
optP p = do
  st0 <- P get
  let (ea, st1) = runP p st0
  case ea of
    Left _ -> pure Nothing
    Right a -> Just a <$ P (put st1)

altP :: Foldable f => f (Text, P e a) -> P e a
altP = go . toList where
  go xps = do
    st0 <- P get
    goNext st0 Empty xps
  goNext st0 !errs = \case
    [] -> errP (ReasonAlt errs)
    (x, p):xps' ->
      let (ea, st1) = runP p st0
      in case ea of
        Left err -> goNext st0 (errs :|> (x, err)) xps'
        Right a -> a <$ P (put st1)

greedyP :: P e a -> P e (Seq a)
greedyP p = go Empty where
  go !acc = do
    ma <- optP p
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

greedy1P :: P e a -> P e (Seq a)
greedy1P p = liftA2 (:<|) p (greedyP p)

expectP :: Text -> P e ()
expectP n = do
  o <- takeP (T.length n)
  if n == o
    then pure ()
    else errP (ReasonExpect n o)

infixP :: Text -> P e a -> P e b -> P e (a, b)
infixP n pa pb = go where
  go = if T.null n
    then errP ReasonEmptyInfix
    else do
      st0 <- P get
      goNext st0 Empty (T.breakOnAll n (stHay st0))
  goNext st0 !eacc = \case
    [] -> errP (ReasonInfix n eacc)
    (h1, h2):rest -> do
      let r = stRange st0
          e1 = rangeStart r + T.length h1
          st1 = st0 { stHay = h1, stRange = r { rangeEnd = e1 } }
          st2 = st0 { stHay = h2, stRange = r { rangeStart = e1 + T.length n } }
      case fst (runP (pa <* endP) st1) of
        Left errA -> goNext st0 (eacc :|> (e1, SideLeft, errA)) rest
        Right a -> do
          let (ea2, st3) = runP pb st2
          case ea2 of
            Left errB -> goNext st0 (eacc :|> (e1, SideRight, errB)) rest
            Right b -> (a, b) <$ P (put st3)

takeP :: Int -> P e Text
takeP i = P $ state $ \st ->
  let h = stHay st
      (o, h') = T.splitAt i h
      r = stRange st
      r' = r { rangeStart = rangeStart r + T.length o }
  in (o, st { stHay = h', stRange = r' })

-- takeExactP :: Int -> P e Text

dropP :: Int -> P e Int
dropP = fmap T.length . takeP

-- dropExactP :: Int -> P e ()

takeWhileP :: (Char -> Bool) -> P e Text
takeWhileP f = P $ state $ \st ->
  let h = stHay st
      o = T.takeWhile f h
      l = T.length o
      h' = T.drop l h
      r = stRange st
      r' = r { rangeStart = rangeStart r + l }
  in (o, st { stHay = h', stRange = r' })

-- takeWhile1P :: (Char -> Bool) -> P e Text

dropWhileP :: (Char -> Bool) -> P e Int
dropWhileP = fmap T.length . takeWhileP

-- dropWhile1P :: (Char -> Bool) -> P e Int

betweenP :: P e x -> P e y -> P e a -> P e a
betweenP px py pa = px *> pa <* py

sepByP :: P e x -> P e a -> P e (Seq a)
sepByP c p = go where
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

data Value = ValueNull | ValueString !Text | ValueArray !(Seq Value) | ValueObject !(Seq (Text, Value))
  deriving stock (Eq, Ord, Show)

jsonParser :: P Void Value
jsonParser = valP where
  spaceP = void (dropWhileP isSpace)
  valP = spaceP *> rawValP <* spaceP
  rawValP = altP
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

