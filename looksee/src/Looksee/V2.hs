{-# LANGUAGE UndecidableInstances #-}

module Looksee.V2
  ( ParserT
  , Parser
  )
where

import Control.Foldl (FoldM (..))
import Control.Monad (ap, (>=>))
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, runStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (MonadWriter (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Looksee.Base

-- | Parser state
data St = St
  { stBuf :: !TL.Text
  , stBounds :: !Bounds
  , stLabels :: !(Seq Label)
  }
  deriving stock (Eq, Ord, Show)

initSt :: St
initSt = St TL.empty initBounds Empty

textSt :: Text -> St
textSt t = St (TL.fromStrict t) (textBounds t) Empty

bufAddSt :: St -> Text -> St
bufAddSt st t = st {stBuf = stBuf st <> TL.fromStrict t}

data Susp r = Susp !r !St
  deriving stock (Functor)

-- private
data Elem e m a r
  = ElemPure !a !St
  | ElemErr !(Err e) !St
  | ElemCont (Text -> m (Susp r)) (m (Susp r))
  deriving stock (Functor)

instance (Functor m) => Bifunctor (Elem e m) where
  bimap f g = \case
    ElemPure a st -> ElemPure (f a) st
    ElemErr e st -> ElemErr e st
    ElemCont jt jz -> ElemCont (fmap (fmap g) . jt) (fmap (fmap g) jz)

type ParserT :: Type -> (Type -> Type) -> Type -> Type
newtype ParserT e m a = ParserT {unParserT :: forall r. (Elem e m a (ParserT e m a) -> m r) -> St -> m r}

instance (Functor m) => Functor (ParserT e m) where
  fmap f = go
   where
    go (ParserT p) = ParserT (\k -> p (k . bimap f go))

instance (Functor m) => Applicative (ParserT e m) where
  pure a = ParserT (\k -> k . ElemPure a)
  (<*>) = ap

instance (Functor m) => Monad (ParserT e m) where
  ParserT p >>= f = ParserT $ \k st -> flip p st $ \case
    ElemPure a st' -> let ParserT q = f a in q k st'
    ElemErr e st' -> k (ElemErr e st')
    ElemCont jt jz -> k (ElemCont (fmap (fmap (>>= f)) . jt) (fmap (fmap (>>= f)) jz))

type Parser e = ParserT e Identity

instance MonadTrans (ParserT e) where
  lift ma = ParserT (\k st -> ma >>= \a -> k (ElemPure a st))

instance (MonadIO m) => MonadIO (ParserT e m) where
  liftIO ma = ParserT (\k st -> liftIO ma >>= \a -> k (ElemPure a st))

instance (Functor m) => MonadFail (ParserT e m) where
  fail = errP . ReasonFail . T.pack

instance (MonadReader r m) => MonadReader r (ParserT e m) where
  ask = ParserT (\k st -> ask >>= \r -> k (ElemPure r st))
  local f (ParserT p) = ParserT (\k st -> local f (p k st))

instance (MonadState s m) => MonadState s (ParserT e m) where
  get = ParserT (\k st -> get >>= \s -> k (ElemPure s st))
  put s = ParserT (\k st -> put s >> k (ElemPure () st))
  state f = ParserT (\k st -> state f >>= \a -> k (ElemPure a st))

instance (Functor m, Semigroup a) => Semigroup (ParserT e m a) where
  p <> q = liftA2 (<>) p q

instance (Functor m, Monoid a) => Monoid (ParserT e m a) where
  mempty = pure mempty

-- private
finishParserT :: (Monad m) => ParserT e m a -> St -> m (Either (Err e) a, St)
finishParserT (ParserT p) st = flip p st $ \case
  ElemPure a st' -> pure (Right a, st')
  ElemErr e st' -> pure (Left e, st')
  ElemCont _ jz -> jz >>= \(Susp q st') -> finishParserT q st'

-- private
getP :: ParserT e m St
getP = ParserT (\k st -> k (ElemPure st st))

-- private
getsP :: (St -> a) -> ParserT e m a
getsP f = ParserT (\k st -> k (ElemPure (f st) st))

-- private
putP :: St -> ParserT e m ()
putP st' = ParserT (\k _ -> k (ElemPure () st'))

-- private
stateP :: (St -> (a, St)) -> ParserT e m a
stateP f = ParserT (\k st -> let (a, st') = f st in k (ElemPure a st'))

-- private
errP :: Reason e (Err e) -> ParserT e m a
errP re = ParserT $ \k st -> k (ElemErr (Err (ErrF (stBounds st) re)) st)

-- private
leftoverP :: ParserT e m (Maybe Int)
leftoverP = getsP (\st -> let Bounds s me = stBounds st in fmap (\e -> e - pointOffset s) me)

-- | Run a parser transformer. You must consume all input or this will error!
-- If you really don't care about the rest of the input, you can always
-- discard it with 'dropAllP'.
parseT :: (Monad m) => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p h = fmap fst (finishParserT (p <* endP) (textSt h))

-- | Run a parser (see 'parseT')
parse :: Parser e a -> Text -> Either (Err e) a
parse p h = runIdentity (parseT p h)

-- -- | Run a parser and print any errors that occur
-- parseI :: (HasErrMessage e) => Parser e a -> Text -> IO (Either (Err e) a)
-- parseI p h = do
--   let ea = parse p h
--   case ea of
--     Left e -> printE "<interactive>" h e
--     Right _ -> pure ()
--   pure ea

foldParseT :: (Monad m) => ParserT e m a -> FoldM m Text (Either (Err e) a)
foldParseT (ParserT p0) = FoldM step initial extract
 where
  step el t =
    if T.null t
      then pure el
      else case el of
        ElemPure a st -> pure (ElemPure a (bufAddSt st t))
        ElemErr e st -> pure (ElemErr e (bufAddSt st t))
        ElemCont jt _ -> jt t >>= \(Susp (ParserT p) st) -> p pure st
  initial = p0 pure initSt
  extract = \case
    ElemPure a _ -> pure (Right a)
    ElemErr e _ -> pure (Left e)
    ElemCont _ jz -> jz >>= \(Susp (ParserT p) st) -> p extract st

-- | Get the bounds at the current point representing
-- the entire parseable range.
boundsP :: ParserT e m Bounds
boundsP = getsP stBounds

-- | Incorporate span information into a parsed object.
spanP :: (Functor m) => (Span -> a -> b) -> ParserT e m a -> ParserT e m b
spanP f p = do
  Bounds start _ <- boundsP
  a <- p
  Bounds end _ <- boundsP
  pure (f (Span start end) a)

-- | Throw a custom parse error
throwP :: e -> ParserT e m a
throwP = errP . ReasonCustom

-- | Succeed if this is the end of input
endP :: (Functor m) => ParserT e m ()
endP = do
  ml <- leftoverP
  case ml of
    Just 0 -> pure ()
    _ -> errP (ReasonLeftover ml)

-- -- | Makes parse success optional
-- optP :: Functor m => ParserT e m a -> ParserT e m (Maybe a)
-- optP (ParserT p) = ParserT $ \k st -> flip p st $ \case
--     ElemPure a st' -> k (ElemPure (Just a) st')
--     ElemErr _ _ -> k (ElemPure Nothing st)
--     ElemCont _ _ -> k (ElemCont _ _)
