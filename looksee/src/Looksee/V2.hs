{-# LANGUAGE UndecidableInstances #-}

module Looksee.V2 where

import Control.Foldl (FoldM (..))
import Control.Monad (ap)
import Control.Monad.Except
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict (MonadWriter (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Looksee.Base

newtype ParserT e m a = ParserT {unParserT :: forall r. (Elem e a (ParserT e m a) -> T e m r) -> T e m r}

instance Functor (ParserT e m) where
  fmap f = go
   where
    go (ParserT p) = ParserT (\k -> p (k . bimap f go))

instance Applicative (ParserT e m) where
  pure a = ParserT (\k -> k (ElemPure a))
  (<*>) = ap

instance Monad (ParserT e m) where
  ParserT p >>= f = ParserT (p . pbind f)

-- private
pbind :: (a -> ParserT e m b) -> (Elem e b (ParserT e m b) -> T e m r) -> Elem e a (ParserT e m a) -> T e m r
pbind f k = go
 where
  go = \case
    ElemPure a -> let ParserT q = f a in q k
    ElemCont mm md -> k (ElemCont (mm >>= f) (md >>= f))

-- private
pnext :: (Monad m) => ParserT e m a -> T e m (Elem e a (ParserT e m a))
pnext (ParserT p) = p pure

type Parser e = ParserT e Identity

-- parseT :: (Monad m) => ParserT e m a -> Text -> m (Either (Err e) a)
-- parseT p h =
--  let h' = TL.fromStrict h
--  in fmap fst (finishParserT (p <* endP) (St h' (textSpan h) Empty))

-- -- | Run a parser (see 'parseT')
-- parse :: Parser e a -> Text -> Either (Err e) a
-- parse p h = runIdentity (parseT p h)

foldParseT :: (Monad m) => ParserT e m a -> FoldM (T e m) Text a
foldParseT m0 = FoldM step initial extract
 where
  step e t =
    if T.null t
      then pure e
      else case e of
        ElemPure _ -> e <$ modify' (\st -> st {stBuf = stBuf st <> TL.fromStrict t})
        ElemCont mt _ -> pnext mt
  initial = pnext m0
  extract = \case
    ElemPure a -> pure a
    ElemCont _ (ParserT pd) -> pd extract

data Elem e a r
  = ElemPure !a
  | -- | (More cont, done cont)
    ElemCont r r
  deriving stock (Functor)

instance Bifunctor (Elem e) where
  bimap f g = \case
    ElemPure a -> ElemPure (f a)
    ElemCont km kd -> ElemCont (g km) (g kd)

-- private
-- Parser state
data St = St
  { stBuf :: !TL.Text
  , stBounds :: !Bounds
  , stLabels :: !(Seq Label)
  }
  deriving stock (Eq, Ord, Show)

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
mkErrT re = gets (\st -> Err (ErrF (stBounds st) re))

-- private
tryT :: (Monad m) => T e m r -> T e m (Either (Err e) r)
tryT t = get >>= \st -> lift (runT t st) >>= \(er, st') -> er <$ put st'
