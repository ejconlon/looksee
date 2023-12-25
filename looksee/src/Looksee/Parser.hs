{-# LANGUAGE UndecidableInstances #-}

module Looksee.Parser
  ( ParserT
  , Parser
  , parseT
  , parse
  , parseIncT
  , parseInc
  , spanP
  , lengthP
  , throwP
  , emptyP
  , endP
  , optP
  , altP
  , repeatP
  , repeat1P
  , lookP
  , labelP
  , textP
  , textP_
  , charP
  , charP_
  , headP
  , unconsP
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
  , transP
  )
where

import Control.Applicative (Alternative (..))
import Control.Foldl (FoldM (..))
import Control.Monad (ap, void)
import Control.Monad.Except (Except, ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Looksee.Types
  ( AltPhase (..)
  , Bounds (..)
  , Err (..)
  , ErrF (..)
  , Label
  , Point (..)
  , Reason (..)
  , Span (..)
  , foldBounds
  , initBounds
  , textBounds
  )

-- private
data St = St
  { stBuf :: !TL.Text
  , stBounds :: !Bounds
  , stLabels :: !(Seq Label)
  }
  deriving stock (Eq, Ord, Show)

-- private
initSt :: St
initSt = St TL.empty initBounds Empty

-- private
textSt :: Text -> St
textSt t = St (TL.fromStrict t) (textBounds t) Empty

-- private
sealSt :: St -> St
sealSt st@(St buf bounds@(Bounds (Point startOff _ _) mayEnd) _) =
  case mayEnd of
    Just _ -> st
    Nothing ->
      let end' = startOff + fromIntegral (TL.length buf)
          bounds' = bounds {boundsEndOffset = Just end'}
      in  st {stBounds = bounds'}

-- private
bufAddSt :: St -> Text -> St
bufAddSt st t = st {stBuf = stBuf st <> TL.fromStrict t}

-- private
bufStateSt :: (TL.Text -> (TL.Text, TL.Text)) -> St -> (Text, St)
bufStateSt f st =
  let (ltxt, buf') = f (stBuf st)
      txt = TL.toStrict ltxt
      bounds' = foldBounds txt (stBounds st)
      st' = st {stBuf = buf', stBounds = bounds'}
  in  (txt, st')

type T e = ExceptT (Err e)

-- private
data Susp r = Susp !r !St
  deriving stock (Functor)

-- private
data Elem e m a r
  = ElemPure !a !St
  | ElemErr !(Err e) !St
  | ElemCont (Susp r) (Text -> Susp r)
  deriving stock (Functor)

instance (Functor m) => Bifunctor (Elem e m) where
  bimap f g = \case
    ElemPure a st -> ElemPure (f a) st
    ElemErr e st -> ElemErr e st
    ElemCont jz jt -> ElemCont (fmap g jz) (fmap g . jt)

-- private
mkErr :: Reason e (Err e) -> St -> Err e
mkErr re st = Err (ErrF (stBounds st) re)

-- private
mkElemErr :: Reason e (Err e) -> St -> Elem e m a r
mkElemErr re st = ElemErr (mkErr re st) st

type ParserT :: Type -> (Type -> Type) -> Type -> Type
newtype ParserT e m a = ParserT {unParserT :: forall r. (Elem e m a (ParserT e m a) -> T e m r) -> St -> T e m r}

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
    ElemCont jz jt -> k (ElemCont (fmap (>>= f) jz) (fmap (>>= f) . jt))

type Parser e = ParserT e Identity

instance MonadTrans (ParserT e) where
  lift ma = ParserT (\k st -> lift ma >>= \a -> k (ElemPure a st))

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

instance (Monad m) => Alternative (ParserT e m) where
  empty = emptyP
  p1 <|> p2 = altP [p1, p2]
  many = fmap toList . repeatP
  some = fmap toList . repeat1P

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
errP re = ParserT (\k -> k . mkElemErr re)

-- private
leftoverP :: ParserT e m (Maybe Int)
leftoverP = getsP (\st -> let Bounds s me = stBounds st in fmap (\e -> e - pointOffset s) me)

-- private
finishParseT :: (Monad m) => ParserT e m a -> St -> T e m a
finishParseT (ParserT p) st = flip p st $ \case
  ElemPure a _ -> pure a
  ElemErr e _ -> throwError e
  ElemCont (Susp q st') _ -> finishParseT q st'

-- | Run a parser transformer. You must consume all input or this will error!
-- If you really don't care about the rest of the input, you can always
-- discard it with 'dropAllP'.
parseT :: (Monad m) => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p = runExceptT . finishParseT (p <* endP) . textSt

-- | Run a parser (see 'parseT')
parse :: Parser e a -> Text -> Either (Err e) a
parse p = runIdentity . parseT p

-- -- | Run a parser and print any errors that occur
-- parseI :: (HasErrMessage e) => Parser e a -> Text -> IO (Either (Err e) a)
-- parseI p h = do
--   let ea = parse p h
--   case ea of
--     Left e -> printE "<interactive>" h e
--     Right _ -> pure ()
--   pure ea

parseIncT :: (Monad m) => ParserT e m a -> FoldM (ExceptT (Err e) m) Text a
parseIncT (ParserT p0) = FoldM step initial extract
 where
  step el t =
    if T.null t
      then pure el
      else case el of
        ElemPure a st -> pure (ElemPure a (bufAddSt st t))
        ElemErr e st -> pure (ElemErr e (bufAddSt st t))
        ElemCont _ jt -> let (Susp (ParserT p) st) = jt t in p pure st
  initial = p0 pure initSt
  extract = \case
    ElemPure a _ -> pure a
    ElemErr e _ -> throwError e
    ElemCont (Susp (ParserT p) st) _ -> p extract (sealSt st)

parseInc :: Parser e a -> FoldM (Except (Err e)) Text a
parseInc = parseIncT

-- | Return the consumed span along with the result
spanP :: (Functor m) => ParserT e m a -> ParserT e m (a, Span)
spanP p = do
  Bounds start _ <- getsP stBounds
  a <- p
  Bounds end _ <- getsP stBounds
  pure (a, Span start end)

-- | Return the consumed length along with the result
lengthP :: (Functor m) => ParserT e m a -> ParserT e m (a, Int)
lengthP p = do
  Bounds (Point startOff _ _) _ <- getsP stBounds
  a <- p
  Bounds (Point endOff _ _) _ <- getsP stBounds
  pure (a, endOff - startOff)

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

-- | Makes parse success optional
optP :: ParserT e m a -> ParserT e m (Maybe a)
optP (ParserT p) = ParserT $ \k st -> flip p st $ \case
  ElemPure a st' -> k (ElemPure (Just a) st')
  ElemErr _ _ -> k (ElemPure Nothing st)
  ElemCont jz jt -> k (ElemCont (fmap optP jz) (fmap optP . jt))

-- | Fail with no results
emptyP :: ParserT e m a
emptyP = errP ReasonEmpty

-- private
subAltP
  :: (Monad m)
  => (Elem e m a (ParserT e m a) -> T e m r)
  -> St
  -> Seq (AltPhase, Err e)
  -> [ParserT e m a]
  -> T e m r
subAltP k0 st0 = onLoop
 where
  onLoop !errs = \case
    [] -> k0 (mkElemErr (if Seq.null errs then ReasonEmpty else ReasonAlt errs) st0)
    p : rest -> onParser errs rest p
  onParser errs rest (ParserT p) = p (onElem errs rest) st0
  onElem errs rest = \case
    ElemErr e _ -> onLoop (errs :|> (AltPhaseBranch, e)) rest
    el -> catchError (k0 el) (\e -> onLoop (errs :|> (AltPhaseCont, e)) rest)

-- | Parse with many possible branches
altP :: (Monad m) => [ParserT e m a] -> ParserT e m a
altP ps = ParserT (\k0 st0 -> subAltP k0 st0 Empty ps)

-- private
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
mapElem
  :: (a -> St -> Elem e m b (ParserT e m b))
  -> (Err e -> St -> Elem e m b (ParserT e m b))
  -> Elem e m a (ParserT e m a)
  -> Elem e m b (ParserT e m b)
mapElem onPure onErr = onElem
 where
  onParser (ParserT p) = ParserT (\k st -> p (k . onElem) st)
  onElem = \case
    ElemPure a st' -> onPure a st'
    ElemErr e st' -> onErr e st'
    ElemCont jz jt -> ElemCont (onSusp jz) (onSusp . jt)
  onSusp = fmap onParser

-- | Lookahead - rewinds state if the parser succeeds, otherwise throws error
lookP :: ParserT e m a -> ParserT e m a
lookP (ParserT p0) = ParserT $ \k0 st0 ->
  let onPure a _ = ElemPure a st0
      onErr e _ = ElemErr (mkErr (ReasonLook e) st0) st0
  in  p0 (k0 . mapElem onPure onErr) st0

-- | Labels parse errors
labelP :: Label -> ParserT e m a -> ParserT e m a
labelP lab (ParserT p) = ParserT $ \k st ->
  p (k . mapElem ElemPure (mkElemErr . ReasonLabeled lab)) st

-- | Expect the given text at the start of the range
textP :: (Functor m) => Text -> ParserT e m Text
textP expected = do
  actual <- takeP (T.length expected)
  if actual == expected
    then pure expected
    else errP (ReasonExpect expected actual)

-- | Saves you from importing 'void'
textP_ :: (Functor m) => Text -> ParserT e m ()
textP_ = void . textP

-- | Expect the given character at the start of the range
charP :: (Functor m) => Char -> ParserT e m Char
charP = fmap T.head . textP . T.singleton

-- | Saves you from importing 'void'
charP_ :: (Functor m) => Char -> ParserT e m ()
charP_ = void . charP

-- private
retrying
  :: (St -> Elem e m a (ParserT e m a)) -> (St -> Maybe (Elem e m a (ParserT e m a))) -> ParserT e m a
retrying onTrunc onMore = retMore
 where
  retTrunc = ParserT (. onTrunc)
  retMore = ParserT $ \k st ->
    k $
      let Bounds _ mayEnd = stBounds st
      in  case mayEnd of
            Just _ -> onTrunc st
            Nothing -> case onMore st of
              Nothing ->
                ElemCont
                  (Susp retTrunc st)
                  (Susp retMore . bufAddSt st)
              Just el -> el

-- | Take the given number of characters from the start of the range, or fewer if empty
takeP :: (Functor m) => Int -> ParserT e m Text
takeP len = ret
 where
  ret =
    if len <= 0
      then pure T.empty
      else retrying onTrunc onMore
  onTrunc !st =
    let (txt, st') = bufStateSt (TL.splitAt (fromIntegral len)) st
    in  ElemPure txt st'
  onMore st =
    let need = len - fromIntegral (TL.length (stBuf st))
    in  if need > 0
          then Nothing
          else Just (onTrunc st)

-- | Take exactly the given number of characters from the start of the range,
-- throwing error if insufficient input
takeExactP :: (Functor m) => Int -> ParserT e m Text
takeExactP len = ret
 where
  ret =
    if len <= 0
      then pure T.empty
      else retrying onTrunc onMore
  onTrunc !st =
    let have = fromIntegral (TL.length (stBuf st))
    in  if len - have > 0
          then mkElemErr (ReasonDemand len have) st
          else
            let (txt, st') = bufStateSt (TL.splitAt (fromIntegral len)) st
            in  ElemPure txt st'
  onMore st =
    let need = len - fromIntegral (TL.length (stBuf st))
    in  if need > 0
          then Nothing
          else Just (onTrunc st)

-- | Takes exactly 1 character from the start of the range, throwing error
-- if at end of input
headP :: (Functor m) => ParserT e m Char
headP = fmap T.head (takeExactP 1)

-- | Takes exactly 1 character from the start of the range, returning Nothing
-- if at end of input
unconsP :: (Functor m) => ParserT e m (Maybe Char)
unconsP = fmap (fmap fst . T.uncons) (takeP 1)

-- | Drop the given number of characters from the start of the range, or fewer if empty
dropP :: (Functor m) => Int -> ParserT e m Int
dropP = fmap T.length . takeP

-- | Drop exactly the given number of characters from the start of the range, or error
dropExactP :: (Functor m) => Int -> ParserT e m ()
dropExactP = void . takeExactP

-- private
retryingWith
  :: (Functor m)
  => (s -> St -> Elem e m (a, s) (ParserT e m (a, s)))
  -> (s -> St -> Maybe (Elem e m (a, s) (ParserT e m (a, s))))
  -> s
  -> ParserT e m a
retryingWith onTrunc onMore s0 = fmap fst (retMore s0)
 where
  retTrunc s = ParserT (. onTrunc s)
  retMore s = ParserT $ \k st ->
    k $
      let Bounds _ mayEnd = stBounds st
      in  case mayEnd of
            Just _ -> onTrunc s st
            Nothing -> case onMore s st of
              Nothing ->
                ElemCont
                  (Susp (retTrunc s) st)
                  (Susp (retMore s) . bufAddSt st)
              Just el -> el

-- | Take characters from the start of the range satisfying the predicate
takeWhileP :: (Functor m) => (Char -> Bool) -> ParserT e m Text
takeWhileP f = retryingWith onTrunc onMore mempty
 where
  onTrunc s st =
    let (match, st') = bufStateSt (TL.span f) st
        s' = s <> TLB.fromText match
        res = TL.toStrict (TLB.toLazyText s)
    in  ElemPure (res, s') st'
  onMore _s _st = error "TODO"

-- | Like 'takeWhileP' but ensures at least 1 character has been taken
takeWhile1P :: (Functor m) => (Char -> Bool) -> ParserT e m Text
takeWhile1P f = do
  txt <- takeWhileP f
  if T.null txt
    then errP (ReasonDemand 1 0)
    else pure txt

-- | Drop characters from the start of the range satisfying the predicate
dropWhileP :: (Functor m) => (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

-- | Like 'dropWhileP' but ensures at least 1 character has been dropped
dropWhile1P :: (Functor m) => (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

finalizing
  :: (St -> Elem e m a (ParserT e m a)) -> ParserT e m a
finalizing onTrunc = retMore
 where
  retTrunc = ParserT (. onTrunc)
  retMore = ParserT $ \k st ->
    k $
      let Bounds _ mayEnd = stBounds st
      in  case mayEnd of
            Just _ -> onTrunc st
            Nothing ->
              ElemCont
                (Susp retTrunc st)
                (Susp retMore . bufAddSt st)

-- | Take the remaining range, leaving it empty
takeAllP :: ParserT e m Text
takeAllP = finalizing $ \st ->
  let (txt, st') = bufStateSt (,TL.empty) st
  in  ElemPure txt st'

-- | Like 'takeAllP' but ensures at least 1 character has been taken
takeAll1P :: (Functor m) => ParserT e m Text
takeAll1P = do
  txt <- takeAllP
  if T.null txt
    then errP (ReasonDemand 1 0)
    else pure txt

-- | Drop the remaining range, leaving it empty
dropAllP :: (Functor m) => ParserT e m Int
dropAllP = fmap T.length takeAllP

-- | Like 'dropAllP' but ensures at least 1 character has been dropped
dropAll1P :: (Functor m) => ParserT e m Int
dropAll1P = fmap T.length takeAll1P

-- | Unwrap a monad transformer layer (see 'scopeP' for use)
transP :: (MonadTrans t, Monad m) => (forall a. t m a -> m a) -> ParserT e (t m) b -> ParserT e m b
transP nat = onParser
 where
  onParser (ParserT p) = ParserT (\k st -> hoist nat (p (hoist lift . k . onElem) st))
  onElem = \case
    ElemPure a st -> ElemPure a st
    ElemErr e st -> ElemErr e st
    ElemCont jz jt -> ElemCont (onSusp jz) (onSusp . jt)
  onSusp = fmap onParser
