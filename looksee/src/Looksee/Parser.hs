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
  , transP
  , scopeP
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
  , textP
  , textP_
  , charP
  , charP_
  , breakP
  -- , someBreakP
  -- , splitP
  -- , split1P
  -- , split2P
  -- , leadP
  -- , lead1P
  -- , trailP
  -- , trail1P
  -- , infixRP
  -- , someInfixRP
  )
where

import Control.Applicative (Alternative (..))
import Control.Foldl (FoldM (..))
import Control.Monad (ap, void, when)
import Control.Monad.Except (Except, ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..), StateT, modify', evalStateT, gets)
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Maybe (isNothing)
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
import GHC.Float (double2Float)

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

type ElemP e m a = Elem e m a (ParserT e m a)

type ParserT :: Type -> (Type -> Type) -> Type -> Type
newtype ParserT e m a = ParserT {unParserT :: forall r. (ElemP e m a -> T e m r) -> St -> T e m r}

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

-- | parse
catchP :: (Monad m) => ParserT e m a -> (AltPhase -> Err e -> ParserT e m a) -> ParserT e m a
catchP p0 f = go p0 where
  go (ParserT p) = ParserT $ \k st -> do
    el <- p pure st
    case el of
      ElemPure _ _ -> catchError (k el) (\e -> let ParserT q = f AltPhaseCont e in q k st)
      ElemErr e _ -> let ParserT q = f AltPhaseBranch e in q k st
      ElemCont jz jt -> k (ElemCont (fmap go jz) (fmap go . jt))

-- | Parse with many possible branches
altP :: (Monad m) => [ParserT e m a] -> ParserT e m a
altP = go Empty where
  go !errs = \case
    [] -> errP (if Seq.null errs then ReasonEmpty else ReasonAlt errs)
    p : ps -> catchP p (\phase err -> go (errs :|> (phase, err)) ps)

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
  -> ElemP e m a
  -> ElemP e m b
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

-- | Parse with some local state
scopeP :: (Monad m) => s -> ParserT e (StateT s m) a -> ParserT e m a
scopeP s0 = transP (`evalStateT` s0)

-- private
waitingP :: ParserT e m Bool
waitingP = getsP (isNothing . boundsEndOffset . stBounds)

-- private
loopP :: Functor m => ParserT e m Bool -> ParserT e m ()
loopP p = go where
  go = do
    continue <- p
    when continue $ do
      waiting <- waitingP
      when waiting go

-- -- private
-- loopP :: Functor m => ParserT e m (Maybe a) -> ParserT e m (Maybe a)
-- loopP p = go where
--   go = do
--     ma <- p
--     case ma of
--       Nothing -> do
--         waiting <- waitingP
--         if waiting
--           then go
--           else pure Nothing
--       Just _ -> pure ma

data TakeS = TakeS !Int !TLB.Builder

-- | Take the given number of characters from the start of the range, or fewer if empty
takeP :: (Monad m) => Int -> ParserT e m Text
takeP n0 = scopeP (TakeS n0 mempty) $ do
  loopP $ do
    TakeS n b <- get
    t <- stateP (bufStateSt (TL.splitAt (fromIntegral n)))
    let n' = n - T.length t
        b' = b <> TLB.fromText t
    put (TakeS n' b')
    pure (n' > 0)
  gets (\(TakeS _ b) -> TL.toStrict (TLB.toLazyText b))

-- | Take exactly the given number of characters from the start of the range,
-- throwing error if insufficient input
takeExactP :: (Monad m) => Int -> ParserT e m Text
takeExactP n0 = do
  t <- takeP n0
  let l = T.length t
  if l == n0
    then pure t
    else errP (ReasonDemand n0 l)

-- | Takes exactly 1 character from the start of the range, throwing error
-- if at end of input
headP :: (Monad m) => ParserT e m Char
headP = fmap T.head (takeExactP 1)

-- | Takes exactly 1 character from the start of the range, returning Nothing
-- if at end of input
unconsP :: (Monad m) => ParserT e m (Maybe Char)
unconsP = fmap (fmap fst . T.uncons) (takeP 1)

-- | Drop the given number of characters from the start of the range, or fewer if empty
dropP :: (Monad m) => Int -> ParserT e m Int
dropP = fmap T.length . takeP

-- | Drop exactly the given number of characters from the start of the range, or error
dropExactP :: (Monad m) => Int -> ParserT e m ()
dropExactP = void . takeExactP

-- | Take characters from the start of the range satisfying the predicate
takeWhileP :: Monad m => (Char -> Bool) -> ParserT e m Text
takeWhileP f = scopeP mempty $ do
  loopP $ do
    x <- stateP (bufStateSt (TL.span f))
    modify' (<> TLB.fromText x)
    getsP (TL.null . stBuf)
  gets (TL.toStrict . TLB.toLazyText)

-- | Like 'takeWhileP' but ensures at least 1 character has been taken
takeWhile1P :: (Monad m) => (Char -> Bool) -> ParserT e m Text
takeWhile1P f = do
  txt <- takeWhileP f
  if T.null txt
    then errP ReasonTakeNone
    else pure txt

-- | Drop characters from the start of the range satisfying the predicate
dropWhileP :: (Monad m) => (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

-- | Like 'dropWhileP' but ensures at least 1 character has been dropped
dropWhile1P :: (Monad m) => (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

finalizing
  :: (St -> ElemP e m a) -> ParserT e m a
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

-- | Expect the given text at the start of the range
textP :: (Monad m) => Text -> ParserT e m Text
textP expected = do
  actual <- takeP (T.length expected)
  if actual == expected
    then pure expected
    else errP (ReasonExpect expected actual)

-- | Saves you from importing 'void'
textP_ :: (Monad m) => Text -> ParserT e m ()
textP_ = void . textP

-- | Expect the given character at the start of the range
charP :: (Monad m) => Char -> ParserT e m Char
charP = fmap T.head . textP . T.singleton

-- | Saves you from importing 'void'
charP_ :: (Monad m) => Char -> ParserT e m ()
charP_ = void . charP

-- -- private
-- handleBreak :: TL.Text -> St -> TL.Text -> TL.Text -> (St, St, St)
-- handleBreak needle (St hay bounds@(Bounds start _) labs) hayA rest =
--   let hayX = TL.drop (TL.length hayA) hay
--       hayB = TL.drop (TL.length needle) rest
--       aLen = fromIntegral (TL.length hayA)
--       endA = pointOffset start + aLen
--       rngA = Bounds start (Just endA)
--       rngX = foldBoundsLazy hayA bounds
--       rngB = foldBoundsLazy needle rngX
--       stA = St hayA rngA labs
--       stX = St hayX rngX labs
--       stB = St hayB rngB labs
--   in  (stA, stX, stB)
--
-- -- Returns first possible break point with positions
-- -- (startStream, breakPt) (breakPt, endStream) (breakPt + needLen, endStream)
-- breakRP :: TL.Text -> St -> Maybe (St, St, St)
-- breakRP needle st@(St hay _ _) =
--   let (hayA, rest) = TL.breakOn needle hay
--   in  if TL.null rest
--         then Nothing
--         else Just (handleBreak needle st hayA rest)
--
-- -- Returns list of possible break points with positions
-- -- (startStream, breakPt) (breakPt, endStream) (breakPt + needLen, endStream)
-- breakAllRP :: TL.Text -> St -> [(St, St, St)]
-- breakAllRP needle st@(St hay _ _) =
--   fmap (uncurry (handleBreak needle st)) (TL.breakOnAll needle hay)
--
-- -- | Split once on the delimiter (first argument), parsing everything before it with a narrowed range.
-- -- Chooses first split from START to END of range (see 'infixRP').
-- breakP :: (Monad m) => Text -> ParserT e m a -> ParserT e m a
-- breakP tx pa = fmap fst (infixRP tx pa (pure ()))

data BreakS = BreakS !Int !TLB.Builder !(Maybe (St, St))

-- | Split once on the delimiter (first argument), parsing everything before it with a narrowed range.
-- Chooses first split from START to END of range (see 'infixRP').
breakP :: (Monad m) => Text -> ParserT e m a -> ParserT e m a
breakP tx p = do
  let txLazy = TL.fromStrict tx
      txLen = fromIntegral (T.length tx)
  St _ (Bounds start0 _) labs <- getP
  msts <- scopeP (BreakS 0 mempty Nothing) $ do
    loopP $ do
      (pre, post) <- getsP (TL.breakOn txLazy . stBuf)
      if TL.null post
        then do
          bpre <- stateP (bufStateSt (\buf -> TL.splitAt (TL.length buf - txLen + 1) buf))
          modify' (\(BreakS s b _) -> BreakS (s + T.length bpre) (b <> TLB.fromText bpre) Nothing)
          pure True
        else do
          stB <- getsP (snd . bufStateSt (TL.splitAt txLen))
          modify' $ \(BreakS s b _) ->
            let s' = s + fromIntegral (TL.length pre)
                b' = b <> TLB.fromLazyText pre
                bufA = TLB.toLazyText b'
                boundsA = Bounds start0 (Just s)
                stA = St bufA boundsA labs
            in BreakS s' b' (Just (stA, stB))
          pure False
    gets (\(BreakS _ _ msts) -> msts)
  case msts of
    Nothing -> undefined -- not found error
    Just (stA, stB) -> ParserT $ \k _ -> do
      -- eh something here
      a <- finishParseT (p <* endP) stA
      k (ElemPure a stB)

    --   (pre, post) <- getsP (TL.breakOn txLazy . stBuf)
    --   if TL.null post
    --     then do
    --       bpre <- stateP (bufStateSt (\buf -> TL.splitAt (TL.length buf - txLen + 1) buf))
    --       modify' (\(BreakS b ma) -> BreakS (b <> TLB.fromText bpre) ma)
    --       pure True
    --     else do
    --       bpre <- state $ \(BreakS b ma) ->
    --         let b' = b <> TLB.fromLazyText pre
    --         in (TLB.toLazyText b', BreakS b' ma)
    --       st' =
    -- undefined

-- -- | Split once on the delimiter (first argument), parsing everything before it with a narrowed range.
-- -- Chooses splits from START to END of range (see 'someInfixRP').
-- someBreakP :: (Monad m) => Text -> ParserT e m a -> ParserT e m a
-- someBreakP tx pa = fmap fst (someInfixRP tx pa (pure ()))
--
-- -- private
-- splitTailP :: (Monad m) => Text -> ParserT e m a -> Seq a -> St -> ParserT e m (Seq a)
-- splitTailP tx pa = go
--  where
--   go !acc !st = do
--     mz <- optInfixRP tx pa (pure ())
--     case mz of
--       Nothing -> optP pa >>= maybe (acc <$ putP st) (pure . (acc :|>))
--       Just (a, st', _) -> go (acc :|> a) st'
--
-- -- | Split on the delimiter, parsing segments with a narrowed range, until parsing fails.
-- -- Returns the sequence of successes with state at the delimiter preceding the failure (or end of input),
-- -- Note that this will always succeed, sometimes consuming no input and yielding empty results.
-- splitP :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
-- splitP tx pa = getP >>= splitTailP tx pa Empty
--
-- -- | Like 'splitP' but ensures the sequence is at least length 1.
-- split1P :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
-- split1P tx pa = do
--   mz <- optInfixRP tx pa (pure ())
--   case mz of
--     Nothing -> fmap Seq.singleton pa
--     Just (a, st', _) -> splitTailP tx pa (Seq.singleton a) st'
--
-- -- | Like 'splitP' but ensures the sequence is at least length 2.
-- -- (This ensures there is at least one delimiter included.)
-- split2P :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
-- split2P tx pa = do
--   a0 <- someBreakP tx pa
--   mz <- optInfixRP tx pa (pure ())
--   case mz of
--     Nothing -> fmap (Empty :|> a0 :|>) pa
--     Just (a1, st', _) -> splitTailP tx pa (Empty :|> a0 :|> a1) st'
--
-- -- | Like 'splitP' but ensures a leading delimiter
-- leadP :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
-- leadP tx pa = do
--   mu <- optP (textP tx)
--   case mu of
--     Nothing -> pure Empty
--     Just _ -> split1P tx pa
--
-- -- | Like 'split1P' but ensures a leading delimiter
-- lead1P :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
-- lead1P tx pa = textP tx >> split1P tx pa
--
-- -- | Like 'splitP' but ensures a trailing delimiter
-- trailP :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
-- trailP tx pa = do
--   as <- splitP tx pa
--   case as of
--     Empty -> pure Empty
--     _ -> as <$ textP tx
--
-- -- | Like 'split1P' but ensures a trailing delimiter
-- trail1P :: (Monad m) => Text -> ParserT e m a -> ParserT e m (Seq a)
-- trail1P tx pa = split1P tx pa <* textP tx
--
-- -- private
-- subInfixP
--   :: (Monad m)
--   => St
--   -> ParserT e m a
--   -> ParserT e m b
--   -> (ElemP e m (Maybe (a, St, b)) -> T e m r)
--   -> [(St, St, St)]
--   -> T e m r
-- subInfixP = undefined
--
-- -- subInfixP st0 pa pb j = go Empty
-- --  where
-- --   go !errs = \case
-- --     [] -> do
-- --       put st0
-- --       case errs of
-- --         Empty -> j (Right Nothing)
-- --         _ -> mkErrT (ReasonInfix errs) >>= j . Left
-- --     (stA, stX, stB) : sts -> do
-- --       let startX = spanStart (stSpan stX)
-- --       put stA
-- --       unParserT (pa <* endP) $ \case
-- --         Left errA -> go (errs :|> (startX, InfixPhaseLeft, errA)) sts
-- --         Right a -> do
-- --           put stB
-- --           unParserT pb $ \case
-- --             Left errB -> go (errs :|> (startX, InfixPhaseRight, errB)) sts
-- --             Right b -> do
-- --               ec <- tryT (j (Right (Just (a, stX, b))))
-- --               case ec of
-- --                 Left errC -> go (errs :|> (startX, InfixPhaseCont, errC)) sts
-- --                 Right c -> pure c
--
-- -- private
-- optInfixRP :: (Monad m) => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (Maybe (a, St, b))
-- optInfixRP tx pa pb = ParserT (\k st -> subInfixP st pa pb (optInfixFn k) (breakAllRP (TL.fromStrict tx) st))
--
-- -- private
-- optInfixFn
--   :: (ElemP e m (Maybe (a, St, b)) -> T e m r)
--   -> (ElemP e m (Maybe (a, St, b)) -> T e m r)
-- optInfixFn = undefined
--
-- -- optInfixFn k e = case e of
-- --   Right _ -> k e
-- --   Left _ -> k (Right Nothing)
--
-- -- private
-- requireInfixFn
--   :: (Monad m)
--   => (ElemP e m (a, b) -> T e m r)
--   -> (ElemP e m (Maybe (a, St, b)) -> T e m r)
-- requireInfixFn = undefined
-- -- requireInfixFn k = \case
-- --   Right mxab ->
-- --     case mxab of
-- --       Nothing -> mkErrT ReasonEmpty >>= k . Left
-- --       Just (a, _, b) -> k (Right (a, b))
-- --   Left e -> k (Left e)
--
-- -- | Right-associative infix parsing. Searches for the operator from START to END of range,
-- -- trying only the first break point.
-- infixRP :: (Monad m) => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (a, b)
-- infixRP tx pa pb = ParserT (\k st -> subInfixP st pa pb (requireInfixFn k) (maybeToList (breakRP (TL.fromStrict tx) st)))
--
-- -- | Right-associative infix parsing. Searches for the operator from START to END of range,
-- -- trying subsequent break points until success.
-- someInfixRP :: (Monad m) => Text -> ParserT e m a -> ParserT e m b -> ParserT e m (a, b)
-- someInfixRP tx pa pb = ParserT (\k st -> subInfixP st pa pb (requireInfixFn k) (breakAllRP (TL.fromStrict tx) st))
