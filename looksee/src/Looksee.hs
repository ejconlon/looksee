{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A simple text parser with decent errors
module Looksee
  ( Span (..)
  , LineColLookup ()
  , calculateLineCol
  , lookupLineCol
  , Label (..)
  , textSpan
  , Reason (..)
  , ErrF (..)
  , Err (..)
  , errSpan
  , errReason
  , Assoc (..)
  , Prec (..)
  , ParserT
  , Parser
  , GuardedCase
  , pattern GuardedCase
  , GuardedCaseT (..)
  , guarded
  , guardedWith
  , labelG
  , chooseP
  , chooseElseP
  , Op
  , pattern OpPrefix
  , pattern OpPostfix
  , pattern OpInfix
  , OpT (..)
  , PrefixOp
  , pattern PrefixOp
  , PrefixOpT (..)
  , PostfixOp
  , pattern PostfixOp
  , PostfixOpT (..)
  , InfixOp
  , pattern InfixOp
  , InfixOpT (..)
  , PrattTable
  , PrattTableT
  , prattTable
  , prattP
  , prattAtP
  , prattWithTable
  , prattAtWithTable
  , prefixOp
  , prefixOpWith
  , postfixOp
  , postfixOpWith
  , infixOp
  , infixOpWith
  , parseT
  , parse
  , parseI
  , spanP
  , spanAroundP
  , throwP
  , explainP
  , endP
  , labelP
  , textP
  , textP_
  , charP
  , charP_
  , someCharP
  , someCharP_
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
  , optionalP
  , repeatP
  , repeat1P
  , sepByP
  , sepBy1P
  , sepBy2P
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
  , space1P
  , strip1P
  , stripStart1P
  , stripEnd1P
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
import Data.Hashable (Hashable)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
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
import GHC.Generics (Generic)
import System.IO (stderr)

-- | A generic half-open span, used for tracking ranges of offsets or (line, column) positions.
data Span a = Span
  { spanStart :: !a
  -- ^ Inclusive start of the span.
  , spanEnd :: !a
  -- ^ Exclusive end of the span.
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (Hashable)

-- | Auxiliary data structure to translate offsets to (line, col)
type LineColLookup = Vector (Int, Int)

-- | Construct an offset lookup from a document
calculateLineCol :: Text -> LineColLookup
calculateLineCol t = V.unfoldrN (T.length t) go ((0, 0), T.unpack t)
 where
  go (p@(!line, !col), xs) =
    case xs of
      [] -> Nothing
      x : xs' -> Just (p, if x == '\n' then ((line + 1, 0), xs') else ((line, col + 1), xs'))

-- | Returns 0-based (line, col) for the given offset.
-- Clamps to the valid range of offsets, returning (0, 0) for
-- empty text. Note that the valid range is from before the first
-- character to before the last, so a 3 character string has
-- three valid offsets (0, 1, and 2).
lookupLineCol :: Int -> LineColLookup -> (Int, Int)
lookupLineCol i v =
  if V.null v
    then (0, 0)
    else v V.! max 0 (min i (V.length v - 1))

-- | A parser label used in error reporting.
newtype Label = Label
  { unLabel :: Text
  -- ^ Textual label shown in parse errors.
  }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | Create a span from the given text
textSpan :: Text -> Span Int
textSpan t = Span 0 (T.length t)

-- private
-- Parser state
data St = St
  { stHay :: !Text
  , stSpan :: !(Span Int)
  , stLabels :: !(Seq Label)
  }
  deriving stock (Eq, Ord, Show)

-- | Operator associativity for Pratt infix productions.
data Assoc
  = -- | Left-associative infix operator.
    AssocLeft
  | -- | Right-associative infix operator.
    AssocRight
  | -- | Non-associative infix operator; chaining at the same precedence fails.
    AssocNone
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Numeric Pratt precedence. Higher values bind tighter.
newtype Prec = Prec
  { unPrec :: Int
  -- ^ Extract the numeric precedence.
  }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

-- | Whether to hide an underlying error when rendering an explained error.
data HideError
  = -- | Show both the explanation and underlying error.
    HideErrorNo
  | -- | Show only the explanation.
    HideErrorYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Reason for parse failure.
data Reason e r
  = -- | User-supplied custom parse error.
    ReasonCustom !e
  | -- | Expected text and actual text found.
    ReasonExpect !Text !Text
  | -- | Expected item count and actual item count found.
    ReasonDemand !Int !Int
  | -- | Expected end of input, but this many characters remained.
    ReasonLeftover !Int
  | -- | A non-associative Pratt operator was chained at the given precedence.
    ReasonNonAssoc !Int
  | -- | Failure produced by 'MonadFail'.
    ReasonFail !Text
  | -- | Underlying error annotated with a parser label.
    ReasonLabeled !Label r
  | -- | A take/drop parser consumed no input when at least one item was required.
    ReasonTakeNone
  | -- | No parse result was available.
    ReasonEmpty
  | -- | Underlying error annotated with a custom explanation.
    ReasonExplained !Text !HideError r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''Reason
deriveBifoldable ''Reason
deriveBitraversable ''Reason

-- | Base functor for Err containing the range and reason for the error.
data ErrF e r = ErrF
  { efSpan :: !(Span Int)
  -- ^ Character offset span covered by the error.
  , efReason :: !(Reason e r)
  -- ^ Structured reason for the error.
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''ErrF
deriveBifoldable ''ErrF
deriveBitraversable ''ErrF

-- | A parse error, which may contain multiple sub-errors.
newtype Err e = Err
  { unErr :: ErrF e (Err e)
  -- ^ Project one layer of the recursive parse error tree.
  }
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

-- | Span of a parse error
errSpan :: Err e -> Span Int
errSpan = efSpan . unErr

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

deriving newtype instance (MonadReader r m) => MonadReader r (T e m)

deriving newtype instance (MonadWriter w m) => MonadWriter w (T e m)

-- private
runT :: T e m a -> St -> m (Either (Err e) a, St)
runT = runStateT . runExceptT . unT

-- private
mkErrT :: (Monad m) => Reason e (Err e) -> T e m (Err e)
mkErrT re = gets (\st -> Err (ErrF (stSpan st) re))

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

-- | The parser monad
type Parser e = ParserT e Identity

-- | A parser branch selected by an explicit guard parser.
type GuardedCase e = GuardedCaseT e Identity

-- | A parser branch selected by an explicit guard parser in an arbitrary base monad.
data GuardedCaseT e m b where
  GuardedCaseT
    :: ParserT e m a
    -- ^ Guard parser that selects this branch and produces a value for the body.
    -> (a -> ParserT e m b)
    -- ^ Branch body parser, receiving the guard result.
    -> GuardedCaseT e m b

-- | Pattern synonym for guarded cases specialized to 'Parser'.
pattern GuardedCase :: Parser e a -> (a -> Parser e b) -> GuardedCase e b
pattern GuardedCase guard parser = GuardedCaseT guard parser

{-# COMPLETE GuardedCase #-}

instance Functor (GuardedCaseT e m) where
  fmap f (GuardedCaseT guard parser) = GuardedCaseT guard (fmap f . parser)

-- | A user-defined prefix production.
type PrefixOp e = PrefixOpT e Identity

-- | A user-defined prefix production in an arbitrary base monad.
data PrefixOpT e m a where
  PrefixOpT
    :: { prefixPrec :: !Prec
       -- ^ Prefix operator binding precedence.
       , prefixGuard :: ParserT e m z
       -- ^ Guard parser that identifies and consumes the prefix operator.
       , prefixParser :: z -> ParserT e m (a -> a)
       -- ^ Parser for the rest of the prefix production, returning an AST builder.
       }
    -> PrefixOpT e m a

-- | Pattern synonym for prefix productions specialized to 'Parser'.
pattern PrefixOp :: Prec -> Parser e z -> (z -> Parser e (a -> a)) -> PrefixOp e a
pattern PrefixOp prec guard parser = PrefixOpT prec guard parser

{-# COMPLETE PrefixOp #-}

-- | A user-defined postfix production.
type PostfixOp e = PostfixOpT e Identity

-- | A user-defined postfix production in an arbitrary base monad.
data PostfixOpT e m a where
  PostfixOpT
    :: { postfixPrec :: !Prec
       -- ^ Postfix operator binding precedence.
       , postfixGuard :: ParserT e m z
       -- ^ Guard parser that identifies and consumes the postfix operator.
       , postfixParser :: z -> ParserT e m (a -> a)
       -- ^ Parser for the rest of the postfix production, returning an AST builder.
       }
    -> PostfixOpT e m a

-- | Pattern synonym for postfix productions specialized to 'Parser'.
pattern PostfixOp :: Prec -> Parser e z -> (z -> Parser e (a -> a)) -> PostfixOp e a
pattern PostfixOp prec guard parser = PostfixOpT prec guard parser

{-# COMPLETE PostfixOp #-}

-- | A user-defined infix production.
type InfixOp e = InfixOpT e Identity

-- | A user-defined infix production in an arbitrary base monad.
data InfixOpT e m a where
  InfixOpT
    :: { infixAssoc :: !Assoc
       -- ^ Infix operator associativity.
       , infixPrec :: !Prec
       -- ^ Infix operator binding precedence.
       , infixGuard :: ParserT e m z
       -- ^ Guard parser that identifies and consumes the infix operator.
       , infixParser :: z -> ParserT e m (a -> a -> a)
       -- ^ Parser for the rest of the infix production, returning an AST builder.
       }
    -> InfixOpT e m a

-- | Pattern synonym for infix productions specialized to 'Parser'.
pattern InfixOp :: Assoc -> Prec -> Parser e z -> (z -> Parser e (a -> a -> a)) -> InfixOp e a
pattern InfixOp assoc prec guard parser = InfixOpT assoc prec guard parser

{-# COMPLETE InfixOp #-}

-- | A user-defined Pratt production.
type Op e = OpT e Identity

-- | A user-defined Pratt production in an arbitrary base monad.
data OpT e m a
  = -- | Prefix Pratt operator production.
    OpPrefixT !(PrefixOpT e m a)
  | -- | Postfix Pratt operator production.
    OpPostfixT !(PostfixOpT e m a)
  | -- | Infix Pratt operator production.
    OpInfixT !(InfixOpT e m a)

-- | Pattern synonym for prefix Pratt operators specialized to 'Parser'.
pattern OpPrefix :: PrefixOp e a -> Op e a
pattern OpPrefix prefix = OpPrefixT prefix

-- | Pattern synonym for postfix Pratt operators specialized to 'Parser'.
pattern OpPostfix :: PostfixOp e a -> Op e a
pattern OpPostfix postfix = OpPostfixT postfix

-- | Pattern synonym for infix Pratt operators specialized to 'Parser'.
pattern OpInfix :: InfixOp e a -> Op e a
pattern OpInfix infix0 = OpInfixT infix0

{-# COMPLETE OpPrefix, OpPostfix, OpInfix #-}

data ProdsT e m a = ProdsT
  { prodsPrefix :: !(IntMap (Seq (PrefixOpT e m a)))
  , prodsPostfix :: !(IntMap (Seq (PostfixOpT e m a)))
  , prodsInfix :: !(IntMap (Seq (InfixOpT e m a)))
  }

-- | A grouped Pratt operator table specialized to 'Parser'.
type PrattTable e = PrattTableT e Identity

-- | A grouped Pratt operator table in an arbitrary base monad.
newtype PrattTableT e m a = PrattTableT
  { unPrattTableT :: ProdsT e m a
  }

data StepProdT e m a
  = StepPostfixT !(PostfixOpT e m a)
  | StepInfixT !(InfixOpT e m a)

instance MonadTrans (ParserT e) where
  lift ma = ParserT (\j -> lift ma >>= j . Right)

instance (MonadIO m) => MonadIO (ParserT e m) where
  liftIO ma = ParserT (\j -> liftIO ma >>= j . Right)

instance (Monad m) => MonadFail (ParserT e m) where
  fail = errP . ReasonFail . T.pack

instance (MonadReader r m) => MonadReader r (ParserT e m) where
  ask = ParserT (\j -> ask >>= j . Right)
  local f (ParserT g) = ParserT (local f . g)

instance (MonadState s m) => MonadState s (ParserT e m) where
  get = ParserT (\j -> lift get >>= j . Right)
  put s = ParserT (\j -> lift (put s) >>= j . Right)
  state f = ParserT (\j -> lift (state f) >>= j . Right)

instance (Semigroup a) => Semigroup (ParserT e m a) where
  p <> q = liftA2 (<>) p q

instance (Monoid a) => Monoid (ParserT e m a) where
  mempty = pure mempty

-- private
finishParserT :: (Monad m) => ParserT e m a -> St -> m (Either (Err e) a, St)
finishParserT (ParserT g) st =
  let t = g (either throwError pure)
  in  runT t st

-- private
getsP :: (Monad m) => (St -> a) -> ParserT e m a
getsP f = ParserT (\j -> gets f >>= j . Right)

-- private
stateP :: (Monad m) => (St -> (a, St)) -> ParserT e m a
stateP f = ParserT (\j -> state f >>= j . Right)

-- private
errP :: (Monad m) => Reason e (Err e) -> ParserT e m a
errP re = ParserT (\j -> mkErrT re >>= j . Left)

-- private
leftoverP :: (Monad m) => ParserT e m Int
leftoverP = getsP (\st -> let Span s e = stSpan st in e - s)

-- | Run a parser transformer. You must consume all input or this will error!
-- If you really don't care about the rest of the input, you can always
-- discard it with 'dropAllP'.
parseT :: (Monad m) => ParserT e m a -> Text -> m (Either (Err e) a)
parseT p h = fmap fst (finishParserT (p <* endP) (St h (textSpan h) Empty))

-- | Run a parser (see 'parseT')
parse :: Parser e a -> Text -> Either (Err e) a
parse p h = runIdentity (parseT p h)

-- | Run a parser and print any errors that occur
parseI :: (HasErrMessage e) => Parser e a -> Text -> IO (Either (Err e) a)
parseI p h = do
  let ea = parse p h
  case ea of
    Left e -> printE "<interactive>" h e
    Right _ -> pure ()
  pure ea

-- | Get the span (in character offset) at the current point representing
-- the entire parseable range. At the start of parsing this will be `Span 0 n` for
-- an @n@-character document. The start offset will increase as input is consumed,
-- and the end offset will decrease as lookahead delimits the range. To evaluate
-- the "real" range of characters consumed by a parser, construct a span with the
-- starting offsets before and after executing a subparser (or use 'spanAroundP').
spanP :: (Monad m) => ParserT e m (Span Int)
spanP = getsP stSpan

-- | Incorporate span information into a parsed object.
spanAroundP :: (Monad m) => (Span Int -> a -> b) -> ParserT e m a -> ParserT e m b
spanAroundP f p = do
  Span start _ <- spanP
  a <- p
  Span end _ <- spanP
  pure (f (Span start end) a)

-- | Throw a custom parse error
throwP :: (Monad m) => e -> ParserT e m a
throwP = errP . ReasonCustom

-- | Succeed if this is the end of input
endP :: (Monad m) => ParserT e m ()
endP = do
  l <- leftoverP
  if l == 0
    then pure ()
    else errP (ReasonLeftover l)

-- private
guardP :: (Monad m) => ParserT e m a -> ParserT e m (Maybe a)
guardP (ParserT g) = ParserT $ \j -> do
  st0 <- get
  g $ \case
    Left _ -> put st0 >> j (Right Nothing)
    Right a -> j (Right (Just a))

-- private
noChoiceP :: (Monad m) => ParserT e m a
noChoiceP = ParserT (\j -> mkErrT ReasonEmpty >>= j . Left)

-- | If things fail and you can give a good message explaining why, this combinator will
-- annotate the error with your explanation. Returning 'True' with message will hide
-- the original error message in textual rendering.
explainP :: (Monad m) => (Reason e (Err e) -> Maybe (Text, Bool)) -> ParserT e m a -> ParserT e m a
explainP f (ParserT g) = ParserT $ \j -> g $ \ea ->
  case ea of
    Left e@(Err (ErrF _ re)) ->
      case f re of
        Nothing -> j ea
        Just (msg, hide) -> do
          sp <- gets stSpan
          let hide' = if hide then HideErrorYes else HideErrorNo
          let e' = Err (ErrF sp (ReasonExplained msg hide' e))
          j (Left e')
    Right _ -> j ea

-- | Build a guarded branch whose guard result is passed to the body.
guardedWith :: ParserT e m a -> (a -> ParserT e m b) -> GuardedCaseT e m b
guardedWith = GuardedCaseT

-- | Build a guarded branch whose guard only selects the body.
guarded :: ParserT e m () -> ParserT e m b -> GuardedCaseT e m b
guarded guard parser = guardedWith guard (const parser)

-- | Label the body of a guarded branch without labelling its guard.
labelG :: (Monad m) => Label -> GuardedCaseT e m a -> GuardedCaseT e m a
labelG lab (GuardedCaseT guard parser) = GuardedCaseT guard (labelP lab . parser)

-- | Choose the first guarded branch whose guard succeeds.
chooseP :: (Monad m, Foldable f) => f (GuardedCaseT e m a) -> ParserT e m a
chooseP branches = chooseElseP branches noChoiceP

-- | Choose the first guarded branch whose guard succeeds, otherwise run a fallback.
chooseElseP :: (Monad m, Foldable f) => f (GuardedCaseT e m a) -> ParserT e m a -> ParserT e m a
chooseElseP branches fallback = go (toList branches)
 where
  go [] = fallback
  go (GuardedCaseT guard parser : rest) = do
    mx <- guardP guard
    case mx of
      Nothing -> go rest
      Just x -> parser x

-- | Labels parse errors
labelP :: (Monad m) => Label -> ParserT e m a -> ParserT e m a
labelP lab (ParserT g) = ParserT $ \j ->
  g $ \case
    Left e -> mkErrT (ReasonLabeled lab e) >>= j . Left
    Right a -> j (Right a)

-- | Expect the given text at the start of the range
textP :: (Monad m) => Text -> ParserT e m Text
textP n = do
  o <- takeP (T.length n)
  if n == o
    then pure n
    else errP (ReasonExpect n o)

-- | Expect the given text at the start of the range, discarding the matched text.
textP_ :: (Monad m) => Text -> ParserT e m ()
textP_ = void . textP

-- | Expect the given character at the start of the range
charP :: (Monad m) => Char -> ParserT e m Char
charP = fmap T.head . textP . T.singleton

-- | Expect the given character at the start of the range, discarding the matched character.
charP_ :: (Monad m) => Char -> ParserT e m ()
charP_ = void . charP

-- | Expect one of the given characters at the start of the range.
someCharP :: (Monad m, Foldable f) => f Char -> ParserT e m Char
someCharP chars0 = do
  let chars = toList chars0
  case chars of
    [] -> errP ReasonEmpty
    _ -> do
      mc <- stateP $ \st ->
        let h = stHay st
        in  case T.uncons h of
              Nothing -> (Nothing, st)
              Just (c, h') ->
                if c `elem` chars
                  then
                    let r = stSpan st
                        r' = r {spanStart = spanStart r + 1}
                        st' = st {stHay = h', stSpan = r'}
                    in  (Just c, st')
                  else (Nothing, st)
      case mc of
        Nothing -> do
          hay <- getsP stHay
          let expected = T.pack chars
              actual = T.take 1 hay
          errP (ReasonExpect expected actual)
        Just c -> pure c

-- | Expect one of the given characters at the start of the range, discarding the matched character.
someCharP_ :: (Monad m, Foldable f) => f Char -> ParserT e m ()
someCharP_ = void . someCharP

-- | Parse an expression using all Pratt productions, starting at the lowest precedence.
prattP :: (Monad m, Foldable f) => f (OpT e m a) -> ParserT e m a -> ParserT e m a
prattP productions = prattWithTable (prattTable productions)

-- | Parse an expression using Pratt productions whose binding power is at least the supplied precedence.
prattAtP :: (Monad m, Foldable f) => Prec -> f (OpT e m a) -> ParserT e m a -> ParserT e m a
prattAtP minPrec productions = prattAtWithTable minPrec (prattTable productions)

-- | Group Pratt operator productions into a reusable table.
prattTable :: (Foldable f) => f (OpT e m a) -> PrattTableT e m a
prattTable = PrattTableT . groupProds

-- | Parse an expression using a pre-grouped Pratt operator table, starting at the lowest precedence.
prattWithTable :: (Monad m) => PrattTableT e m a -> ParserT e m a -> ParserT e m a
prattWithTable = prattAtWithTable (Prec minBound)

-- | Parse an expression using a pre-grouped Pratt operator table at the supplied minimum precedence.
prattAtWithTable :: (Monad m) => Prec -> PrattTableT e m a -> ParserT e m a -> ParserT e m a
prattAtWithTable minPrec (PrattTableT prods) atom = parseAt minPrec Nothing
 where
  parseAt prec forbidden = do
    lhs <- parseHead
    parseTail prec forbidden lhs

  parseHead = chooseElseP (prefixBranch <$> prodsPrefixAll prods) atom

  prefixBranch (PrefixOpT opPrec guard op) = guardedWith guard $ \guardValue -> do
    build <- op guardValue
    build <$> parseAt opPrec Nothing

  parseTail prec forbidden lhs = do
    rejectForbidden forbidden
    step <- chooseElseP (stepBranch lhs <$> (postfixSteps <> infixSteps)) (pure Nothing)
    case step of
      Nothing -> pure lhs
      Just lhs1 -> parseTail prec Nothing lhs1
   where
    postfixSteps = StepPostfixT <$> prodsPostfixAtLeast prec prods
    infixSteps = StepInfixT <$> prodsInfixAtLeast prec prods

    stepBranch lhs0 stepProd = case stepProd of
      StepPostfixT (PostfixOpT _ guard op) -> guardedWith guard $ \guardValue -> do
        build <- op guardValue
        pure (Just (build lhs0))
      StepInfixT (InfixOpT assoc opPrec guard op) -> guardedWith guard $ \guardValue -> do
        build <- op guardValue
        rhs <- parseAt (rightPrec assoc opPrec) (nonAssocPrec assoc opPrec)
        pure (Just (build lhs0 rhs))

  rejectForbidden = \case
    Nothing -> pure ()
    Just opPrec -> chooseElseP (forbiddenBranch <$> prodsInfixAt opPrec prods) (pure ())
     where
      forbiddenBranch (InfixOpT _ _ guard _) = guardedWith guard (const (errP (ReasonNonAssoc (unPrec opPrec))))

  rightPrec = \case
    AssocLeft -> nextPrec
    AssocRight -> id
    AssocNone -> nextPrec

  nonAssocPrec = \case
    AssocNone -> Just
    _ -> const Nothing

-- | Build a prefix Pratt operator from a guard and a constant AST builder.
prefixOp :: Prec -> ParserT e m z -> (a -> a) -> OpT e m a
prefixOp prec guard build = prefixOpWith prec guard (const (pure build))

-- | Build a prefix Pratt operator from a guard and parser for its AST builder.
prefixOpWith :: Prec -> ParserT e m z -> (z -> ParserT e m (a -> a)) -> OpT e m a
prefixOpWith prec guard parser = OpPrefixT (PrefixOpT prec guard parser)

-- | Build a postfix Pratt operator from a guard and a constant AST builder.
postfixOp :: Prec -> ParserT e m z -> (a -> a) -> OpT e m a
postfixOp prec guard build = postfixOpWith prec guard (const (pure build))

-- | Build a postfix Pratt operator from a guard and parser for its AST builder.
postfixOpWith :: Prec -> ParserT e m z -> (z -> ParserT e m (a -> a)) -> OpT e m a
postfixOpWith prec guard parser = OpPostfixT (PostfixOpT prec guard parser)

-- | Build an infix Pratt operator from a guard and a constant AST builder.
infixOp :: Assoc -> Prec -> ParserT e m z -> (a -> a -> a) -> OpT e m a
infixOp assoc prec guard build = infixOpWith assoc prec guard (const (pure build))

-- | Build an infix Pratt operator from a guard and parser for its AST builder.
infixOpWith :: Assoc -> Prec -> ParserT e m z -> (z -> ParserT e m (a -> a -> a)) -> OpT e m a
infixOpWith assoc prec guard parser = OpInfixT (InfixOpT assoc prec guard parser)

nextPrec :: Prec -> Prec
nextPrec (Prec n)
  | n == maxBound = Prec maxBound
  | otherwise = Prec (n + 1)

groupProds :: (Foldable f) => f (OpT e m a) -> ProdsT e m a
groupProds = foldr go (ProdsT IntMap.empty IntMap.empty IntMap.empty)
 where
  go prod acc = case prod of
    OpPrefixT prefix -> acc {prodsPrefix = insertProd prefixPrec prefix (prodsPrefix acc)}
    OpPostfixT postfix -> acc {prodsPostfix = insertProd postfixPrec postfix (prodsPostfix acc)}
    OpInfixT infix0 -> acc {prodsInfix = insertProd infixPrec infix0 (prodsInfix acc)}

insertProd :: (a -> Prec) -> a -> IntMap (Seq a) -> IntMap (Seq a)
insertProd prec prod = IntMap.alter appendProd (unPrec (prec prod))
 where
  appendProd = \case
    Nothing -> Just (Seq.singleton prod)
    Just prods -> Just (prods :|> prod)

prodsPrefixAll :: ProdsT e m a -> Seq (PrefixOpT e m a)
prodsPrefixAll = intMapDesc . prodsPrefix

prodsPostfixAtLeast :: Prec -> ProdsT e m a -> Seq (PostfixOpT e m a)
prodsPostfixAtLeast minPrec = prodsAtLeast minPrec . prodsPostfix

prodsInfixAtLeast :: Prec -> ProdsT e m a -> Seq (InfixOpT e m a)
prodsInfixAtLeast minPrec = prodsAtLeast minPrec . prodsInfix

prodsInfixAt :: Prec -> ProdsT e m a -> Seq (InfixOpT e m a)
prodsInfixAt prec = fromMaybe Empty . IntMap.lookup (unPrec prec) . prodsInfix

prodsAtLeast :: Prec -> IntMap (Seq a) -> Seq a
prodsAtLeast prec prods = intMapDescThen higher (fromMaybe Empty here)
 where
  (_, here, higher) = IntMap.splitLookup (unPrec prec) prods

intMapDesc :: IntMap (Seq a) -> Seq a
intMapDesc prods = intMapDescThen prods Empty

intMapDescThen :: IntMap (Seq a) -> Seq a -> Seq a
intMapDescThen prods rest = case IntMap.maxViewWithKey prods of
  Nothing -> rest
  Just ((_, bucket), lower) -> bucket <> intMapDescThen lower rest

-- | Take the given number of characters from the start of the range, or fewer if empty
takeP :: (Monad m) => Int -> ParserT e m Text
takeP i = stateP $ \st ->
  let h = stHay st
      (o, h') = T.splitAt i h
      l = T.length o
      r = stSpan st
      r' = r {spanStart = spanStart r + l}
      st' = st {stHay = h', stSpan = r'}
  in  (T.copy o, st')

-- | Take exactly the given number of characters from the start of the range, or error
takeExactP :: (Monad m) => Int -> ParserT e m Text
takeExactP i = do
  et <- stateP $ \st ->
    let h = stHay st
        (o, h') = T.splitAt i h
        l = T.length o
        r = stSpan st
        r' = r {spanStart = spanStart r + T.length o}
        st' = st {stHay = h', stSpan = r'}
    in  if l == i then (Right (T.copy o), st') else (Left l, st)
  case et of
    Left l -> errP (ReasonDemand i l)
    Right a -> pure a

-- | Drop the given number of characters from the start of the range, or fewer if empty
dropP :: (Monad m) => Int -> ParserT e m Int
dropP = fmap T.length . takeP

-- | Drop exactly the given number of characters from the start of the range, or error
dropExactP :: (Monad m) => Int -> ParserT e m ()
dropExactP = void . takeExactP

-- | Take characters from the start of the range satisfying the predicate
takeWhileP :: (Monad m) => (Char -> Bool) -> ParserT e m Text
takeWhileP f = stateP $ \st ->
  let h = stHay st
      o = T.takeWhile f h
      l = T.length o
      h' = T.drop l h
      r = stSpan st
      r' = r {spanStart = spanStart r + l}
      st' = st {stHay = h', stSpan = r'}
  in  (T.copy o, st')

-- | Like 'takeWhileP' but ensures at least 1 character has been taken
takeWhile1P :: (Monad m) => (Char -> Bool) -> ParserT e m Text
takeWhile1P f = do
  mt <- stateP $ \st ->
    let h = stHay st
        o = T.takeWhile f h
        l = T.length o
        h' = T.drop l h
        r = stSpan st
        r' = r {spanStart = spanStart r + l}
        st' = st {stHay = h', stSpan = r'}
    in  if l == 0 then (Nothing, st) else (Just (T.copy o), st')
  case mt of
    Nothing -> errP ReasonTakeNone
    Just a -> pure a

-- | Drop characters from the start of the range satisfying the predicate
dropWhileP :: (Monad m) => (Char -> Bool) -> ParserT e m Int
dropWhileP = fmap T.length . takeWhileP

-- | Like 'dropWhileP' but ensures at least 1 character has been dropped
dropWhile1P :: (Monad m) => (Char -> Bool) -> ParserT e m Int
dropWhile1P = fmap T.length . takeWhile1P

-- | Take the remaining range, leaving it empty
takeAllP :: (Monad m) => ParserT e m Text
takeAllP = stateP $ \st ->
  let h = stHay st
      r = stSpan st
      r' = r {spanStart = spanEnd r}
      st' = st {stHay = T.empty, stSpan = r'}
  in  (T.copy h, st')

-- | Like 'takeAllP' but ensures at least 1 character has been taken
takeAll1P :: (Monad m) => ParserT e m Text
takeAll1P = do
  mt <- stateP $ \st ->
    let h = stHay st
        r = stSpan st
        r' = r {spanStart = spanEnd r}
        st' = st {stHay = T.empty, stSpan = r'}
    in  if T.null h then (Nothing, st) else (Just (T.copy h), st')
  case mt of
    Nothing -> errP (ReasonDemand 1 0)
    Just a -> pure a

-- | Drop the remaining range, leaving it empty
dropAllP :: (Monad m) => ParserT e m Int
dropAllP = fmap T.length takeAllP

-- | Like 'dropAllP' but ensures at least 1 character has been dropped
dropAll1P :: (Monad m) => ParserT e m Int
dropAll1P = fmap T.length takeAll1P

-- | Unwrap a monad transformer layer (see 'scopeP' for use)
transP :: (MonadTrans t, Monad m) => (forall a. t m a -> m a) -> ParserT e (t m) b -> ParserT e m b
transP f (ParserT g) = ParserT $ \j -> do
  st0 <- get
  (ea, st1) <- lift (f (runT (g (hoist lift . j)) st0))
  put st1
  either throwError pure ea

-- | Parse with some local state
scopeP :: (Monad m) => s -> ParserT e (StateT s m) a -> ParserT e m a
scopeP s0 = transP (`evalStateT` s0)

-- | Repeats the parser until it returns a 'Just' value
iterP :: ParserT e m (Maybe a) -> ParserT e m a
iterP p = go
 where
  go = p >>= maybe go pure

data StrState = StrState !Bool !(Seq Char)

-- | Parse a string with a custom quote character. Supports backslash-escaping.
strP :: (Monad m) => Char -> ParserT e m Text
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
doubleStrP :: (Monad m) => ParserT e m Text
doubleStrP = strP '"'

-- | Parse a single-quoted string
singleStrP :: (Monad m) => ParserT e m Text
singleStrP = strP '\''

-- | Parse between an opening delimiter (first parser) and a closing delimited (second parser)
betweenP :: ParserT e m x -> ParserT e m y -> ParserT e m a -> ParserT e m a
betweenP px py pa = px *> pa <* py

-- | Parse an optional guarded production.
optionalP :: (Monad m) => GuardedCaseT e m a -> ParserT e m (Maybe a)
optionalP branch = chooseElseP [Just <$> branch] (pure Nothing)

repeatTailP :: (Monad m) => GuardedCaseT e m a -> Seq a -> ParserT e m (Seq a)
repeatTailP (GuardedCaseT guard parser) = go
 where
  go !acc = do
    chooseElseP
      [ guardedWith guard $ \x -> do
          a <- parser x
          go (acc :|> a)
      ]
      (pure acc)

-- | Repeat a guarded parser until the guard no longer matches, collecting the results.
repeatP :: (Monad m) => GuardedCaseT e m a -> ParserT e m (Seq a)
repeatP branch = repeatTailP branch Empty

-- | Like 'repeatP' but requires the first item.
repeat1P :: (Monad m) => GuardedCaseT e m a -> ParserT e m (Seq a)
repeat1P branch = do
  item <- chooseP [branch]
  repeatTailP branch (Seq.singleton item)

-- private
sepByTailP :: (Monad m, Foldable f) => GuardedCaseT e m () -> f (GuardedCaseT e m a) -> Seq a -> ParserT e m (Seq a)
sepByTailP (GuardedCaseT sepGuard sepParser) itemBranches = go
 where
  go !acc = do
    chooseElseP
      [ guardedWith sepGuard $ \x -> do
          sepParser x
          a <- chooseP itemBranches
          go (acc :|> a)
      ]
      (pure acc)

-- | Parse a guarded sequence of items separated by a guarded separator.
sepByP :: (Monad m, Foldable f) => GuardedCaseT e m () -> f (GuardedCaseT e m a) -> ParserT e m (Seq a)
sepByP sepBranch itemBranches = chooseElseP (firstBranch <$> toList itemBranches) (pure Empty)
 where
  firstBranch (GuardedCaseT itemGuard itemParser) = guardedWith itemGuard $ \x -> do
    item <- itemParser x
    sepByTailP sepBranch itemBranches (Seq.singleton item)

-- | Like 'sepByP' but ensures at least one result.
sepBy1P :: (Monad m, Foldable f) => GuardedCaseT e m () -> f (GuardedCaseT e m a) -> ParserT e m (Seq a)
sepBy1P sepBranch itemBranch = do
  item <- chooseP itemBranch
  sepByTailP sepBranch itemBranch (Seq.singleton item)

-- | Like 'sepByP' but ensures at least two results (and at least one delimiter).
sepBy2P :: (Monad m, Foldable f) => GuardedCaseT e m () -> f (GuardedCaseT e m a) -> ParserT e m (Seq a)
sepBy2P sepBranch itemBranch = do
  a0 <- chooseP itemBranch
  chooseP [sepBranch]
  a1 <- chooseP itemBranch
  sepByTailP sepBranch itemBranch (Empty :|> a0 :|> a1)

-- | Consumes many spaces at the start of the range
spaceP :: (Monad m) => ParserT e m ()
spaceP = void (dropWhileP isSpace)

-- | Strips spaces before and after parsing
stripP :: (Monad m) => ParserT e m a -> ParserT e m a
stripP p = spaceP *> p <* spaceP

-- | Strips spaces before parsing
stripStartP :: (Monad m) => ParserT e m a -> ParserT e m a
stripStartP p = spaceP *> p

-- | Strips spaces after parsing
stripEndP :: (Monad m) => ParserT e m a -> ParserT e m a
stripEndP p = p <* spaceP

-- | Parses and returns the length of the consumed input along with the result
measureP :: (Monad m) => ParserT e m a -> ParserT e m (a, Int)
measureP p = do
  start <- getsP (spanStart . stSpan)
  a <- p
  end <- getsP (spanStart . stSpan)
  pure (a, end - start)

-- | Takes exactly 1 character from the start of the range, returning Nothing
-- if at end of input
unconsP :: (Monad m) => ParserT e m (Maybe Char)
unconsP = stateP $ \st ->
  let h = stHay st
      mxy = T.uncons h
  in  case mxy of
        Nothing -> (Nothing, st)
        Just (x, y) ->
          let r = stSpan st
              r' = r {spanStart = spanStart r + 1}
              st' = st {stHay = y, stSpan = r'}
          in  (Just x, st')

-- | Takes exactly 1 character from the start of the range, throwing error
-- if at end of input
headP :: (Monad m) => ParserT e m Char
headP = unconsP >>= maybe (errP (ReasonDemand 1 0)) pure

-- | Add signed-ness to any parser with a negate function
signedWithP :: (Monad m) => (a -> a) -> ParserT e m a -> ParserT e m a
signedWithP neg p = chooseElseP [guarded (charP_ '-') (fmap neg p)] p

-- | Add signed-ness to any numeric parser
signedP :: (Monad m, Num a) => ParserT e m a -> ParserT e m a
signedP = signedWithP negate

-- | Parse an signed integer
intP :: (Monad m) => ParserT e m Integer
intP = signedP uintP

-- | Parse an unsigned integer
uintP :: (Monad m) => ParserT e m Integer
uintP = T.foldl' (\n d -> n * 10 + fromIntegral (digitToInt d)) 0 <$> takeWhile1P isDigit

-- | Parse a signed decimal
decP :: (Monad m) => ParserT e m Rational
decP = signedP udecP

-- | Parse an unsigned decimal
udecP :: (Monad m) => ParserT e m Rational
udecP = do
  whole <- fmap fromInteger uintP
  chooseElseP
    [ guarded (charP_ '.') $ do
        (numerator, places) <- measureP uintP
        let denominator = 10 ^ places
            part = numerator % denominator
        pure (whole + part)
    ]
    (pure whole)

-- | Parse a signed scientific number
sciP :: (Monad m) => ParserT e m Scientific
sciP = signedP usciP

-- | Parse an unsigned scientific  number
usciP :: (Monad m) => ParserT e m Scientific
usciP = do
  whole <- uintP
  (frac, places) <- chooseElseP [guarded (charP_ '.') (measureP uintP)] (pure (0, 0))
  ex <-
    chooseElseP [guarded (someCharP_ ("eE" :: String)) (fmap fromIntegral intP)] (pure 0)
  let wholeS = S.scientific whole ex
      partS = S.scientific frac (ex - places)
  pure (wholeS + partS)

-- | Parse a signed integer/scientific number, defaulting to integer if possible.
numP :: (Monad m) => ParserT e m (Either Integer Scientific)
numP = signedWithP (bimap negate negate) unumP

-- | Parse an unsigned integer/scientific number, defaulting to integer if possible.
unumP :: (Monad m) => ParserT e m (Either Integer Scientific)
unumP = do
  whole <- uintP
  mayFracPlaces <- chooseElseP [guarded (charP_ '.') (fmap Just (measureP uintP))] (pure Nothing)
  mayEx <-
    chooseElseP
      [guarded (someCharP_ ("eE" :: String)) (fmap (Just . fromIntegral) intP)]
      (pure Nothing)
  case (mayFracPlaces, mayEx) of
    (Nothing, Nothing) -> pure (Left whole)
    _ -> do
      let (frac, places) = fromMaybe (0, 0) mayFracPlaces
          ex = fromMaybe 0 mayEx
          wholeS = S.scientific whole ex
          partS = S.scientific frac (ex - places)
      pure (Right (wholeS + partS))

-- | Like 'spaceP' but ensures at least 1 space removed
space1P :: (Monad m) => ParserT e m ()
space1P = void (dropWhile1P isSpace)

-- | Like 'stripP' but ensures at least 1 space removed
strip1P :: (Monad m) => ParserT e m a -> ParserT e m a
strip1P p = space1P *> p <* space1P

-- | Like 'stripStartP' but ensures at least 1 space removed
stripStart1P :: (Monad m) => ParserT e m a -> ParserT e m a
stripStart1P p = space1P *> p

-- | Like 'stripEndP' but ensures at least 1 space removed
stripEnd1P :: (Monad m) => ParserT e m a -> ParserT e m a
stripEnd1P p = p <* space1P

-- | Implement this to format custom errors. The list will be indented and joined with `unlines`.
class HasErrMessage e where
  -- | Render a custom error value as one or more lines, using the supplied offset renderer.
  getErrMessage :: (Int -> Text) -> e -> [Text]

instance HasErrMessage Void where
  getErrMessage = const absurd

-- private
indent :: Int -> [Text] -> [Text]
indent i = let s = T.replicate (2 * i) " " in fmap (s <>)

instance (HasErrMessage e) => HasErrMessage (Err e) where
  getErrMessage repPos = go
   where
    go (Err (ErrF (Span start end) re)) =
      let pos = "Error in range " <> repPos start <> "-" <> repPos end <> ":"
          body = case re of
            ReasonCustom e ->
              let hd = "Custom error:"
                  tl = indent 1 (getErrMessage repPos e)
              in  hd : tl
            ReasonExpect expected actual ->
              ["Expected text: '" <> expected <> "' but found: '" <> actual <> "'"]
            ReasonDemand expected actual ->
              ["Expected count: " <> T.pack (show expected) <> " but got: " <> T.pack (show actual)]
            ReasonLeftover count ->
              ["Expected end but had leftover count: " <> T.pack (show count)]
            ReasonNonAssoc prec ->
              ["Non-associative operator cannot be chained at precedence: " <> T.pack (show prec)]
            ReasonFail msg -> ["User reported failure: " <> msg]
            ReasonLabeled lab e ->
              let hd = "Label: " <> unLabel lab
                  tl = indent 1 (go e)
              in  hd : tl
            ReasonTakeNone -> ["Took/dropped no elements"]
            ReasonEmpty -> ["No parse results"]
            ReasonExplained msg hide e ->
              case hide of
                HideErrorNo ->
                  let tl = indent 1 (go e)
                  in  msg : tl
                HideErrorYes -> [msg]
      in  pos : body

-- | Create errata formatting a parse error.
errataE :: (HasErrMessage e) => FilePath -> (Int -> (E.Line, E.Column)) -> Err e -> [E.Errata]
errataE fp mkP e =
  let (line, col) = mkP (spanStart (errSpan e))
      repP i = let (l, c) = mkP i in "(" <> T.pack (show l) <> ", " <> T.pack (show c) <> ")"
      msg = getErrMessage repP e
      block = E.blockSimple E.basicStyle E.basicPointer fp Nothing (line, col, col + 1, Nothing) (Just (T.unlines msg))
  in  [E.Errata Nothing [block] Nothing]

-- | Render a formatted error to text
renderE :: (HasErrMessage e) => FilePath -> Text -> Err e -> Text
renderE fp h e =
  let v = calculateLineCol h
      mkP i = let (l, c) = lookupLineCol i v in (l + 1, c + 1)
  in  TL.toStrict (E.prettyErrors h (errataE fp mkP e))

-- | Print a formatted error to stderr
printE :: (HasErrMessage e) => FilePath -> Text -> Err e -> IO ()
printE fp h e = TIO.hPutStrLn stderr (renderE fp h e)
