{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Looksee.Sexp
  ( Symbol (..)
  , Atom (..)
  , AtomType (..)
  , Brace (..)
  , SexpF (..)
  , Sexp (..)
  , SexpType (..)
  , sexpList
  , IsSexp (..)
  , LocSexp
  , OffsetSpan
  , sexpParser
  )
where

import Bowtie (Anno (..), Memo (..), pattern MemoP)
import Bowtie qualified as B
import Control.Foldl (Fold (..))
import Control.Monad (guard, unless, void)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Char (isControl, isDigit, isSpace)
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Looksee (ParserT, Span (..))
import Looksee qualified as L
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

{- TODO
 - Recognize quote/unquote
 - Add tests
 -}

-- Generic parser combinators

guard1P :: (Monad m) => (Char -> Bool) -> ParserT e m ()
guard1P f = L.headP >>= guard . f

guard2P :: (Monad m) => (Char -> Bool) -> (Char -> Bool) -> ParserT e m ()
guard2P f g = guard1P f *> guard1P g

cons1P :: (Monad m) => (Char -> Bool) -> (Char -> Bool) -> ParserT e m Text
cons1P f g = liftA2 T.cons (L.headP >>= \c -> c <$ guard (f c)) (L.takeWhileP g)

commitSameP :: (Monad m) => [ParserT e m a] -> ParserT e m a
commitSameP = L.commitP . fmap (\p -> (void p, p))

explainEmptyP :: (Monad m) => Text -> ParserT e m a -> ParserT e m a
explainEmptyP msg = L.explainP $ \case
  L.ReasonEmpty -> Just (msg, True)
  _ -> Nothing

-- Domain-specific stuff

newtype Symbol = Symbol {unSymbol :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Leaves of S-expression trees
data Atom
  = AtomSym !Symbol
  | AtomInt !Integer
  | AtomSci !Scientific
  | AtomStr !Text
  | AtomChar !Char
  deriving stock (Eq, Ord, Show)

instance Pretty Atom where
  pretty = \case
    AtomSym x -> pretty x
    AtomInt x -> pretty x
    AtomSci x -> P.viaShow x
    AtomStr x -> "\"" <> pretty x <> "\""
    AtomChar x -> "'" <> pretty x <> "'"

data AtomType
  = AtomTypeSym
  | AtomTypeInt
  | AtomTypeSci
  | AtomTypeStr
  | AtomTypeChar
  deriving stock (Eq, Ord, Show, Enum, Bounded)

atomType :: Atom -> AtomType
atomType = \case
  AtomSym _ -> AtomTypeSym
  AtomInt _ -> AtomTypeInt
  AtomSci _ -> AtomTypeSci
  AtomStr _ -> AtomTypeStr
  AtomChar _ -> AtomTypeChar

data Brace = BraceParen | BraceCurly | BraceSquare
  deriving stock (Eq, Ord, Show, Enum, Bounded)

openBraceChar :: Brace -> Char
openBraceChar = \case
  BraceParen -> '('
  BraceCurly -> '{'
  BraceSquare -> '['

closeBraceChar :: Brace -> Char
closeBraceChar = \case
  BraceParen -> ')'
  BraceCurly -> '}'
  BraceSquare -> ']'

readOpenBrace :: Char -> Maybe Brace
readOpenBrace = \case
  '(' -> Just BraceParen
  '{' -> Just BraceCurly
  '[' -> Just BraceSquare
  _ -> Nothing

readCloseBrace :: Char -> Maybe Brace
readCloseBrace = \case
  ')' -> Just BraceParen
  '}' -> Just BraceCurly
  ']' -> Just BraceSquare
  _ -> Nothing

-- | An S-expression
data SexpF r
  = SexpAtomF !Atom
  | SexpListF !Brace !(Seq r)
  | SexpQuoteF r
  | SexpUnquoteF r
  | SexpDocF !(Seq Text) r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty r) => Pretty (SexpF r) where
  pretty = \case
    SexpAtomF a -> pretty a
    SexpListF b rs -> pretty (openBraceChar b) <> P.hsep (fmap pretty (toList rs)) <> pretty (closeBraceChar b)
    SexpQuoteF r -> "`" <> pretty r
    SexpUnquoteF r -> "," <> pretty r
    SexpDocF d r ->
      case d of
        Empty -> pretty r
        _ -> P.hcat (toList (fmap (\y -> ";|" <> pretty y <> "\n") d :|> pretty r))

newtype Sexp = Sexp {unSexp :: SexpF Sexp}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

type instance Base Sexp = SexpF

instance Recursive Sexp where project = unSexp

instance Corecursive Sexp where embed = Sexp

data SexpType
  = SexpTypeAtom !AtomType
  | SexpTypeList !Brace
  | SexpTypeQuote
  | SexpTypeUnquote
  | SexpTypeDoc
  deriving stock (Eq, Ord, Show)

sexpType :: SexpF r -> SexpType
sexpType = \case
  SexpAtomF at -> SexpTypeAtom (atomType at)
  SexpListF b _ -> SexpTypeList b
  SexpQuoteF _ -> SexpTypeQuote
  SexpUnquoteF _ -> SexpTypeUnquote
  SexpDocF _ _ -> SexpTypeDoc

class IsSexp s where
  toSexp :: s -> Sexp

instance IsSexp Sexp where
  toSexp = id

instance IsSexp (Memo SexpF b) where
  toSexp = B.unMkMemo

instance (IsSexp s) => IsSexp (Anno b s) where
  toSexp = toSexp . B.annoVal

instance IsSexp Atom where
  toSexp = Sexp . SexpAtomF

instance IsSexp Integer where
  toSexp = toSexp . AtomInt

instance IsSexp Symbol where
  toSexp = toSexp . AtomSym

instance IsSexp Char where
  toSexp = toSexp . AtomChar

instance IsSexp String where
  toSexp = toSexp . T.pack

instance IsSexp Text where
  toSexp = toSexp . AtomStr

sexpList :: Brace -> [Sexp] -> Sexp
sexpList b = Sexp . SexpListF b . Seq.fromList

sexpQuote :: Sexp -> Sexp
sexpQuote = Sexp . SexpQuoteF

sexpUnquote :: Sexp -> Sexp
sexpUnquote = Sexp . SexpQuoteF

-- * Parser

-- Char predicates

isSymStart
  , isSymCont
  , isListStart
  , isListEnd
  , isCharStart
  , isStringStart
  , isQuoteStart
  , isUnquoteStart
  , isCommentStart
  , isDocCont
  , isNumStart
  , isAtomStart
    :: Char -> Bool
isSymStart c = not (isDigit c) && isSymCont c
isSymCont c =
  not $
    isControl c
      || isSpace c
      || isCommentStart c
      || isListStart c
      || isListEnd c
      || isStringStart c
      || isCharStart c
      || isQuoteStart c
      || isUnquoteStart c
      || isDigit c
isListStart c = c == '(' || c == '{' || c == '['
isListEnd c = c == ')' || c == '}' || c == ']'
isCharStart c = c == '\''
isStringStart c = c == '\"'
isQuoteStart c = c == '`'
isUnquoteStart c = c == ','
isCommentStart c = c == ';'
isDocCont c = c == '|'
isNumStart c = isDigit c || c == '-'
isAtomStart c =
  isSymStart c
    || isNumStart c
    || isStringStart c
    || isCharStart c

-- The final recursive type

type LocSexp = Memo SexpF OffsetSpan

type OffsetSpan = Span Int

-- Specific parsers

docStartP :: (Monad m) => ParserT e m ()
docStartP = guard2P isCommentStart isDocCont

spaceNP :: (Monad m) => Int -> ParserT e m Int
spaceNP !acc = do
  mc <- L.lookP L.unconsP
  case mc of
    Just ';' -> do
      mds <- L.lookP (L.optP docStartP)
      case mds of
        Just _ -> pure acc
        Nothing -> L.dropWhileP (/= '\n') >>= spaceNP . (acc +)
    Just c | isSpace c -> L.dropWhileP isSpace >>= spaceNP . (acc +)
    _ -> pure acc

spaceP, space1P :: (Monad m) => ParserT e m ()
spaceP = void (spaceNP 0)
space1P = do
  acc <- spaceNP 0
  unless (acc > 0) L.space1P -- Use this to fail

stripP, stripEndP :: (Monad m) => ParserT e m a -> ParserT e m a
stripP p = spaceP *> p <* spaceP
stripEndP p = p <* spaceP

symP :: (Monad m) => ParserT e m Symbol
symP = fmap Symbol (cons1P isSymStart isSymCont)

charLitP :: (Monad m) => ParserT e m Char
charLitP = L.charP_ '\'' *> L.headP <* L.charP_ '\''

stringLitP :: (Monad m) => ParserT e m Text
stringLitP = L.strP '"'

openBraceP :: (Monad m) => ParserT e m Brace
openBraceP = commitSameP (fmap (\b -> b <$ L.charP_ (openBraceChar b)) [minBound .. maxBound])

docLineP :: (Monad m) => ParserT e m Text
docLineP = do
  docStartP
  lin <- L.takeWhileP (/= '\n')
  L.charP_ '\n'
  pure lin

docLinesP :: (Monad m) => ParserT e m (Seq Text)
docLinesP = go Empty
 where
  go !acc = do
    mds <- L.lookP (L.optP docStartP)
    case mds of
      Nothing -> pure acc
      Just _ -> do
        lin <- docLineP
        go (acc :|> lin)

-- | A parser for S-expressions
sexpParser :: (Monad m) => ParserT e m LocSexp
sexpParser = stripP rootP
 where
  rootP =
    explainEmptyP "Not a recognizable Sexp" $
      L.spanAroundP MemoP $
        L.commitP
          [ (guard1P isListStart, L.labelP "list" listP)
          , (guard1P isQuoteStart, L.labelP "quote" quoteP)
          , (guard1P isUnquoteStart, L.labelP "unquote" unquoteP)
          , (guard1P isAtomStart, L.labelP "atom" atomP)
          , (docStartP, L.labelP "doc" docP)
          ]
  listP = do
    b <- stripEndP openBraceP
    ss <- stripEndP (L.sepByP space1P rootP)
    L.charP_ (closeBraceChar b)
    pure (SexpListF b ss)
  quoteP = L.charP_ '`' *> fmap SexpQuoteF rootP
  unquoteP = L.charP_ ',' *> fmap SexpQuoteF rootP
  atomP =
    SexpAtomF
      <$> L.commitP
        [ (guard1P isSymStart, L.labelP "sym" (fmap AtomSym symP))
        , (guard1P isNumStart, L.labelP "num" (fmap (either AtomInt AtomSci) L.numP))
        , (guard1P isStringStart, L.labelP "str" (fmap AtomStr stringLitP))
        , (guard1P isCharStart, L.labelP "char" (fmap AtomChar charLitP))
        ]
  docP = do
    doc <- docLinesP
    fmap (SexpDocF doc) rootP

-- * Recognizer

data X e s = X !(Maybe e) !s

foldUntilErr :: (a -> ExceptT e (State s) ()) -> s -> (Maybe e -> s -> b) -> Fold a b
foldUntilErr step initial extract = Fold step' initial' extract'
 where
  step' x@(X me s) a =
    case me of
      Just _ -> x
      Nothing ->
        let (ea, s') = runState (runExceptT (step a)) s
        in  case ea of
              Left e -> X (Just e) s'
              Right _ -> X Nothing s'
  initial' = X Nothing initial
  extract' (X me s) = extract me s

data RecogElem
  = RecogElemString
  | RecogElemChar
  | RecogElemComment
  | RecogElemSlashEsc
  | RecogElemQuote
  | RecogElemUnquote
  | RecogElemBrace !Brace
  deriving stock (Eq, Ord, Show)

newtype RecogErr
  = RecogErrMismatch Brace
  deriving stock (Eq, Ord, Show)

data RecogState = RecogState
  { rsOffset :: !Int
  , rsStack :: ![RecogElem]
  }
  deriving stock (Eq, Ord, Show)

initRecogState :: RecogState
initRecogState = RecogState 0 []

type RecogM = ExceptT RecogErr (State RecogState)

data CharCase
  = CharCaseNewline
  | CharCaseDoubleQuote
  | CharCaseSingleQuote
  | CharCaseOpenComment
  | CharCaseSlashEsc
  | CharCaseOpenBrace !Brace
  | CharCaseCloseBrace !Brace
  deriving stock (Eq, Ord, Show)

readCharCase :: Char -> Maybe CharCase
readCharCase c =
  if
    | c == '\n' -> Just CharCaseNewline
    | c == '"' -> Just CharCaseDoubleQuote
    | c == '\'' -> Just CharCaseSingleQuote
    | c == ';' -> Just CharCaseOpenComment
    | c == '\\' -> Just CharCaseSlashEsc
    | otherwise ->
        case readOpenBrace c of
          Just b -> Just (CharCaseOpenBrace b)
          Nothing -> case readCloseBrace c of
            Just b -> Just (CharCaseCloseBrace b)
            Nothing -> Nothing

stepR :: Char -> RecogM ()
stepR c = goRet
 where
  goRet = goStart <* incOffset
  goStart = do
    mh <- peekStack
    case mh of
      Just RecogElemString -> goString
      Just RecogElemChar -> goChar
      Just RecogElemComment -> goComment
      Just RecogElemSlashEsc -> goSlashEsc
      Just RecogElemQuote -> goQuote
      Just RecogElemUnquote -> goUnquote
      Just (RecogElemBrace b) -> goDefault (Just b)
      Nothing -> goDefault Nothing
  goString = case readCharCase c of
    Just CharCaseDoubleQuote -> popStack
    Just CharCaseSlashEsc -> pushStack RecogElemSlashEsc
    _ -> pure ()
  goChar = case readCharCase c of
    Just CharCaseSingleQuote -> popStack
    Just CharCaseSlashEsc -> pushStack RecogElemSlashEsc
    _ -> pure ()
  goComment = case readCharCase c of
    Just CharCaseNewline -> popStack
    _ -> pure ()
  goSlashEsc = popStack -- just ignore input and leave slash esc mode
  goQuote = error "TODO"
  goUnquote = error "TODO"
  goDefault mb = case readCharCase c of
    Just CharCaseDoubleQuote -> pushStack RecogElemString
    Just CharCaseSingleQuote -> pushStack RecogElemChar
    Just CharCaseOpenComment -> pushStack RecogElemComment
    Just (CharCaseOpenBrace b) -> pushStack (RecogElemBrace b)
    Just (CharCaseCloseBrace b) ->
      case mb of
        Just b0 | b == b0 -> popStack
        _ -> throwError (RecogErrMismatch b)
    _ -> pure ()
  incOffset = modify' (\s -> s {rsOffset = rsOffset s + 1})
  pushStack h = modify' (\s -> s {rsStack = h : rsStack s})
  popStack = modify' $ \s ->
    case rsStack s of
      [] -> s
      _ : t -> s {rsStack = t}
  peekStack = gets $ \s ->
    case rsStack s of
      [] -> Nothing
      h : _ -> Just h

extractR :: Maybe RecogErr -> RecogState -> Either RecogErr Bool
extractR me s = maybe (Right (null (rsStack s))) Left me

sexpRecognizer :: Fold Char (Either RecogErr Bool)
sexpRecognizer = foldUntilErr stepR initRecogState extractR
