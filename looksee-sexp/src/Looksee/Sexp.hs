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
  , sexpParser
  )
where

import Bowtie (Anno (..), Memo (..), MemoF (..))
import Bowtie qualified as B
import Control.Foldl (Fold (..))
import Control.Monad (guard)
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
import Data.Void (Void)
import Looksee (Parser, ParserT, Span (..))
import Looksee qualified as L
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

newtype Symbol = Symbol {unSymbol :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Leaves of S-expression trees
data Atom
  = AtomSym !Symbol
  | AtomInt !Integer
  | AtomSci !Scientific
  | AtomStr !Text
  deriving stock (Eq, Ord, Show)

instance Pretty Atom where
  pretty = \case
    AtomSym x -> pretty x
    AtomInt x -> pretty x
    AtomSci x -> P.viaShow x
    AtomStr x -> "\"" <> pretty x <> "\""

data AtomType
  = AtomTypeSym
  | AtomTypeInt
  | AtomTypeSci
  | AtomTypeStr
  deriving stock (Eq, Ord, Show, Enum, Bounded)

atomType :: Atom -> AtomType
atomType = \case
  AtomSym _ -> AtomTypeSym
  AtomInt _ -> AtomTypeInt
  AtomSci _ -> AtomTypeSci
  AtomStr _ -> AtomTypeStr

data Brace = BraceParen | BraceCurly | BraceSquare | BraceAngle
  deriving stock (Eq, Ord, Show, Enum, Bounded)

openBrace :: (IsString s) => Brace -> s
openBrace = \case
  BraceParen -> fromString "("
  BraceCurly -> fromString "{"
  BraceSquare -> fromString "["
  BraceAngle -> fromString "<"

closeBrace :: (IsString s) => Brace -> s
closeBrace = \case
  BraceParen -> fromString ")"
  BraceCurly -> fromString "}"
  BraceSquare -> fromString "]"
  BraceAngle -> fromString ">"

readOpenBrace :: Char -> Maybe Brace
readOpenBrace = \case
  '(' -> Just BraceParen
  '{' -> Just BraceCurly
  '[' -> Just BraceSquare
  '<' -> Just BraceAngle
  _ -> Nothing

readCloseBrace :: Char -> Maybe Brace
readCloseBrace = \case
  ')' -> Just BraceParen
  '}' -> Just BraceCurly
  ']' -> Just BraceSquare
  '>' -> Just BraceAngle
  _ -> Nothing

-- | An S-expression
data SexpF r
  = SexpAtomF !Atom
  | SexpListF !Brace !(Seq r)
  -- \| SexpQuote !Sexp
  -- \| SexpUnquote !Sexp
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty r) => Pretty (SexpF r) where
  pretty = \case
    SexpAtomF a -> pretty a
    SexpListF b ss -> openBrace b <> P.hsep (fmap pretty (toList ss)) <> closeBrace b

newtype Sexp = Sexp {unSexp :: SexpF Sexp}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

type instance Base Sexp = SexpF

instance Recursive Sexp where project = unSexp

instance Corecursive Sexp where embed = Sexp

data SexpType
  = SexpTypeAtom !AtomType
  | SexpTypeList !Brace
  deriving stock (Eq, Ord, Show)

sexpType :: SexpF r -> SexpType
sexpType = \case
  SexpAtomF at -> SexpTypeAtom (atomType at)
  SexpListF b _ -> SexpTypeList b

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

sexpList :: Brace -> [Sexp] -> Sexp
sexpList b = Sexp . SexpListF b . Seq.fromList

type LocSexp = Memo SexpF Span

identStartPred :: Char -> Bool
identStartPred c = not (isDigit c) && identContPred c

identContPred :: Char -> Bool
identContPred c =
  not $
    isControl c
      || isSpace c
      || c == ';'
      || c == '#'
      || c == '('
      || c == ')'
      || c == '{'
      || c == '}'
      || c == '['
      || c == ']'
      || c == '<'
      || c == '>'

look1P :: (Monad m) => (Char -> Bool) -> ParserT e m ()
look1P p = L.lookP (L.headP >>= guard . p)

-- | A parser for S-expressions
sexpParser :: Parser Void LocSexp
sexpParser = L.stripP rootP
 where
  -- (needs explicit forall)
  stripEndP :: forall a. Parser Void a -> Parser Void a
  stripEndP p = p <* spaceP
  spaceP = do
    mhd <- L.lookP L.unconsP
    case mhd of
      Just ';' -> L.dropWhileP (/= '\n') *> spaceP
      Just c | isSpace c -> spaceP
      _ -> pure ()
  space1P = do
    mhd <- L.lookP L.unconsP
    case mhd of
      Just c | c == ';' || isSpace c -> spaceP
      _ -> L.space1P -- Fail with this
  symP = look1P identStartPred *> L.takeWhile1P identContPred
  atomP =
    L.altP
      [ L.labelP "sym" (AtomSym . Symbol <$> symP)
      , L.labelP "int" (AtomInt <$> L.intP)
      , L.labelP "sci" (AtomSci <$> L.sciP)
      , L.labelP "str" (AtomStr <$> L.strP '"')
      ]
  listP = do
    b <-
      stripEndP $
        L.altP
          [ BraceParen <$ L.charP_ '('
          , BraceCurly <$ L.charP_ '{'
          , BraceSquare <$ L.charP_ '['
          , BraceAngle <$ L.charP_ '<'
          ]
    ss <- stripEndP (L.sepByP space1P rootP)
    stripEndP (L.textP_ (closeBrace b))
    pure (SexpListF b ss)
  rootP =
    fmap (Memo . MemoF) $
      spanP $
        L.altP
          [ L.labelP "atom" (SexpAtomF <$> atomP)
          , L.labelP "list" listP
          ]

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
  | RecogElemComment
  | RecogElemSlashEsc
  | RecogElemBrace !Brace
  deriving stock (Eq, Ord, Show)

data RecogErr
  = RecogErrMismatch !Brace
  | RecogErrQuote
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
  | CharCaseStringQuote
  | CharCaseOpenComment
  | CharCaseSlashEsc
  | CharCaseOpenBrace !Brace
  | CharCaseCloseBrace !Brace
  deriving stock (Eq, Ord, Show)

readCharCase :: Char -> Maybe CharCase
readCharCase c =
  if
    | c == '\n' -> Just CharCaseNewline
    | c == '"' -> Just CharCaseStringQuote
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
      Just RecogElemComment -> goComment
      Just RecogElemSlashEsc -> goSlashEsc
      Just (RecogElemBrace b) -> goDefault (Just b)
      Nothing -> goDefault Nothing
  goString = case readCharCase c of
    Just CharCaseStringQuote -> popStack
    Just CharCaseSlashEsc -> pushStack RecogElemSlashEsc
    _ -> pure ()
  goComment = case readCharCase c of
    Just CharCaseNewline -> popStack
    _ -> pure ()
  goSlashEsc = popStack -- just ignore input and leave slash esc mode
  goDefault mb = case readCharCase c of
    Just CharCaseStringQuote -> pushStack RecogElemString
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
