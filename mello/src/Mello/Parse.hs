{-# LANGUAGE OverloadedStrings #-}

module Mello.Parse
  ( LocSexp
  , OffsetSpan
  , sexpParser
  )
where

import Bowtie (Memo, pattern MemoP)
import Control.Monad (guard, unless, void)
import Data.Char (isSpace)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Looksee (ParserT, Span (..))
import Looksee qualified as L
import Mello.Syntax (Atom (..), Doc (..), SexpF (..), Symbol (..))
import Mello.Text
  ( Brace
  , closeBraceChar
  , isAtomStart
  , isCharStart
  , isListStart
  , isNumStart
  , isQuoteStart
  , isStringStart
  , isSymCont
  , isSymStart
  , isUnquoteStart
  , openBraceChar
  )

-- Generic parser combinators

guard1P :: (Monad m) => (Char -> Bool) -> ParserT e m ()
guard1P f = L.headP >>= guard . f

cons1P :: (Monad m) => (Char -> Bool) -> (Char -> Bool) -> ParserT e m Text
cons1P f g = liftA2 T.cons (L.headP >>= \c -> c <$ guard (f c)) (L.takeWhileP g)

commitSameP :: (Monad m) => [ParserT e m a] -> ParserT e m a
commitSameP = L.commitP . fmap (\p -> (void p, p))

explainEmptyP :: (Monad m) => Text -> ParserT e m a -> ParserT e m a
explainEmptyP msg = L.explainP $ \case
  L.ReasonEmpty -> Just (msg, True)
  _ -> Nothing

-- The final recursive type

type LocSexp = Memo SexpF OffsetSpan

type OffsetSpan = Span Int

-- Specific parsers

docStartP :: (Monad m) => ParserT e m ()
docStartP = L.textP_ ";|"

commentStartP :: (Monad m) => ParserT e m ()
commentStartP = L.charP_ ';'

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

closeBraceP :: (Monad m) => Brace -> ParserT e m ()
closeBraceP = L.charP_ . closeBraceChar

docLinesP :: (Monad m) => ParserT e m Doc
docLinesP = go True Empty
 where
  lineStartP isFirst = if isFirst then docStartP else commentStartP
  lineP isFirst = do
    lineStartP isFirst
    lin <- L.takeWhileP (/= '\n')
    L.charP_ '\n'
    pure lin
  go !isFirst !acc = do
    mx <- L.lookP (L.optP (lineStartP isFirst))
    case mx of
      Nothing -> pure (Doc acc)
      Just _ -> do
        lin <- lineP isFirst
        go False (acc :|> lin)

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
    closeBraceP b
    pure (SexpListF b ss)
  quoteP = L.charP_ '`' *> fmap SexpQuoteF rootP
  unquoteP = L.charP_ ',' *> fmap SexpUnquoteF rootP
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
