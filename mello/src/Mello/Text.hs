module Mello.Text
  ( Brace (..)
  , openBraceChar
  , closeBraceChar
  , readOpenBrace
  , readCloseBrace
  , isSymStart
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
  )
where

import Data.Char (isControl, isDigit, isSpace)

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
