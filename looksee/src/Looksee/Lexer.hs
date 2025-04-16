-- | Adaptor layer for porting from 'Text.Megaparser.Char.Lexer'.
module Looksee.Lexer
  ( space
  , lexeme
  , symbol
  , symbol_
  , skipLineComment
  , skipBlockComment
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Looksee (ParserT, altP, breakP, dropAllP, dropWhileP, optP, textP, textP_)

space
  :: (Monad m)
  => ParserT e m ()
  -- ^ A parser for space characters which does not accept empty (e.g. 'space1P')
  -> ParserT e m ()
  -- ^ A parser for a line comment (e.g. 'skipLineComment')
  -> ParserT e m ()
  -- ^ A parser for a block comment (e.g. 'skipBlockComment')
  -> ParserT e m ()
space sp line block =
  let p = optP (altP [sp, line, block])
      go = p >>= \case Nothing -> pure (); Just _ -> go
  in  go

lexeme
  :: ParserT e m ()
  -- ^ How to consume white space after lexeme
  -> ParserT e m a
  -- ^ How to parse actual lexeme
  -> ParserT e m a
lexeme spc p = p <* spc

symbol
  :: (Monad m)
  => ParserT e m ()
  -- ^ How to consume white space after lexeme
  -> Text
  -- ^ Symbol to parse
  -> ParserT e m Text
symbol spc = lexeme spc . textP

symbol_
  :: (Monad m)
  => ParserT e m ()
  -- ^ How to consume white space after lexeme
  -> Text
  -- ^ Symbol to parse
  -> ParserT e m ()
symbol_ spc = lexeme spc . textP_

skipLineComment
  :: (Monad m)
  => Text
  -- ^ Line comment prefix
  -> ParserT e m ()
skipLineComment start = do
  textP_ start
  void (dropWhileP (/= '\n'))

skipBlockComment
  :: (Monad m)
  => Text
  -- ^ Start of block comment
  -> Text
  -- ^ End of block comment
  -> ParserT e m ()
skipBlockComment start end = do
  textP_ start
  void (breakP end dropAllP)
