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
import Looksee (GuardedCaseT, ParserT, chooseElseP, dropWhileP, guarded, headP, iterP, textP, textP_)

space
  :: (Monad m)
  => GuardedCaseT e m ()
  -- ^ A guarded parser for space characters which does not accept empty (e.g. @guarded space1P (pure ())@)
  -> GuardedCaseT e m ()
  -- ^ A guarded parser for a line comment (e.g. @guarded (skipLineComment "--") (pure ())@)
  -> GuardedCaseT e m ()
  -- ^ A guarded parser for a block comment (e.g. @guarded (skipBlockComment "{-" "-}") (pure ())@)
  -> ParserT e m ()
space sp line block =
  iterP $ chooseElseP [Nothing <$ sp, Nothing <$ line, Nothing <$ block] (pure (Just ()))

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
  iterP $ chooseElseP [guarded (textP_ end) (pure (Just ()))] (Nothing <$ headP)
