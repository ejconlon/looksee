module Test.Looksee.Trip
  ( Cmp
  , cmpEq
  , ExpectP
  , expectParse
  , expectParsePretty
  , expectParseT
  , expectParseTPretty
  , expectRendered
  )
where

import Data.Text (Text)
import Looksee (Err, Parser, ParserT, parse, parseT)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Daytripper (Expect, MonadExpect (..), expectDuring, mkExpect)

prettyText :: (Pretty a) => a -> Text
prettyText = renderStrict . layoutPretty defaultLayoutOptions . pretty

type Cmp e m a = Maybe a -> Either (Err e) a -> m ()

cmpEq :: (MonadExpect m, Eq e, Show e, Eq a, Show a) => Cmp e m a
cmpEq ma ea = expectAssertEq (maybe (Left Nothing) Right ma) (either (Left . Just) Right ea)

type ExpectP e m a = Expect m a Text (Either (Err e) a)

expectParse :: (MonadExpect m) => (a -> Text) -> Parser e a -> Cmp e m a -> ExpectP e m a
expectParse printer parser = mkExpect enc dec
 where
  enc = pure . printer
  dec = pure . parse parser

expectParsePretty :: (MonadExpect m, Pretty a) => Parser e a -> Cmp e m a -> ExpectP e m a
expectParsePretty = expectParse prettyText

expectParseT :: (MonadExpect m) => (a -> Text) -> ParserT e m a -> Cmp e m a -> ExpectP e m a
expectParseT printer parser = mkExpect enc dec
 where
  enc = pure . printer
  dec = parseT parser

expectParseTPretty :: (MonadExpect m, Pretty a) => ParserT e m a -> Cmp e m a -> ExpectP e m a
expectParseTPretty = expectParseT prettyText

expectRendered :: (MonadExpect m) => Text -> ExpectP e m a -> ExpectP e m a
expectRendered = expectDuring . const . flip expectAssertEq
