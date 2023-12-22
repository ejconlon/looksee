module Test.Looksee.Trip
  ( Cmp
  , cmpEq
  , expectParse
  , expectParsePretty
  , expectParseT
  , expectParseTPretty
  )
where

import Data.Text (Text)
import Looksee (Err, Parser, ParserT, parse, parseT)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Daytripper (Expect, MonadExpect (..), mkExpect)

type Cmp e m a = Maybe a -> Either (Err e) a -> m ()

prettyText :: (Pretty a) => a -> Text
prettyText = renderStrict . layoutPretty defaultLayoutOptions . pretty

cmpEq :: (MonadExpect m, Eq e, Show e, Eq a, Show a) => Cmp e m a
cmpEq ma ea = expectAssertEq (maybe (Left Nothing) Right ma) (either (Left . Just) Right ea)

expectParse :: (MonadExpect m) => (a -> Text) -> Parser e a -> Cmp e m a -> Expect m a Text (Either (Err e) a)
expectParse printer parser = mkExpect enc dec
 where
  enc = pure . printer
  dec = pure . parse parser

expectParsePretty :: (MonadExpect m, Pretty a) => Parser e a -> Cmp e m a -> Expect m a Text (Either (Err e) a)
expectParsePretty = expectParse prettyText

expectParseT :: (MonadExpect m) => (a -> Text) -> ParserT e m a -> Cmp e m a -> Expect m a Text (Either (Err e) a)
expectParseT printer parser = mkExpect enc dec
 where
  enc = pure . printer
  dec = parseT parser

expectParseTPretty :: (MonadExpect m, Pretty a) => ParserT e m a -> Cmp e m a -> Expect m a Text (Either (Err e) a)
expectParseTPretty = expectParseT prettyText
