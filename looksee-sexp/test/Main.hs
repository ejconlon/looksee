module Main (main) where

import Bowtie (unMkMemo)
import Data.Text (Text)
import Data.Void (Void)
import Looksee (Err)
import Looksee.Sexp (Atom (..), Sexp (..), SexpF (..), sexpParser)
import Test.Daytripper (Expect, MonadExpect, daytripperMain, mkUnitRT, testRT)
import Test.Looksee.Trip (cmpEq, expectParsePretty)
import Test.Tasty (testGroup)

expectParseSexp :: (MonadExpect m) => Expect m Sexp Text (Either (Err Void) Sexp)
expectParseSexp = expectParsePretty (fmap unMkMemo sexpParser) cmpEq

main :: IO ()
main =
  daytripperMain $
    testGroup
      "looksee-sexp"
      [ testRT $ mkUnitRT "example" expectParseSexp (Sexp (SexpAtomF (AtomInt 1)))
      ]
