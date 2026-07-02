{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Sequence qualified as Seq
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Looksee
import Looksee.Examples
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

newtype Error = Error {unError :: String} deriving (Eq, Show, IsString)

type TestParser = Parser Error

data ParserCase a = ParserCase !TestName !(TestParser a) !Text !(Either (Err Error) a)

data Expr
  = ExprVar !Text
  | ExprNeg !Expr
  | ExprFact !Expr
  | ExprAdd !Expr !Expr
  | ExprPow !Expr !Expr
  | ExprEq !Expr !Expr
  deriving stock (Eq, Show)

testParserCase :: (Show a, Eq a) => ParserCase a -> TestTree
testParserCase (ParserCase name parser input expected) = testCase name $ do
  parse parser input @?= expected

testExampleCase :: (Show a, Eq a) => TestName -> Parser Void a -> Text -> Either (Err Void) a -> TestTree
testExampleCase name parser input expected = testCase name $ do
  parse parser input @?= expected

testBasic :: TestTree
testBasic =
  testGroup
    "basic"
    [ testParserCase (ParserCase "text" (textP "hi") "hi" (Right "hi"))
    , testParserCase (ParserCase "takeWhile1" (takeWhile1P (== 'x')) "xxx" (Right "xxx"))
    , testParserCase (ParserCase "takeAll" takeAllP "abc" (Right "abc"))
    , testParserCase (ParserCase "char" (charP 'x') "x" (Right 'x'))
    , testParserCase (ParserCase "someChar" (someCharP ("abc" :: String)) "b" (Right 'b'))
    , testParserCase (ParserCase "someChar_" (someCharP_ ("abc" :: String)) "c" (Right ()))
    ]

testChoose :: TestTree
testChoose = testGroup "choose" (fmap testParserCase cases)
 where
  parser =
    chooseP
      [ guardedWith (charP 'h') (\c -> pure (T.pack [c, 'i']))
      , guarded (charP_ 'y') (pure "i")
      ]
      :: TestParser Text
  fallback = chooseElseP [guarded (charP_ 'h') (pure "i")] (textP "zz")
  cases =
    [ ParserCase "first" parser "h" (Right "hi")
    , ParserCase "second" parser "y" (Right "i")
    , ParserCase "fallback" fallback "zz" (Right "zz")
    ]

testRepeatSep :: TestTree
testRepeatSep = testGroup "repeatSep" (fmap testParserCase cases)
 where
  item = guardedWith (charP 'x') pure
  comma = guarded (charP_ ',') (pure ())
  cases =
    [ ParserCase "repeat" (repeatP item) "xxx" (Right (Seq.fromList "xxx"))
    , ParserCase "repeat empty" (repeatP item) "" (Right Seq.empty)
    , ParserCase "sep" (sepByP comma [item]) "x,x,x" (Right (Seq.fromList "xxx"))
    , ParserCase "sep empty" (sepByP comma [item]) "" (Right Seq.empty)
    ]

testPratt :: TestTree
testPratt = testGroup "pratt" (fmap testParserCase cases)
 where
  parser = prattP ops atomP :: TestParser Expr
  var name = ExprVar name
  atomP = var <$> takeWhile1P (\c -> c >= 'a' && c <= 'z')
  ops =
    [ prefixOp 35 (textP_ "-") ExprNeg
    , postfixOp 40 (textP_ "!") ExprFact
    , infixOp AssocRight 30 (textP_ "^") ExprPow
    , infixOp AssocLeft 10 (textP_ "+") ExprAdd
    , infixOp AssocNone 5 (textP_ "=") ExprEq
    ]
  cases =
    [ ParserCase "left assoc" parser "a+b+c" (Right (ExprAdd (ExprAdd (var "a") (var "b")) (var "c")))
    , ParserCase "right assoc" parser "a^b^c" (Right (ExprPow (var "a") (ExprPow (var "b") (var "c"))))
    , ParserCase "precedence" parser "a+b^c!" (Right (ExprAdd (var "a") (ExprPow (var "b") (ExprFact (var "c")))))
    , ParserCase "prefix postfix" parser "-a!" (Right (ExprNeg (ExprFact (var "a"))))
    ]

testExamples :: TestTree
testExamples =
  testGroup
    "examples"
    [ testExampleCase
        "json"
        jsonParser
        "{\"x\": [null, true]}"
        (Right (JsonObject (Seq.fromList [("x", JsonArray (Seq.fromList [JsonNull, JsonBool True]))])))
    , testExampleCase "arith" arithParser "1 + 2 * 3" (Right (ArithAdd (ArithNum 1) (ArithMul (ArithNum 2) (ArithNum 3))))
    , testExampleCase
        "sexp"
        sexpParser
        "(1 abc \"x\")"
        (Right (SexpList (Seq.fromList [SexpAtom (AtomInt 1), SexpAtom (AtomIdent "abc"), SexpAtom (AtomString "x")])))
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "looksee"
      [ testBasic
      , testChoose
      , testRepeatSep
      , testPratt
      , testExamples
      ]
