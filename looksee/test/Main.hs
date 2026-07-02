{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify)
import Data.Ratio ((%))
import Data.Scientific qualified as S
import Data.Sequence qualified as Seq
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Looksee
import Looksee.Examples
import Looksee.Lexer qualified as Lex
import Test.Looksee.Scala.Parse qualified as Scala
import Test.Looksee.Scala.Syntax qualified as Scala
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

newtype Error = Error {unError :: String} deriving (Eq, Show, IsString)

instance HasErrMessage Error where
  getErrMessage _ (Error msg) = [T.pack msg]

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

testParsePred :: TestName -> TestParser a -> Text -> (Either (Err Error) a -> Bool) -> TestTree
testParsePred name parser input predicate = testCase name $ do
  predicate (parse parser input) @?= True

testBasic :: TestTree
testBasic =
  testGroup
    "basic"
    [ testParserCase (ParserCase "text" (textP "hi") "hi" (Right "hi"))
    , testCase "textSpan" $ textSpan "a\nb" @?= Span (Pos 0 0 0) (Pos 3 1 1)
    , testParserCase (ParserCase "string" (stringP "hi") "hi" (Right "hi"))
    , testParserCase (ParserCase "text_" (textP_ "hi") "hi" (Right ()))
    , testParserCase (ParserCase "end" endP "" (Right ()))
    , testParserCase (ParserCase "eof" eofP "" (Right ()))
    , testParserCase (ParserCase "takeWhile1" (takeWhile1P (== 'x')) "xxx" (Right "xxx"))
    , testParserCase (ParserCase "takeWhile" (takeWhileP (== 'x')) "xx" (Right "xx"))
    , testParserCase (ParserCase "dropWhile" (dropWhileP (== 'x')) "xx" (Right 2))
    , testParserCase (ParserCase "dropWhile1" (dropWhile1P (== 'x')) "xx" (Right 2))
    , testParserCase (ParserCase "take" (takeP 2) "ab" (Right "ab"))
    , testParserCase (ParserCase "drop" (dropP 2) "ab" (Right 2))
    , testParserCase (ParserCase "takeExact" (takeExactP 2) "ab" (Right "ab"))
    , testParserCase (ParserCase "dropExact" (dropExactP 2) "ab" (Right ()))
    , testParserCase (ParserCase "takeAll" takeAllP "abc" (Right "abc"))
    , testParserCase (ParserCase "rest" restP "abc" (Right "abc"))
    , testParserCase (ParserCase "dropAll" dropAllP "abc" (Right 3))
    , testParserCase (ParserCase "takeAll1" takeAll1P "abc" (Right "abc"))
    , testParserCase (ParserCase "dropAll1" dropAll1P "abc" (Right 3))
    , testParserCase (ParserCase "char" (charP 'x') "x" (Right 'x'))
    , testParserCase (ParserCase "char_" (charP_ 'x') "x" (Right ()))
    , testParserCase (ParserCase "someChar" (someCharP ("abc" :: String)) "b" (Right 'b'))
    , testParserCase (ParserCase "someChar_" (someCharP_ ("abc" :: String)) "c" (Right ()))
    , testParserCase (ParserCase "uncons" unconsP "x" (Right (Just 'x')))
    , testParserCase (ParserCase "head" headP "x" (Right 'x'))
    , testParserCase (ParserCase "any" anyP "x" (Right 'x'))
    , testParserCase (ParserCase "satisfy" (satisfyP (== 'x')) "x" (Right 'x'))
    , testParserCase (ParserCase "digit" digitP "1" (Right '1'))
    , testParserCase (ParserCase "letter" letterP "a" (Right 'a'))
    , testParserCase (ParserCase "upper" upperP "A" (Right 'A'))
    , testParserCase (ParserCase "alphaNum" alphaNumP "1" (Right '1'))
    , testParserCase (ParserCase "noneOf" (noneOfP ("abc" :: String)) "x" (Right 'x'))
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
    , ParserCase "repeat1" (repeat1P item) "xxx" (Right (Seq.fromList "xxx"))
    , ParserCase "sep" (sepByP comma [item]) "x,x,x" (Right (Seq.fromList "xxx"))
    , ParserCase "sep empty" (sepByP comma [item]) "" (Right Seq.empty)
    , ParserCase "sep1" (sepBy1P comma [item]) "x,x" (Right (Seq.fromList "xx"))
    , ParserCase "sep2" (sepBy2P comma [item]) "x,x" (Right (Seq.fromList "xx"))
    ]

testCombinators :: TestTree
testCombinators =
  testGroup
    "combinators"
    [ testParserCase (ParserCase "between" (betweenP (charP_ '(') (charP_ ')') (textP "x")) "(x)" (Right "x"))
    , testParserCase (ParserCase "optional some" (optionalP (guardedWith (charP 'x') pure)) "x" (Right (Just 'x')))
    , testParserCase (ParserCase "optional none" (optionalP (guardedWith (charP 'x') pure)) "" (Right Nothing))
    , testParserCase (ParserCase "commit" (commitP (guarded (charP_ 'x') (textP "y"))) "xy" (Right "y"))
    , testParserCase (ParserCase "commit else" (commitElseP (guarded (charP_ 'x') (textP "y")) (textP "z")) "z" (Right "z"))
    , testParserCase (ParserCase "measure" (measureP (textP "abc")) "abc" (Right ("abc", 3)))
    , testParserCase (ParserCase "iter" iterExample "xxx!" (Right 3))
    , testParserCase
        (ParserCase "delim" (delimP (Delims (charP_ '[') (charP_ ']')) Nothing (charP 'x')) "[xx]" (Right (Seq.fromList "xx")))
    , testParserCase
        (ParserCase "delim by" (delimByP (charP_ '[') (charP_ ']') (charP_ ',') (charP 'x')) "[x,x]" (Right (Seq.fromList "xx")))
    , testParserCase
        ( ParserCase
            "delim end by"
            (delimEndByP (charP_ '[') (charP_ ']') (charP_ ',') (charP 'x'))
            "[x,x,]"
            (Right (Seq.fromList "xx"))
        )
    , testParserCase
        (ParserCase "maybe delim none" (maybeDelimByP (charP_ '[') (charP_ ']') (charP_ ',') (charP 'x')) "" (Right Seq.empty))
    , testParserCase
        ( ParserCase
            "maybe delim some"
            (maybeDelimByP (charP_ '[') (charP_ ']') (charP_ ',') (charP 'x'))
            "[x,x]"
            (Right (Seq.fromList "xx"))
        )
    , testParserCase
        ( ParserCase
            "maybe delim end"
            (maybeDelimEndByP (charP_ '[') (charP_ ']') (charP_ ',') (charP 'x'))
            "[x,]"
            (Right (Seq.singleton 'x'))
        )
    , testParserCase
        ( ParserCase
            "prefix end by"
            (prefixEndByP (charP_ '>') (charP 'x') (charP_ '\n') (textP "done"))
            ">x\n>x\ndone"
            (Right (Seq.fromList "xx", "done"))
        )
    , testParserCase
        ( ParserCase
            "singletonOrP singleton"
            (singletonOrP (const 'z') (pure (Seq.singleton 'x') :: TestParser (Seq.Seq Char)))
            ""
            (Right 'x')
        )
    , testParserCase
        ( ParserCase
            "singletonOrP grouped"
            (singletonOrP (const 'z') (pure (Seq.fromList "xy") :: TestParser (Seq.Seq Char)))
            ""
            (Right 'z')
        )
    ]
 where
  iterExample = scopeP (0 :: Int) $ iterP $ do
    c <- headP
    if c == '!'
      then Just <$> get
      else modify (+ 1) *> pure Nothing

testPratt :: TestTree
testPratt = testGroup "pratt" (fmap testParserCase cases)
 where
  parser = prattP ops atomP :: TestParser Expr
  table = prattTable ops
  var name = ExprVar name
  atomP = var <$> takeWhile1P (\c -> c >= 'a' && c <= 'z')
  ops =
    [ prefixOpWith 35 (textP_ "-") (const (pure ExprNeg))
    , postfixOpWith 40 (textP_ "!") (const (pure ExprFact))
    , infixOpWith AssocRight 30 (textP_ "^") (const (pure ExprPow))
    , infixOp AssocLeft 10 (textP_ "+") ExprAdd
    , infixOp AssocNone 5 (textP_ "=") ExprEq
    ]
  cases =
    [ ParserCase "left assoc" parser "a+b+c" (Right (ExprAdd (ExprAdd (var "a") (var "b")) (var "c")))
    , ParserCase "right assoc" parser "a^b^c" (Right (ExprPow (var "a") (ExprPow (var "b") (var "c"))))
    , ParserCase "precedence" parser "a+b^c!" (Right (ExprAdd (var "a") (ExprPow (var "b") (ExprFact (var "c")))))
    , ParserCase "prefix postfix" parser "-a!" (Right (ExprNeg (ExprFact (var "a"))))
    , ParserCase "table" (prattWithTable table atomP) "a+b" (Right (ExprAdd (var "a") (var "b")))
    , ParserCase "at" (prattAtP 30 ops atomP) "a^b" (Right (ExprPow (var "a") (var "b")))
    , ParserCase "at table" (prattAtWithTable 30 table atomP) "a^b" (Right (ExprPow (var "a") (var "b")))
    ]

testNumbersStrings :: TestTree
testNumbersStrings =
  testGroup
    "numbersStrings"
    [ testParserCase (ParserCase "signedWith" (signedWithP T.reverse (textP "ab")) "-ab" (Right "ba"))
    , testParserCase (ParserCase "signed" (signedP uintP) "-12" (Right (-12)))
    , testParserCase (ParserCase "int" intP "-12" (Right (-12)))
    , testParserCase (ParserCase "uint" uintP "12" (Right 12))
    , testParserCase (ParserCase "dec" decP "-1.25" (Right (-(5 % 4))))
    , testParserCase (ParserCase "udec" udecP "1.25" (Right (5 % 4)))
    , testParserCase (ParserCase "sci" sciP "-1.25e2" (Right (S.scientific (-125) 0)))
    , testParserCase (ParserCase "usci" usciP "1.25e2" (Right (S.scientific 125 0)))
    , testParserCase (ParserCase "num int" numP "-12" (Right (Left (-12))))
    , testParserCase (ParserCase "unum sci" unumP "1.5" (Right (Right (S.scientific 15 (-1)))))
    , testParserCase (ParserCase "str" (strP '#') "#a\\#b#" (Right "a#b"))
    , testParserCase (ParserCase "doubleStr" doubleStrP "\"a\"" (Right "a"))
    , testParserCase (ParserCase "singleStr" singleStrP "'a'" (Right "a"))
    ]

testWhitespace :: TestTree
testWhitespace =
  testGroup
    "whitespace"
    [ testParserCase (ParserCase "space" (spaceP *> textP "x") " \n\tx" (Right "x"))
    , testParserCase (ParserCase "hspace" (hspaceP *> textP "x") " \tx" (Right "x"))
    , testParserCase (ParserCase "hspace1" (hspace1P *> textP "x") " \tx" (Right "x"))
    , testParserCase (ParserCase "eol crlf" eolP "\r\n" (Right "\r\n"))
    , testParserCase (ParserCase "eol lf" eolP "\n" (Right "\n"))
    , testParserCase (ParserCase "strip" (stripP (textP "x")) " x " (Right "x"))
    , testParserCase (ParserCase "stripStart" (stripStartP (textP "x")) " x" (Right "x"))
    , testParserCase (ParserCase "stripEnd" (stripEndP (textP "x")) "x " (Right "x"))
    , testParserCase (ParserCase "space1" (space1P *> textP "x") " x" (Right "x"))
    , testParserCase (ParserCase "strip1" (strip1P (textP "x")) " x " (Right "x"))
    , testParserCase (ParserCase "stripStart1" (stripStart1P (textP "x")) " x" (Right "x"))
    , testParserCase (ParserCase "stripEnd1" (stripEnd1P (textP "x")) "x " (Right "x"))
    , testParserCase
        (ParserCase "line comment" (Lex.skipLineComment "--" *> charP_ '\n' *> textP "x") "-- comment\nx" (Right "x"))
    , testParserCase (ParserCase "block comment" (Lex.skipBlockComment "{-" "-}" *> textP "x") "{- comment -}x" (Right "x"))
    ]

testErrorsAndTransforms :: TestTree
testErrorsAndTransforms =
  testGroup
    "errorsTransforms"
    [ testCase "parseT" $ runIdentity (parseT (textP "x" :: TestParser Text) "x") @?= Right "x"
    , testCase "parseI" $ parseI (textP "x" :: TestParser Text) "x" >>= (@?= Right "x")
    , testParserCase (ParserCase "scope" (scopeP (1 :: Int) get) "" (Right 1))
    , testParserCase
        (ParserCase "trans" (transP (`evalStateT` (2 :: Int)) (get :: ParserT Error (StateT Int Identity) Int)) "" (Right 2))
    , testParsePred "throw" (throwP "boom") "" (\case Left err -> errReason err == ReasonCustom "boom"; Right _ -> False)
    , testParsePred
        "label"
        (labelP "thing" (textP "x"))
        "y"
        ( \case
            Left err -> case errReason err of
              ReasonLabeled (Label "thing") _ -> True
              _ -> False
            Right _ -> False
        )
    , testParsePred
        "labelG"
        (chooseP [labelG "thing" (guarded (textP_ "x") (textP "y"))])
        "xz"
        ( \case
            Left err -> case errReason err of
              ReasonLabeled (Label "thing") _ -> True
              _ -> False
            Right _ -> False
        )
    , testParsePred
        "explain"
        (explainP explainAll (textP "x"))
        "y"
        ( \case
            Left err -> case errReason err of
              ReasonExplained "explained" _ _ -> True
              _ -> False
            Right _ -> False
        )
    , testCase "errSpan" $ case parse (textP "x" :: TestParser Text) "y" of
        Left err -> errSpan err @?= Span (Pos 1 0 1) (Pos 1 0 1)
        Right _ -> assertFailure "expected error"
    , testCase "render" $ case parse (throwP "boom" :: TestParser ()) "" of
        Left err -> T.isInfixOf "boom" (renderE "test" "" err) @?= True
        Right _ -> assertFailure "expected error"
    , testCase "errata" $ case parse (throwP "boom" :: TestParser ()) "" of
        Left err -> null (errataE "test" (const (1, 1)) err) @?= False
        Right _ -> assertFailure "expected error"
    , renderGolden
        "golden expect"
        (textP "x")
        "y"
        "--> input:1:2\n  |\n1 | y\n  |  ^\nError in range (1, 2)-(1, 2):\nExpected text: 'x' but found: 'y'\n"
    , renderGolden
        "golden custom"
        (throwP "boom")
        ""
        "--> input:1:1\n  |\n1 | \n  | ^\nError in range (1, 1)-(1, 1):\nCustom error:\n  boom\n"
    , renderGolden
        "golden indent"
        (charP_ '\n' *> indentAtP 2 indentation *> textP "x")
        "\n x"
        "--> input:2:2\n  |\n2 |  x\n  |  ^\nError in range (2, 2)-(2, 3):\nExpected indentation column 2 but got 1\n"
    ]
 where
  explainAll _ = Just ("explained", False)
  indentation = void (takeWhile1P (== ' '))
  renderGolden :: TestName -> TestParser a -> Text -> Text -> TestTree
  renderGolden name parser input expected = testCase name $ case parse parser input of
    Left err -> renderE "input" input err @?= expected
    Right _ -> assertFailure "expected error"

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
      , testCombinators
      , testPratt
      , testNumbersStrings
      , testWhitespace
      , testErrorsAndTransforms
      , testLayout
      , testScala
      , testExamples
      ]

testLayout :: TestTree
testLayout =
  testGroup
    "layout"
    [ testParserCase (ParserCase "span pos" (spanAroundP const (textP "a\nb")) "a\nb" (Right (Span (Pos 0 0 0) (Pos 3 1 1))))
    , testParserCase (ParserCase "indent at" (nl *> indentAtP 2 indentation *> textP "x") "\n  x" (Right "x"))
    , testParserCase (ParserCase "indent block" block "[\n  a\n  b\n]" (Right (Seq.fromList ["a", "b"])))
    , testParserCase
        (ParserCase "indent block end by" blockEnd "  val a\n  val b\n  done" (Right (Seq.fromList ["a", "b"], "done")))
    , testParserCase
        (ParserCase "indented after" (textP_ "=" *> indentedAfterP nl indentation (textP "x")) "=\n  x" (Right "x"))
    , testParserCase
        (ParserCase "indent block end by after" blockEndAfter "[\n  val a\n  done" (Right (Seq.singleton "a", "done")))
    , testParserCase (ParserCase "indented or inline" body "= x" (Right "x"))
    , testParserCase (ParserCase "indented or inline layout" body "=\n  x" (Right "x"))
    ]
 where
  nl = charP_ '\n'
  indentation = void (takeWhile1P (== ' '))
  optionalNl = chooseElseP [guarded nl (pure ())] (pure ())
  item = takeWhile1P (/= '\n') <* optionalNl
  block = charP_ '[' *> indentBlockAfterP nl indentation item <* charP_ ']'
  blockEndItem = takeWhile1P (/= '\n') <* nl
  blockEnd = indentBlockEndByP indentation (textP_ "val ") blockEndItem (textP "done")
  blockEndAfter = charP_ '[' *> indentBlockEndByAfterP nl indentation (textP_ "val ") blockEndItem (textP "done")
  body = textP_ "=" *> indentedOrInlineP nl indentation (dropWhileP (== ' ') *> takeWhile1P (/= '\n'))

testScala :: TestTree
testScala =
  testGroup
    "scala"
    [ scalaExpr "precedence" "1 + 2 * 3" (Scala.Add (Scala.Lit 1) (Scala.Mul (Scala.Lit 2) (Scala.Lit 3)))
    , scalaExpr "right assoc" "2 ^ 3 ^ 4" (Scala.Pow (Scala.Lit 2) (Scala.Pow (Scala.Lit 3) (Scala.Lit 4)))
    , scalaExpr "prefix postfix" "-2!" (Scala.Neg (Scala.Fact (Scala.Lit 2)))
    , scalaExpr
        "if"
        "if x == 0 then 1 else x + 1"
        (Scala.If (Scala.Eq (Scala.Var "x") (Scala.Lit 0)) (Scala.Lit 1) (Scala.Add (Scala.Var "x") (Scala.Lit 1)))
    , scalaExpr "lambda" "(x, y) => x + y" (Scala.Lam ["x", "y"] (Scala.Add (Scala.Var "x") (Scala.Var "y")))
    , scalaExpr
        "chain"
        "xs.map((x) => x + 1).filter(isPositive)"
        ( Scala.Apply
            ( Scala.Select
                ( Scala.Apply
                    (Scala.Select (Scala.Var "xs") "map")
                    [Scala.PosArg (Scala.Lam ["x"] (Scala.Add (Scala.Var "x") (Scala.Lit 1)))]
                )
                "filter"
            )
            [Scala.PosArg (Scala.Var "isPositive")]
        )
    , scalaExpr
        "record"
        "{ x = 1; y = x + 1; }"
        (Scala.Record [("x", Scala.Lit 1), ("y", Scala.Add (Scala.Var "x") (Scala.Lit 1))])
    , scalaProgram
        "program"
        "{ import data.math; def add(x, y) = x + y; enum Option a = Some(a) | None; }"
        [ Scala.Import ["data", "math"]
        , Scala.Def "add" [param "x", param "y"] (Scala.Add (Scala.Var "x") (Scala.Var "y"))
        , Scala.Data "Option" ["a"] [("Some", ["a"]), ("None", [])]
        ]
    , scalaProgram
        "layout object"
        "object Math\n  val one = 1\n  def inc(x) =\n    x + 1"
        [ Scala.Object
            "Math"
            [ Scala.Val "one" Nothing (Scala.Lit 1)
            , Scala.Def "inc" [param "x"] (Scala.Add (Scala.Var "x") (Scala.Lit 1))
            ]
        ]
    , scalaProgram
        "layout enum"
        "enum Option a =\n  Some(a)\n  None"
        [Scala.Data "Option" ["a"] [("Some", ["a"]), ("None", [])]]
    ]
 where
  scalaExpr name input expected = testCase name (parse Scala.expr input @?= Right expected)
  scalaProgram name input expected = testCase name (parse Scala.program input @?= Right expected)
  param name = Scala.Param name Nothing Nothing
