{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (Alternative (..), liftA2)
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Looksie
import Looksie.Examples
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

newtype Error = Error {unError :: String} deriving (Eq, Show, IsString)

type TestParser = Parser Error

type TestResult = Either (Err Error)

data ParserCase a = ParserCase !TestName !(TestParser a) !Text !(TestResult (a, Int))

err :: Range -> Reason Error (Err Error) -> TestResult (a, Int)
err ra re = Left (Err (ErrF ra re))

errAlt :: Range -> [(AltPhase, Range, Reason Error (Err Error))] -> TestResult (a, Int)
errAlt ra tups = Left (Err (ErrF ra (ReasonAlt (Seq.fromList (fmap f tups)))))
 where
  f (ap, ra', re) = (ap, Err (ErrF ra' re))

errInfix :: Range -> [(Int, InfixPhase, Range, Reason Error (Err Error))] -> TestResult (a, Int)
errInfix ra tups = Left (Err (ErrF ra (ReasonInfix (Seq.fromList (fmap f tups)))))
 where
  f (ix, ip, ra', re) = (ix, ip, Err (ErrF ra' re))

errLook :: Range -> Range -> Reason Error (Err Error) -> TestResult (a, Int)
errLook ra1 ra2 re = Left (Err (ErrF ra1 (ReasonLook (Err (ErrF ra2 re)))))

suc :: a -> Int -> TestResult (a, Int)
suc a i = Right (a, i)

testParserCase :: (Show a, Eq a) => ParserCase a -> TestTree
testParserCase (ParserCase name parser input expected) = testCase name $ do
  let parser' = liftA2 (,) parser dropAllP
      actual = parse parser' input
  actual @?= expected

testBasic :: TestTree
testBasic =
  testGroup "basic" $
    fmap
      (uncurry testGroup)
      [ ("empty", testEmpty)
      , ("pure", testPure)
      , ("fail", testFail)
      , ("head", testHead)
      , ("take", testTake)
      , ("drop", testDrop)
      , ("end", testEnd)
      , ("expectHead", testExpectHead)
      , ("expect", testExpect)
      , ("greedy", testGreedy)
      , ("greedy1", testGreedy1)
      , ("or", testOr)
      , ("alt", testAlt)
      , ("opt (empty)", testOptEmpty)
      , ("opt", testOpt)
      , ("bind (1)", testBind1)
      , ("bind (2)", testBind2)
      , ("throw", testThrow)
      , ("throw (consume)", testConsumeThrow)
      , ("throw (opt)", testOptThrow)
      , ("throw (opt consume)", testOptConsumeThrow)
      , ("throw (mixed)", testThrowMixed)
      , ("throw (mixed flip)", testThrowMixedFlip)
      , ("backtrack", testBacktrack)
      , ("look (pure)", testLookPure)
      , ("look (success)", testLookSuccess)
      , ("look (failure)", testLookFailure)
      , ("takeWhile", testTakeWhile)
      , ("takeWhile1", testTakeWhile1)
      , ("dropWhile", testDropWhile)
      , ("dropWhile1", testDropWhile1)
      , ("infixR", testInfixR)
      , ("infixL", testInfixL)
      ]

testEmpty :: [TestTree]
testEmpty = fmap testParserCase cases
 where
  parser = emptyP :: TestParser Int
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) ReasonEmpty)
    , ParserCase "non-empty" parser "hi" (err (Range 0 2) ReasonEmpty)
    ]

testPure :: [TestTree]
testPure = fmap testParserCase cases
 where
  parser = pure 'x'
  cases =
    [ ParserCase "empty" parser "" (suc 'x' 0)
    , ParserCase "non-empty" parser "hi" (suc 'x' 2)
    ]

testFail :: [TestTree]
testFail = fmap testParserCase cases
 where
  parser = fail "i give up" :: TestParser Int
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonFail "i give up"))
    , ParserCase "non-empty" parser "hi" (err (Range 0 2) (ReasonFail "i give up"))
    ]

testHead :: [TestTree]
testHead = fmap testParserCase cases
 where
  parser = headP
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonDemand 1 0))
    , ParserCase "non-empty" parser "hi" (suc 'h' 1)
    ]

testTake :: [TestTree]
testTake = fmap testParserCase cases
 where
  parser = takeP 2
  cases =
    [ ParserCase "len 0" parser "" (suc "" 0)
    , ParserCase "len 1" parser "h" (suc "h" 0)
    , ParserCase "len 2" parser "hi" (suc "hi" 0)
    , ParserCase "len 3" parser "hii" (suc "hi" 1)
    ]

testDrop :: [TestTree]
testDrop = fmap testParserCase cases
 where
  parser = dropP 2
  cases =
    [ ParserCase "len 0" parser "" (suc 0 0)
    , ParserCase "len 1" parser "h" (suc 1 0)
    , ParserCase "len 2" parser "hi" (suc 2 0)
    , ParserCase "len 3" parser "hii" (suc 2 1)
    ]

testEnd :: [TestTree]
testEnd = fmap testParserCase cases
 where
  parser = endP
  cases =
    [ ParserCase "empty" parser "" (suc () 0)
    , ParserCase "non-empty" parser "hi" (err (Range 0 2) (ReasonLeftover 2))
    ]

testExpectHead :: [TestTree]
testExpectHead = fmap testParserCase cases
 where
  parser = expectHeadP 'h'
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonExpect "h" ""))
    , ParserCase "non-empty" parser "hi" (suc 'h' 1)
    , ParserCase "non-match" parser "bye" (err (Range 1 3) (ReasonExpect "h" "b"))
    ]

testExpect :: [TestTree]
testExpect = fmap testParserCase cases
 where
  parser = expectP "hi"
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonExpect "hi" ""))
    , ParserCase "non-empty" parser "hi" (suc "hi" 0)
    , ParserCase "prefix" parser "hiya" (suc "hi" 2)
    , ParserCase "partial" parser "hey" (err (Range 2 3) (ReasonExpect "hi" "he"))
    , ParserCase "non-match" parser "bye" (err (Range 2 3) (ReasonExpect "hi" "by"))
    , ParserCase "short" parser "h" (err (Range 1 1) (ReasonExpect "hi" "h"))
    ]

testGreedy :: [TestTree]
testGreedy = fmap testParserCase cases
 where
  parser = fmap (T.pack . toList) (greedyP (expectHeadP 'h')) :: TestParser Text
  cases =
    [ ParserCase "empty" parser "" (suc "" 0)
    , ParserCase "non-empty" parser "hi" (suc "h" 1)
    , ParserCase "repeat" parser "hhi" (suc "hh" 1)
    , ParserCase "full" parser "hhh" (suc "hhh" 0)
    , ParserCase "non-match" parser "bye" (suc "" 3)
    ]

testGreedy1 :: [TestTree]
testGreedy1 = fmap testParserCase cases
 where
  parser = fmap (T.pack . toList) (greedy1P (expectHeadP 'h')) :: TestParser Text
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonExpect "h" ""))
    , ParserCase "non-empty" parser "hi" (suc "h" 1)
    , ParserCase "repeat" parser "hhi" (suc "hh" 1)
    , ParserCase "full" parser "hhh" (suc "hhh" 0)
    , ParserCase "non-match" parser "bye" (err (Range 1 3) (ReasonExpect "h" "b"))
    ]

testOr :: [TestTree]
testOr = fmap testParserCase cases
 where
  parser = expectP "h" <|> expectP "xi" :: TestParser Text
  cases =
    [ ParserCase "empty" parser "" $
        errAlt
          (Range 0 0)
          [ (AltPhaseBranch, Range 0 0, ReasonExpect "h" "")
          , (AltPhaseBranch, Range 0 0, ReasonExpect "xi" "")
          ]
    , ParserCase "first" parser "hi" (suc "h" 1)
    , ParserCase "second" parser "xi" (suc "xi" 0)
    , ParserCase "non-match" parser "bye" $
        errAlt
          (Range 0 3)
          [ (AltPhaseBranch, Range 1 3, ReasonExpect "h" "b")
          , (AltPhaseBranch, Range 2 3, ReasonExpect "xi" "by")
          ]
    ]

testAlt :: [TestTree]
testAlt = fmap testParserCase cases
 where
  parser = altP [expectP "h", "y" <$ headP, expectP "xi"] :: TestParser Text
  cases =
    [ ParserCase "empty" parser "" $
        errAlt
          (Range 0 0)
          [ (AltPhaseBranch, Range 0 0, ReasonExpect "h" "")
          , (AltPhaseBranch, Range 0 0, ReasonDemand 1 0)
          , (AltPhaseBranch, Range 0 0, ReasonExpect "xi" "")
          ]
    , ParserCase "first" parser "hi" (suc "h" 1)
    , ParserCase "middle" parser "zi" (suc "y" 1)
    , ParserCase "last" parser "xi" (suc "y" 1)
    ]

testOptEmpty :: [TestTree]
testOptEmpty = fmap testParserCase cases
 where
  parser = optP emptyP :: TestParser (Maybe ())
  cases =
    [ ParserCase "empty" parser "" (suc Nothing 0)
    , ParserCase "non-empty" parser "hi" (suc Nothing 2)
    ]

testOpt :: [TestTree]
testOpt = fmap testParserCase cases
 where
  parser = optP (expectHeadP 'h') :: TestParser (Maybe Char)
  cases =
    [ ParserCase "non-match empty" parser "" (suc Nothing 0)
    , ParserCase "match" parser "hi" (suc (Just 'h') 1)
    , ParserCase "non-match" parser "bye" (suc Nothing 3)
    ]

testBind1 :: [TestTree]
testBind1 = fmap testParserCase cases
 where
  parser = expectHeadP 'x' >>= \c -> pure [c, c]
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonExpect "x" ""))
    , ParserCase "first" parser "hi" (err (Range 1 2) (ReasonExpect "x" "h"))
    , ParserCase "second" parser "xi" (suc "xx" 1)
    ]

testBind2 :: [TestTree]
testBind2 = fmap testParserCase cases
 where
  parser = headP >>= \x -> if x == 'x' then pure 'y' else emptyP
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonDemand 1 0))
    , ParserCase "first" parser "hi" (err (Range 1 2) ReasonEmpty)
    , ParserCase "second" parser "xi" (suc 'y' 1)
    ]

testThrow :: [TestTree]
testThrow = fmap testParserCase cases
 where
  cust = Error "boo"
  parser = throwP cust :: TestParser Int
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonCustom cust))
    , ParserCase "non-empty" parser "hi" (err (Range 0 2) (ReasonCustom cust))
    ]

testConsumeThrow :: [TestTree]
testConsumeThrow = fmap testParserCase cases
 where
  cust = Error "boo"
  parser = headP *> throwP cust :: TestParser Int
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonDemand 1 0))
    , ParserCase "non-empty" parser "hi" (err (Range 1 2) (ReasonCustom cust))
    ]

testOptThrow :: [TestTree]
testOptThrow = fmap testParserCase cases
 where
  cust = Error "boo"
  parser = optP (throwP cust) :: TestParser (Maybe Int)
  cases =
    [ ParserCase "empty" parser "" (suc Nothing 0)
    , ParserCase "non-empty" parser "hi" (suc Nothing 2)
    ]

testOptConsumeThrow :: [TestTree]
testOptConsumeThrow = fmap testParserCase cases
 where
  cust = Error "boo"
  parser = optP (headP *> throwP cust) :: TestParser (Maybe Int)
  cases =
    [ ParserCase "empty" parser "" (suc Nothing 0)
    , ParserCase "non-empty" parser "hi" (suc Nothing 2)
    ]

testThrowMixed :: [TestTree]
testThrowMixed = fmap testParserCase cases
 where
  cust = Error "boo"
  parser = throwP cust <|> pure 1 :: TestParser Int
  cases =
    [ ParserCase "non-empty" parser "hi" (suc 1 2)
    ]

testThrowMixedFlip :: [TestTree]
testThrowMixedFlip = fmap testParserCase cases
 where
  cust = Error "boo"
  parser = pure 1 <|> throwP cust :: TestParser Int
  cases =
    [ ParserCase "non-empty" parser "hi" (suc 1 2)
    ]

testBacktrack :: [TestTree]
testBacktrack = fmap testParserCase cases
 where
  parser = (expectP "x" <|> expectP "xz") <* (expectP_ "z" *> endP)
  cases =
    [ ParserCase "non-empty" parser "xzz" (suc "xz" 0)
    ]

testLookPure :: [TestTree]
testLookPure = fmap testParserCase cases
 where
  parser = lookP (pure 1) :: TestParser Int
  cases =
    [ ParserCase "empty" parser "" (suc 1 0)
    , ParserCase "non-empty" parser "hi" (suc 1 2)
    ]

testLookSuccess :: [TestTree]
testLookSuccess = fmap testParserCase cases
 where
  parser = lookP headP
  cases =
    [ ParserCase "non-match empty" parser "" (errLook (Range 0 0) (Range 0 0) (ReasonDemand 1 0))
    , ParserCase "non-empty" parser "hi" (suc 'h' 2)
    ]

testLookFailure :: [TestTree]
testLookFailure = fmap testParserCase cases
 where
  cust = Error "boo"
  parser = lookP (headP *> throwP cust) :: TestParser Char
  cases =
    [ ParserCase "non-match empty" parser "" (errLook (Range 0 0) (Range 0 0) (ReasonDemand 1 0))
    , ParserCase "non-empty" parser "hi" (errLook (Range 0 2) (Range 1 2) (ReasonCustom cust))
    ]

testTakeWhile :: [TestTree]
testTakeWhile = fmap testParserCase cases
 where
  parser = takeWhileP (== 'h') :: TestParser Text
  cases =
    [ ParserCase "empty" parser "" (suc "" 0)
    , ParserCase "non-match" parser "i" (suc "" 1)
    , ParserCase "match" parser "hi" (suc "h" 1)
    , ParserCase "match 2" parser "hhi" (suc "hh" 1)
    , ParserCase "match end" parser "hh" (suc "hh" 0)
    ]

testTakeWhile1 :: [TestTree]
testTakeWhile1 = fmap testParserCase cases
 where
  parser = takeWhile1P (== 'h') :: TestParser Text
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) ReasonTakeNone)
    , ParserCase "non-match" parser "i" (err (Range 0 1) ReasonTakeNone)
    , ParserCase "match" parser "hi" (suc "h" 1)
    , ParserCase "match 2" parser "hhi" (suc "hh" 1)
    , ParserCase "match end" parser "hh" (suc "hh" 0)
    ]

testDropWhile :: [TestTree]
testDropWhile = fmap testParserCase cases
 where
  parser = dropWhileP (== 'h') :: TestParser Int
  cases =
    [ ParserCase "empty" parser "" (suc 0 0)
    , ParserCase "non-match" parser "i" (suc 0 1)
    , ParserCase "match" parser "hi" (suc 1 1)
    , ParserCase "match 2" parser "hhi" (suc 2 1)
    , ParserCase "match end" parser "hh" (suc 2 0)
    ]

testDropWhile1 :: [TestTree]
testDropWhile1 = fmap testParserCase cases
 where
  parser = dropWhile1P (== 'h') :: TestParser Int
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) ReasonTakeNone)
    , ParserCase "non-match" parser "i" (err (Range 0 1) ReasonTakeNone)
    , ParserCase "match" parser "hi" (suc 1 1)
    , ParserCase "match 2" parser "hhi" (suc 2 1)
    , ParserCase "match end" parser "hh" (suc 2 0)
    ]

testInfixR :: [TestTree]
testInfixR = fmap testParserCase cases
 where
  sub d = takeWhile1P (\c -> c == d || c == '+')
  parser = fmap (\(_, xx, yy) -> (xx, yy)) (infixRP (expectHeadP '+') (sub 'x') (sub 'y')) :: TestParser (Text, Text)
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) ReasonEmpty)
    , ParserCase "fail delim" parser "xy" (err (Range 0 2) ReasonEmpty)
    , ParserCase "fail first" parser "+y" (errInfix (Range 0 2) [(0, InfixPhaseLeft, Range 0 0, ReasonTakeNone)])
    , ParserCase "fail second" parser "x+" (errInfix (Range 0 2) [(1, InfixPhaseRight, Range 2 2, ReasonTakeNone)])
    , ParserCase "match" parser "x+y" (suc ("x", "y") 0)
    , ParserCase "match multi" parser "x++y" (suc ("x", "+y") 0)
    ]

testInfixL :: [TestTree]
testInfixL = fmap testParserCase cases
 where
  sub d = takeWhile1P (\c -> c == d || c == '+')
  parser = fmap (\(_, xx, yy) -> (xx, yy)) (infixLP (expectHeadP '+') (sub 'x') (sub 'y')) :: TestParser (Text, Text)
  cases =
    [ ParserCase "empty" parser "" (err (Range 0 0) ReasonEmpty)
    , ParserCase "match" parser "x+y" (suc ("x", "y") 0)
    , ParserCase "fail delim" parser "xy" (err (Range 0 2) ReasonEmpty)
    , ParserCase "fail first" parser "+y" (errInfix (Range 0 2) [(0, InfixPhaseLeft, Range 0 0, ReasonTakeNone)])
    , ParserCase "fail second" parser "x+" (errInfix (Range 0 2) [(1, InfixPhaseRight, Range 2 2, ReasonTakeNone)])
    , ParserCase "match multi" parser "x++y" (suc ("x+", "y") 0)
    ]

testJson :: TestTree
testJson = testGroup "json" (fmap test cases)
 where
  test (name, str, expected) = testCase name $ do
    let actual = either (const Nothing) Just (parse jsonParser str)
    actual @?= expected
  trueVal = JsonBool True
  falseVal = JsonBool False
  arrVal = JsonArray . Seq.fromList
  objVal = JsonObject . Seq.fromList
  cases =
    [ ("empty", "", Nothing)
    , ("bad", "bad", Nothing)
    , ("null", "null", Just JsonNull)
    , ("true", "true", Just trueVal)
    , ("false", "false", Just falseVal)
    , ("arr0", "[]", Just (arrVal []))
    , ("arr1", "[null]", Just (arrVal [JsonNull]))
    , ("arr2", "[null, false]", Just (arrVal [JsonNull, falseVal]))
    , ("arr3", "[null, false, true]", Just (arrVal [JsonNull, falseVal, trueVal]))
    , ("arrx", "[null,]", Nothing)
    , ("str0", "\"\"", Just (JsonString ""))
    , ("str1", "\"x\"", Just (JsonString "x"))
    , ("str2", "\"xy\"", Just (JsonString "xy"))
    , ("str3", "\"xyz\"", Just (JsonString "xyz"))
    , ("str4", "\"xy\\\"z\"", Just (JsonString "xy\"z"))
    , ("obj0", "{}", Just (objVal []))
    , ("obj1", "{\"x\": true}", Just (objVal [("x", trueVal)]))
    , ("obj2", "{\"x\": true, \"y\": false}", Just (objVal [("x", trueVal), ("y", falseVal)]))
    , ("num0", "0", Just (JsonNum 0))
    , ("num1", "123", Just (JsonNum 123))
    , ("num2", "123.45", Just (JsonNum 123.45))
    , ("num3", "1e100", Just (JsonNum (read "1e100")))
    , ("num4", "{\"x\": 1e100, \"y\": 123.45}", Just (objVal [("x", JsonNum (read "1e100")), ("y", JsonNum 123.45)]))
    ]

testSexp :: TestTree
testSexp = testGroup "sexp" (fmap test cases)
 where
  test (name, str, expected) = testCase name $ do
    let actual = either (const Nothing) Just (parse sexpParser str)
    actual @?= expected
  numSexp = SexpAtom (AtomInt 1)
  sciExpSexp = SexpAtom (AtomSci 1)
  identSexp = SexpAtom (AtomIdent "abc")
  stringSexp = SexpAtom (AtomString "xyz")
  sciSexp = SexpAtom (AtomSci 3.14)
  emptyList = SexpList Empty
  singletonList = SexpList (Seq.singleton numSexp)
  pairList = SexpList (Seq.fromList [numSexp, numSexp])
  cases =
    [ ("empty", "", Nothing)
    , ("empty list", "()", Just emptyList)
    , ("singleton list", "(1)", Just singletonList)
    , ("singleton empty list", "(())", Just (SexpList (Seq.fromList [emptyList])))
    , ("singleton nested list", "((1))", Just (SexpList (Seq.fromList [singletonList])))
    , ("num", "1", Just numSexp)
    , ("num neg", "-1", Just (SexpAtom (AtomInt -1)))
    , ("ident", "abc", Just identSexp)
    , ("string", "\"xyz\"", Just stringSexp)
    , ("sci", "3.14", Just sciSexp)
    , ("sci neg", "-3.14", Just (SexpAtom (AtomSci -3.14)))
    , ("sci neg exp", "314e-2", Just sciSexp)
    , ("sci neg exp 2", "31.4e-1", Just sciSexp)
    , ("sci pos exp 3", "0.314e1", Just sciSexp)
    , ("sci dec exp", "1.0", Just sciExpSexp)
    , ("sci exp", "1e0", Just sciExpSexp)
    , ("sci dec exp 2", "1.0e0", Just sciExpSexp)
    , ("multi list", "(1 abc \"xyz\" 3.14)", Just (SexpList (Seq.fromList [numSexp, identSexp, stringSexp, sciSexp])))
    , ("pair nested list", "((1 1) (1 1))", Just (SexpList (Seq.fromList [pairList, pairList])))
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Looksie"
      [ testBasic
      , testJson
      , testSexp
      ]
