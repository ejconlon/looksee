{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (Alternative (..), liftA2)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString)
import Data.Text (Text)
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
      ]

testEmpty :: [TestTree]
testEmpty =
  let parser = emptyP :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" (err (Range 0 0) ReasonEmpty)
        , ParserCase "non-empty" parser "hi" (err (Range 0 2) ReasonEmpty)
        ]
  in  fmap testParserCase cases

testPure :: [TestTree]
testPure =
  let parser = pure 'x'
      cases =
        [ ParserCase "empty" parser "" (suc 'x' 0)
        , ParserCase "non-empty" parser "hi" (suc 'x' 2)
        ]
  in  fmap testParserCase cases

testFail :: [TestTree]
testFail =
  let parser = fail "i give up" :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonFail "i give up"))
        , ParserCase "non-empty" parser "hi" (err (Range 0 2) (ReasonFail "i give up"))
        ]
  in  fmap testParserCase cases

testHead :: [TestTree]
testHead =
  let parser = headP
      cases =
        [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonDemand 1 0))
        , ParserCase "non-empty" parser "hi" (suc 'h' 1)
        ]
  in  fmap testParserCase cases

testTake :: [TestTree]
testTake =
  let parser = takeP 2
      cases =
        [ ParserCase "len 0" parser "" (suc "" 0)
        , ParserCase "len 1" parser "h" (suc "h" 0)
        , ParserCase "len 2" parser "hi" (suc "hi" 0)
        , ParserCase "len 3" parser "hii" (suc "hi" 1)
        ]
  in  fmap testParserCase cases

testDrop :: [TestTree]
testDrop =
  let parser = dropP 2
      cases =
        [ ParserCase "len 0" parser "" (suc 0 0)
        , ParserCase "len 1" parser "h" (suc 1 0)
        , ParserCase "len 2" parser "hi" (suc 2 0)
        , ParserCase "len 3" parser "hii" (suc 2 1)
        ]
  in  fmap testParserCase cases

testEnd :: [TestTree]
testEnd =
  let parser = endP
      cases =
        [ ParserCase "empty" parser "" (suc () 0)
        , ParserCase "non-empty" parser "hi" (err (Range 0 2) (ReasonLeftover 2))
        ]
  in  fmap testParserCase cases

testExpectHead :: [TestTree]
testExpectHead =
  let parser = expectHeadP 'h'
      cases =
        [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonExpect "h" ""))
        , ParserCase "non-empty" parser "hi" (suc () 1)
        , ParserCase "non-match" parser "bye" (err (Range 1 3) (ReasonExpect "h" "b"))
        ]
  in  fmap testParserCase cases

testExpect :: [TestTree]
testExpect =
  let parser = expectP "hi"
      cases =
        [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonExpect "hi" ""))
        , ParserCase "non-empty" parser "hi" (suc () 0)
        , ParserCase "prefix" parser "hiya" (suc () 2)
        , ParserCase "partial" parser "hey" (err (Range 2 3) (ReasonExpect "hi" "he"))
        , ParserCase "non-match" parser "bye" (err (Range 2 3) (ReasonExpect "hi" "by"))
        , ParserCase "short" parser "h" (err (Range 1 1) (ReasonExpect "hi" "h"))
        ]
  in  fmap testParserCase cases

testGreedy :: [TestTree]
testGreedy =
  let parser = fmap length (greedyP (expectHeadP 'h')) :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" (suc 0 0)
        , ParserCase "non-empty" parser "hi" (suc 1 1)
        , ParserCase "repeat" parser "hhi" (suc 2 1)
        , ParserCase "full" parser "hhh" (suc 3 0)
        , ParserCase "non-match" parser "bye" (suc 0 3)
        ]
  in  fmap testParserCase cases

testGreedy1 :: [TestTree]
testGreedy1 =
  let parser = fmap length (greedy1P (expectHeadP 'h')) :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" (err (Range 0 0) (ReasonExpect "h" ""))
        , ParserCase "non-empty" parser "hi" (suc 1 1)
        , ParserCase "repeat" parser "hhi" (suc 2 1)
        , ParserCase "full" parser "hhh" (suc 3 0)
        , ParserCase "non-match" parser "bye" (err (Range 1 3) (ReasonExpect "h" "b"))
        ]
  in  fmap testParserCase cases

testOr :: [TestTree]
testOr =
  let parser = ("h" <$ expectHeadP 'h') <|> ("xi" <$ expectP "xi") :: TestParser String
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
  in  fmap testParserCase cases

testAlt :: [TestTree]
testAlt =
  let parser = altP ["h" <$ expectHeadP 'h', "y" <$ headP, "xi" <$ expectP "xi"] :: TestParser String
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
  in  fmap testParserCase cases

testOptEmpty :: [TestTree]
testOptEmpty =
  let parser = optP emptyP :: TestParser (Maybe ())
      cases =
        [ ParserCase "empty" parser "" (suc Nothing 0)
        , ParserCase "non-empty" parser "hi" (suc Nothing 2)
        ]
  in  fmap testParserCase cases

testOpt :: [TestTree]
testOpt =
  let parser = optP (expectHeadP 'h') :: TestParser (Maybe ())
      cases =
        [ ParserCase "non-match empty" parser "" (suc Nothing 0)
        , ParserCase "match" parser "hi" (suc (Just ()) 1)
        , ParserCase "non-match" parser "bye" (suc Nothing 3)
        ]
  in  fmap testParserCase cases

-- test_bind_1 :: [TestTree]
-- test_bind_1 =
--   let state = OffsetStream 1 "i"
--       parser = matchToken 'x' >>= \c -> pure [c, c]
--       cases =
--         [ ParserCase "empty" parser "" (err [matchTokErr (OffsetStream 0 "") 'x' Nothing])
--         , ParserCase "first" parser "hi" (err [matchTokErr (OffsetStream 0 "hi") 'x' (Just 'h')])
--         , ParserCase "second" parser "xi" (suc state "xx")
--         ]
--   in  fmap testParserCase cases

-- test_bind_2 :: [TestTree]
-- test_bind_2 =
--   let state = OffsetStream 1 "i"
--       parser = anyToken >>= \x -> if x == 'x' then pure 'y' else emptyParser
--       cases =
--         [ ParserCase "empty" parser "" (err [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "first" parser "hi" Nothing
--         , ParserCase "second" parser "xi" (suc state 'y')
--         ]
--   in  fmap testParserCase cases

-- test_throw :: [TestTree]
-- test_throw =
--   let err = Error "boo"
--       parser = throwParser err :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (err [custErr (OffsetStream 0 "") err])
--         , ParserCase "non-empty" parser "hi" (err [custErr (OffsetStream 0 "hi") err])
--         ]
--   in  fmap testParserCase cases

-- test_consume_throw :: [TestTree]
-- test_consume_throw =
--   let err = Error "boo"
--       parser = anyToken *> throwParser err :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (err [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "non-empty" parser "hi" (err [custErr (OffsetStream 1 "i") err])
--         ]
--   in  fmap testParserCase cases

-- test_default_throw :: [TestTree]
-- test_default_throw =
--   let err = Error "boo"
--       parser = defaultParser 'z' (throwParser err)
--       cases =
--         [ ParserCase "empty" parser "" (suc (OffsetStream 0 "") 'z')
--         , ParserCase "non-empty" parser "hi" (suc (OffsetStream 0 "hi") 'z')
--         ]
--   in  fmap testParserCase cases

-- test_default_consume_throw :: [TestTree]
-- test_default_consume_throw =
--   let err = Error "boo"
--       parser = defaultParser 'z' (anyToken *> throwParser err)
--       cases =
--         [ ParserCase "empty" parser "" (suc (OffsetStream 0 "") 'z')
--         , ParserCase "non-empty" parser "hi" (suc (OffsetStream 0 "hi") 'z')
--         ]
--   in  fmap testParserCase cases

-- test_throw_mixed :: [TestTree]
-- test_throw_mixed =
--   let state = OffsetStream 0 "hi"
--       err = Error "boo"
--       parser = orParser (throwParser err) (pure 1) :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" (suc state 1)
--         ]
--   in  fmap testParserCase cases

-- test_throw_mixed_flip :: [TestTree]
-- test_throw_mixed_flip =
--   let state = OffsetStream 0 "hi"
--       err = Error "boo"
--       parser = orParser (pure 1) (throwParser err) :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" (suc state 1)
--         ]
--   in  fmap testParserCase cases

-- test_look_ahead_pure :: [TestTree]
-- test_look_ahead_pure =
--   let parser = lookAheadParser (pure 1) :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (suc (OffsetStream 0 "") 1)
--         , ParserCase "non-empty" parser "hi" (suc (OffsetStream 0 "hi") 1)
--         ]
--   in  fmap testParserCase cases

-- test_look_ahead_success :: [TestTree]
-- test_look_ahead_success =
--   let parser = lookAheadParser anyToken
--       cases =
--         [ ParserCase "non-match empty" parser "" (err [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "non-empty" parser "hi" (suc (OffsetStream 0 "hi") 'h')
--         ]
--   in  fmap testParserCase cases

-- test_look_ahead_failure :: [TestTree]
-- test_look_ahead_failure =
--   let err = Error "boo"
--       parser = lookAheadParser (anyToken *> throwParser err) :: TestParser Char
--       cases =
--         [ ParserCase "non-match empty" parser "" (err [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "non-empty" parser "hi" (err [custErr (OffsetStream 1 "i") err])
--         ]
--   in  fmap testParserCase cases

-- test_take_while :: [TestTree]
-- test_take_while =
--   let parser = takeTokensWhile (== 'h') :: TestParser Text
--       cases =
--         [ ParserCase "empty" parser "" (suc (OffsetStream 0 "") "")
--         , ParserCase "non-match" parser "i" (suc (OffsetStream 0 "i") "")
--         , ParserCase "match" parser "hi" (suc (OffsetStream 1 "i") "h")
--         , ParserCase "match 2" parser "hhi" (suc (OffsetStream 2 "i") "hh")
--         , ParserCase "match end" parser "hh" (suc (OffsetStream 2 "") "hh")
--         ]
--   in  fmap testParserCase cases

-- test_take_while_1 :: [TestTree]
-- test_take_while_1 =
--   let parser = takeTokensWhile1 Nothing (== 'h') :: TestParser Text
--       cases =
--         [ ParserCase "empty" parser "" (err [takeTokErr (OffsetStream 0 "") 0 Nothing])
--         , ParserCase "non-match" parser "i" (err [takeTokErr (OffsetStream 0 "i") 0 (Just 'i')])
--         , ParserCase "match" parser "hi" (suc (OffsetStream 1 "i") "h")
--         , ParserCase "match 2" parser "hhi" (suc (OffsetStream 2 "i") "hh")
--         , ParserCase "match end" parser "hh" (suc (OffsetStream 2 "") "hh")
--         ]
--   in  fmap testParserCase cases

-- test_drop_while :: [TestTree]
-- test_drop_while =
--   let parser = dropTokensWhile (== 'h') :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (suc (OffsetStream 0 "") 0)
--         , ParserCase "non-match" parser "i" (suc (OffsetStream 0 "i") 0)
--         , ParserCase "match" parser "hi" (suc (OffsetStream 1 "i") 1)
--         , ParserCase "match 2" parser "hhi" (suc (OffsetStream 2 "i") 2)
--         , ParserCase "match end" parser "hh" (suc (OffsetStream 2 "") 2)
--         ]
--   in  fmap testParserCase cases

-- test_drop_while_1 :: [TestTree]
-- test_drop_while_1 =
--   let parser = dropTokensWhile1 Nothing (== 'h') :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (err [dropTokErr (OffsetStream 0 "") 0 Nothing])
--         , ParserCase "non-match" parser "i" (err [dropTokErr (OffsetStream 0 "i") 0 (Just 'i')])
--         , ParserCase "match" parser "hi" (suc (OffsetStream 1 "i") 1)
--         , ParserCase "match 2" parser "hhi" (suc (OffsetStream 2 "i") 2)
--         , ParserCase "match end" parser "hh" (suc (OffsetStream 2 "") 2)
--         ]
--   in  fmap testParserCase cases

testJsonCase :: TestName -> Text -> Maybe Json -> TestTree
testJsonCase name str expected = testCase ("json " <> name) $ do
  let actual = either (const Nothing) Just (parse jsonParser str)
  actual @?= expected

testJson :: TestTree
testJson =
  testGroup "json" $
    let trueVal = JsonBool True
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
          -- TODO implement Scientific parsing
          -- , ("num3", "1e100", Just (JsonNum (read "1e100")))
          -- , ("num4", "{\"x\": 1e100, \"y\": 123.45}", Just (objVal [("x", JsonNum (read "1e100")), ("y", JsonNum 123.45)]))
          ]
    in  fmap (\(n, s, e) -> testJsonCase n s e) cases

testSexpCase :: TestName -> Text -> Maybe Sexp -> TestTree
testSexpCase name str expected = testCase ("sexp " <> name) $ do
  let actual = either (const Nothing) Just (parse sexpParser str)
  actual @?= expected

testSexp :: TestTree
testSexp =
  testGroup "sexp" $
    let numSexp = SexpAtom (AtomInt 1)
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
          , ("sci dec exp", "1.0", Just sciExpSexp)
          , -- TODO implement Scientific parsing
            -- , ("sci exp", "1e0", Just sciExpSexp)
            -- , ("sci pos exp", "+1e0", Just sciExpSexp)
            -- , ("sci dec exp 2", "1.0e0", Just sciExpSexp)
            ("multi list", "(1 abc \"xyz\" 3.14)", Just (SexpList (Seq.fromList [numSexp, identSexp, stringSexp, sciSexp])))
          , ("pair nested list", "((1 1) (1 1))", Just (SexpList (Seq.fromList [pairList, pairList])))
          ]
    in  fmap (\(n, s, e) -> testSexpCase n s e) cases

main :: IO ()
main =
  defaultMain $
    testGroup
      "Looksie"
      [ testBasic
      , testJson
      , testSexp
      ]
