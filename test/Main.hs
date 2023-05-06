{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Looksie
import Looksie.Examples
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- newtype Label = Label {unLabel :: String} deriving (Eq, Show, IsString)

-- newtype Error = Error {unError :: String} deriving (Eq, Show, IsString)

-- type TestState = OffsetStream Text

-- type TestBlock a b = PureMatchBlock Label TestState Error a b

-- type TestParser a = Parser Label TestState Error a

-- type TestResult a = ParseResult Label TestState Error a

-- type TestRawError = RawError Text Char

-- type TestParseError = ParseError Label TestState Error

-- data ParserCase a = ParserCase !TestName !(TestParser a) !Text !(Maybe (TestResult a))

-- data ExamineCase a b = ExamineCase !TestName !(TestBlock a b) !Text !(LookAheadTestResult Label)

-- fwd :: Int -> TestState -> TestState
-- fwd n (OffsetStream (Offset i) t) =
--   let m = min n (T.length t)
--   in  OffsetStream (Offset (i + m)) (T.drop m t)

-- sucRes :: TestState -> a -> Maybe (TestResult a)
-- sucRes st = Just . ParseResultSuccess . ParseSuccess st

-- errRes :: [TestParseError] -> Maybe (TestResult a)
-- errRes es = Just (ParseResultError (ParseErrorBundle (NESeq.unsafeFromSeq (Seq.fromList es))))

-- custErr :: TestState -> Error -> TestParseError
-- custErr endSt = ParseError emptyStack endSt . CompoundErrorCustom

-- stmErr :: TestState -> TestRawError -> TestParseError
-- stmErr endSt = ParseError emptyStack endSt . CompoundErrorStream . StreamError

-- failErr :: TestState -> Text -> TestParseError
-- failErr endSt = ParseError emptyStack endSt . CompoundErrorFail

-- markWith :: TestState -> TestParseError -> TestParseError
-- markWith s = markParseError (Mark Nothing s)

-- anyTokErr :: TestState -> TestParseError
-- anyTokErr s = markWith s (stmErr s RawErrorAnyToken)

-- anyChunkErr :: TestState -> TestParseError
-- anyChunkErr s = markWith s (stmErr s RawErrorAnyChunk)

-- matchTokErr :: TestState -> Char -> Maybe Char -> TestParseError
-- matchTokErr s x my = markWith s (stmErr (fwd 1 s) (RawErrorMatchToken x my))

-- matchChunkErr :: TestState -> Text -> Maybe Text -> TestParseError
-- matchChunkErr s x my = markWith s (stmErr (fwd (T.length x) s) (RawErrorMatchChunk x my))

-- matchEndErr :: TestState -> Char -> TestParseError
-- matchEndErr s x = markWith s (stmErr (fwd 1 s) (RawErrorMatchEnd x))

-- takeTokErr :: TestState -> Int -> Maybe Char -> TestParseError
-- takeTokErr s n my = markWith s (stmErr (fwd n s) (RawErrorTakeTokensWhile1 my))

-- dropTokErr :: TestState -> Int -> Maybe Char -> TestParseError
-- dropTokErr s n my = markWith s (stmErr (fwd n s) (RawErrorDropTokensWhile1 my))

-- testParserCase :: (Show a, Eq a) => ParserCase a -> TestTree
-- testParserCase (ParserCase name parser input expected) = testCase name $ do
--   let actual = runParser parser (newOffsetStream input)
--   actual @?= expected

-- testExamineCase :: ExamineCase a b -> TestTree
-- testExamineCase (ExamineCase name block input expected) = testCase name $ do
--   let actual = pureLookAheadTest block (newOffsetStream input)
--   actual @?= expected

-- test_empty :: [TestTree]
-- test_empty =
--   let parser = emptyParser :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" Nothing
--         , ParserCase "non-empty" parser "hi" Nothing
--         ]
--   in  fmap testParserCase cases

-- test_pure :: [TestTree]
-- test_pure =
--   let parser = pure (1 :: Int)
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") 1)
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 0 "hi") 1)
--         ]
--   in  fmap testParserCase cases

-- test_fail :: [TestTree]
-- test_fail =
--   let parser = fail "i give up" :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (errRes [failErr (OffsetStream 0 "") "i give up"])
--         , ParserCase "non-empty" parser "hi" (errRes [failErr (OffsetStream 0 "hi") "i give up"])
--         ]
--   in  fmap testParserCase cases

-- test_peek_token :: [TestTree]
-- test_peek_token =
--   let parser = peekToken
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") Nothing)
--         , ParserCase "match" parser "hi" (sucRes (OffsetStream 0 "hi") (Just 'h'))
--         ]
--   in  fmap testParserCase cases

-- test_pop_token :: [TestTree]
-- test_pop_token =
--   let parser = popToken
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") Nothing)
--         , ParserCase "match" parser "hi" (sucRes (OffsetStream 1 "i") (Just 'h'))
--         ]
--   in  fmap testParserCase cases

-- test_peek_chunk :: [TestTree]
-- test_peek_chunk =
--   let parser = peekChunk 2
--       cases =
--         [ ParserCase "len 0" parser "" (sucRes (OffsetStream 0 "") Nothing)
--         , ParserCase "len 1" parser "h" (sucRes (OffsetStream 0 "h") (Just "h"))
--         , ParserCase "len 2" parser "hi" (sucRes (OffsetStream 0 "hi") (Just "hi"))
--         , ParserCase "len 3" parser "hii" (sucRes (OffsetStream 0 "hii") (Just "hi"))
--         ]
--   in  fmap testParserCase cases

-- test_pop_chunk :: [TestTree]
-- test_pop_chunk =
--   let parser = popChunk 2
--       cases =
--         [ ParserCase "len 0" parser "" (sucRes (OffsetStream 0 "") Nothing)
--         , ParserCase "len 1" parser "h" (sucRes (OffsetStream 1 "") (Just "h"))
--         , ParserCase "len 2" parser "hi" (sucRes (OffsetStream 2 "") (Just "hi"))
--         , ParserCase "len 3" parser "hii" (sucRes (OffsetStream 2 "i") (Just "hi"))
--         ]
--   in  fmap testParserCase cases

-- test_drop_chunk :: [TestTree]
-- test_drop_chunk =
--   let parser = dropChunk 2
--       cases =
--         [ ParserCase "len 0" parser "" (sucRes (OffsetStream 0 "") Nothing)
--         , ParserCase "len 1" parser "h" (sucRes (OffsetStream 1 "") (Just 1))
--         , ParserCase "len 2" parser "hi" (sucRes (OffsetStream 2 "") (Just 2))
--         , ParserCase "len 3" parser "hii" (sucRes (OffsetStream 2 "i") (Just 2))
--         ]
--   in  fmap testParserCase cases

-- test_is_end :: [TestTree]
-- test_is_end =
--   let parser = isEnd
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") True)
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 0 "hi") False)
--         ]
--   in  fmap testParserCase cases

-- test_match_end :: [TestTree]
-- test_match_end =
--   let parser = matchEnd
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") ())
--         , ParserCase "non-empty" parser "hi" (errRes [matchEndErr (OffsetStream 0 "hi") 'h'])
--         ]
--   in  fmap testParserCase cases

-- test_any_token :: [TestTree]
-- test_any_token =
--   let parser = anyToken
--       cases =
--         [ ParserCase "empty" parser "" (errRes [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 1 "i") 'h')
--         ]
--   in  fmap testParserCase cases

-- test_any_chunk :: [TestTree]
-- test_any_chunk =
--   let parser = anyChunk 2 :: TestParser Text
--       cases =
--         [ ParserCase "len 0" parser "" (errRes [anyChunkErr (OffsetStream 0 "")])
--         , ParserCase "len 1" parser "h" (sucRes (OffsetStream 1 "") "h")
--         , ParserCase "len 2" parser "hi" (sucRes (OffsetStream 2 "") "hi")
--         , ParserCase "len 3" parser "hii" (sucRes (OffsetStream 2 "i") "hi")
--         ]
--   in  fmap testParserCase cases

-- test_match_token :: [TestTree]
-- test_match_token =
--   let parser = matchToken 'h'
--       cases =
--         [ ParserCase "empty" parser "" (errRes [matchTokErr (OffsetStream 0 "") 'h' Nothing])
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 1 "i") 'h')
--         , ParserCase "non-match" parser "bye" (errRes [matchTokErr (OffsetStream 0 "bye") 'h' (Just 'b')])
--         ]
--   in  fmap testParserCase cases

-- test_match_chunk :: [TestTree]
-- test_match_chunk =
--   let parser = matchChunk "hi"
--       cases =
--         [ ParserCase "empty" parser "" (errRes [matchChunkErr (OffsetStream 0 "") "hi" Nothing])
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 2 "") "hi")
--         , ParserCase "prefix" parser "hiya" (sucRes (OffsetStream 2 "ya") "hi")
--         , ParserCase "partial" parser "hey" (errRes [matchChunkErr (OffsetStream 0 "hey") "hi" (Just "he")])
--         , ParserCase "non-match" parser "bye" (errRes [matchChunkErr (OffsetStream 0 "bye") "hi" (Just "by")])
--         , ParserCase "short" parser "h" (errRes [matchChunkErr (OffsetStream 0 "h") "hi" (Just "h")])
--         ]
--   in  fmap testParserCase cases

-- test_greedy_star :: [TestTree]
-- test_greedy_star =
--   let parser = greedyStarParser (matchToken 'h') :: TestParser String
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") "")
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 1 "i") "h")
--         , ParserCase "repeat" parser "hhi" (sucRes (OffsetStream 2 "i") "hh")
--         , ParserCase "full" parser "hhh" (sucRes (OffsetStream 3 "") "hhh")
--         , ParserCase "non-match" parser "bye" (sucRes (OffsetStream 0 "bye") "")
--         ]
--   in  fmap testParserCase cases

-- test_greedy_star_unit :: [TestTree]
-- test_greedy_star_unit =
--   let parser = greedyStarParser_ (matchToken 'h')
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") ())
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 1 "i") ())
--         , ParserCase "repeat" parser "hhi" (sucRes (OffsetStream 2 "i") ())
--         , ParserCase "full" parser "hhh" (sucRes (OffsetStream 3 "") ())
--         , ParserCase "non-match" parser "bye" (sucRes (OffsetStream 0 "bye") ())
--         ]
--   in  fmap testParserCase cases

-- test_greedy_plus :: [TestTree]
-- test_greedy_plus =
--   let parser = greedyPlusParser (matchToken 'h') :: TestParser String
--       cases =
--         [ ParserCase "empty" parser "" (errRes [matchTokErr (OffsetStream 0 "") 'h' Nothing])
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 1 "i") "h")
--         , ParserCase "repeat" parser "hhi" (sucRes (OffsetStream 2 "i") "hh")
--         , ParserCase "full" parser "hhh" (sucRes (OffsetStream 3 "") "hhh")
--         , ParserCase "non-match" parser "bye" (errRes [matchTokErr (OffsetStream 0 "bye") 'h' (Just 'b')])
--         ]
--   in  fmap testParserCase cases

-- test_greedy_plus_unit :: [TestTree]
-- test_greedy_plus_unit =
--   let parser = greedyPlusParser_ (matchToken 'h')
--       cases =
--         [ ParserCase "empty" parser "" (errRes [matchTokErr (OffsetStream 0 "") 'h' Nothing])
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 1 "i") ())
--         , ParserCase "repeat" parser "hhi" (sucRes (OffsetStream 2 "i") ())
--         , ParserCase "full" parser "hhh" (sucRes (OffsetStream 3 "") ())
--         , ParserCase "non-match" parser "bye" (errRes [matchTokErr (OffsetStream 0 "bye") 'h' (Just 'b')])
--         ]
--   in  fmap testParserCase cases

-- test_or :: [TestTree]
-- test_or =
--   let parser = orParser (matchToken 'h') (matchToken 'x')
--       cases =
--         [ ParserCase "empty" parser "" $
--             errRes
--               [ matchTokErr (OffsetStream 0 "") 'h' Nothing
--               , matchTokErr (OffsetStream 0 "") 'x' Nothing
--               ]
--         , ParserCase "first" parser "hi" (sucRes (OffsetStream 1 "i") 'h')
--         , ParserCase "second" parser "xi" (sucRes (OffsetStream 1 "i") 'x')
--         , ParserCase "non-match" parser "bye" $
--             errRes
--               [ matchTokErr (OffsetStream 0 "bye") 'h' (Just 'b')
--               , matchTokErr (OffsetStream 0 "bye") 'x' (Just 'b')
--               ]
--         ]
--   in  fmap testParserCase cases

-- test_asum :: [TestTree]
-- test_asum =
--   let state = OffsetStream 1 "i"
--       parser = asum [matchToken 'h', 'y' <$ anyToken, matchToken 'x']
--       cases =
--         [ ParserCase "empty" parser "" $
--             errRes
--               [ matchTokErr (OffsetStream 0 "") 'h' Nothing
--               , anyTokErr (OffsetStream 0 "")
--               , matchTokErr (OffsetStream 0 "") 'x' Nothing
--               ]
--         , ParserCase "first" parser "hi" (sucRes state 'h')
--         , ParserCase "middle" parser "zi" (sucRes state 'y')
--         , ParserCase "last" parser "xi" (sucRes state 'y')
--         ]
--   in  fmap testParserCase cases

-- test_default_empty :: [TestTree]
-- test_default_empty =
--   let parser = defaultParser 'z' emptyParser
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") 'z')
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 0 "hi") 'z')
--         ]
--   in  fmap testParserCase cases

-- test_default :: [TestTree]
-- test_default =
--   let parser = defaultParser 'z' (matchToken 'h')
--       cases =
--         [ ParserCase "non-match empty" parser "" (sucRes (OffsetStream 0 "") 'z')
--         , ParserCase "match" parser "hi" (sucRes (OffsetStream 1 "i") 'h')
--         , ParserCase "non-match" parser "bye" (sucRes (OffsetStream 0 "bye") 'z')
--         ]
--   in  fmap testParserCase cases

-- test_bind_1 :: [TestTree]
-- test_bind_1 =
--   let state = OffsetStream 1 "i"
--       parser = matchToken 'x' >>= \c -> pure [c, c]
--       cases =
--         [ ParserCase "empty" parser "" (errRes [matchTokErr (OffsetStream 0 "") 'x' Nothing])
--         , ParserCase "first" parser "hi" (errRes [matchTokErr (OffsetStream 0 "hi") 'x' (Just 'h')])
--         , ParserCase "second" parser "xi" (sucRes state "xx")
--         ]
--   in  fmap testParserCase cases

-- test_bind_2 :: [TestTree]
-- test_bind_2 =
--   let state = OffsetStream 1 "i"
--       parser = anyToken >>= \x -> if x == 'x' then pure 'y' else emptyParser
--       cases =
--         [ ParserCase "empty" parser "" (errRes [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "first" parser "hi" Nothing
--         , ParserCase "second" parser "xi" (sucRes state 'y')
--         ]
--   in  fmap testParserCase cases

-- test_throw :: [TestTree]
-- test_throw =
--   let err = Error "boo"
--       parser = throwParser err :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (errRes [custErr (OffsetStream 0 "") err])
--         , ParserCase "non-empty" parser "hi" (errRes [custErr (OffsetStream 0 "hi") err])
--         ]
--   in  fmap testParserCase cases

-- test_consume_throw :: [TestTree]
-- test_consume_throw =
--   let err = Error "boo"
--       parser = anyToken *> throwParser err :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (errRes [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "non-empty" parser "hi" (errRes [custErr (OffsetStream 1 "i") err])
--         ]
--   in  fmap testParserCase cases

-- test_default_throw :: [TestTree]
-- test_default_throw =
--   let err = Error "boo"
--       parser = defaultParser 'z' (throwParser err)
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") 'z')
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 0 "hi") 'z')
--         ]
--   in  fmap testParserCase cases

-- test_default_consume_throw :: [TestTree]
-- test_default_consume_throw =
--   let err = Error "boo"
--       parser = defaultParser 'z' (anyToken *> throwParser err)
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") 'z')
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 0 "hi") 'z')
--         ]
--   in  fmap testParserCase cases

-- test_throw_mixed :: [TestTree]
-- test_throw_mixed =
--   let state = OffsetStream 0 "hi"
--       err = Error "boo"
--       parser = orParser (throwParser err) (pure 1) :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" (sucRes state 1)
--         ]
--   in  fmap testParserCase cases

-- test_throw_mixed_flip :: [TestTree]
-- test_throw_mixed_flip =
--   let state = OffsetStream 0 "hi"
--       err = Error "boo"
--       parser = orParser (pure 1) (throwParser err) :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" (sucRes state 1)
--         ]
--   in  fmap testParserCase cases

-- test_catch :: [TestTree]
-- test_catch =
--   let state = OffsetStream 0 "hi"
--       err = Error "boo"
--       parser = catchParser (throwParser err) (\(Error m) -> pure (if m == "boo" then 2 else 3)) :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" (sucRes state 2)
--         ]
--   in  fmap testParserCase cases

-- test_catch_recur :: [TestTree]
-- test_catch_recur =
--   let state = OffsetStream 0 "hi"
--       err1 = Error "boo"
--       err2 = Error "two"
--       parser = catchParser (throwParser err1) (const (throwParser err2)) :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" (errRes [custErr state err2])
--         ]
--   in  fmap testParserCase cases

-- test_silence_success :: [TestTree]
-- test_silence_success =
--   let state = OffsetStream 0 "hi"
--       parser = silenceParser (pure 1) :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" (sucRes state 1)
--         ]
--   in  fmap testParserCase cases

-- test_silence_fail :: [TestTree]
-- test_silence_fail =
--   let err = Error "boo"
--       parser = silenceParser (throwParser err) :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" Nothing
--         ]
--   in  fmap testParserCase cases

-- test_silence_empty :: [TestTree]
-- test_silence_empty =
--   let parser = silenceParser emptyParser :: TestParser Int
--       cases =
--         [ ParserCase "non-empty" parser "hi" Nothing
--         ]
--   in  fmap testParserCase cases

-- test_look_ahead_pure :: [TestTree]
-- test_look_ahead_pure =
--   let parser = lookAheadParser (pure 1) :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") 1)
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 0 "hi") 1)
--         ]
--   in  fmap testParserCase cases

-- test_look_ahead_success :: [TestTree]
-- test_look_ahead_success =
--   let parser = lookAheadParser anyToken
--       cases =
--         [ ParserCase "non-match empty" parser "" (errRes [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "non-empty" parser "hi" (sucRes (OffsetStream 0 "hi") 'h')
--         ]
--   in  fmap testParserCase cases

-- test_look_ahead_failure :: [TestTree]
-- test_look_ahead_failure =
--   let err = Error "boo"
--       parser = lookAheadParser (anyToken *> throwParser err) :: TestParser Char
--       cases =
--         [ ParserCase "non-match empty" parser "" (errRes [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "non-empty" parser "hi" (errRes [custErr (OffsetStream 1 "i") err])
--         ]
--   in  fmap testParserCase cases

-- test_commit :: [TestTree]
-- test_commit =
--   let parser = commitParser (void (matchToken 'h')) (matchChunk "hi") :: TestParser Text
--       cases =
--         [ ParserCase "non-match empty" parser "" Nothing
--         , ParserCase "non-match non-empty" parser "ho" (errRes [matchChunkErr (OffsetStream 0 "ho") "hi" (Just "ho")])
--         , ParserCase "match" parser "hi" (sucRes (OffsetStream 2 "") "hi")
--         ]
--   in  fmap testParserCase cases

-- test_take_while :: [TestTree]
-- test_take_while =
--   let parser = takeTokensWhile (== 'h') :: TestParser Text
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") "")
--         , ParserCase "non-match" parser "i" (sucRes (OffsetStream 0 "i") "")
--         , ParserCase "match" parser "hi" (sucRes (OffsetStream 1 "i") "h")
--         , ParserCase "match 2" parser "hhi" (sucRes (OffsetStream 2 "i") "hh")
--         , ParserCase "match end" parser "hh" (sucRes (OffsetStream 2 "") "hh")
--         ]
--   in  fmap testParserCase cases

-- test_take_while_1 :: [TestTree]
-- test_take_while_1 =
--   let parser = takeTokensWhile1 Nothing (== 'h') :: TestParser Text
--       cases =
--         [ ParserCase "empty" parser "" (errRes [takeTokErr (OffsetStream 0 "") 0 Nothing])
--         , ParserCase "non-match" parser "i" (errRes [takeTokErr (OffsetStream 0 "i") 0 (Just 'i')])
--         , ParserCase "match" parser "hi" (sucRes (OffsetStream 1 "i") "h")
--         , ParserCase "match 2" parser "hhi" (sucRes (OffsetStream 2 "i") "hh")
--         , ParserCase "match end" parser "hh" (sucRes (OffsetStream 2 "") "hh")
--         ]
--   in  fmap testParserCase cases

-- test_drop_while :: [TestTree]
-- test_drop_while =
--   let parser = dropTokensWhile (== 'h') :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (sucRes (OffsetStream 0 "") 0)
--         , ParserCase "non-match" parser "i" (sucRes (OffsetStream 0 "i") 0)
--         , ParserCase "match" parser "hi" (sucRes (OffsetStream 1 "i") 1)
--         , ParserCase "match 2" parser "hhi" (sucRes (OffsetStream 2 "i") 2)
--         , ParserCase "match end" parser "hh" (sucRes (OffsetStream 2 "") 2)
--         ]
--   in  fmap testParserCase cases

-- test_drop_while_1 :: [TestTree]
-- test_drop_while_1 =
--   let parser = dropTokensWhile1 Nothing (== 'h') :: TestParser Int
--       cases =
--         [ ParserCase "empty" parser "" (errRes [dropTokErr (OffsetStream 0 "") 0 Nothing])
--         , ParserCase "non-match" parser "i" (errRes [dropTokErr (OffsetStream 0 "i") 0 (Just 'i')])
--         , ParserCase "match" parser "hi" (sucRes (OffsetStream 1 "i") 1)
--         , ParserCase "match 2" parser "hhi" (sucRes (OffsetStream 2 "i") 2)
--         , ParserCase "match end" parser "hh" (sucRes (OffsetStream 2 "") 2)
--         ]
--   in  fmap testParserCase cases

-- simpleBlock :: TestBlock Char Text
-- simpleBlock =
--   MatchBlock
--     anyToken
--     (pure "default")
--     [ MatchCase (Just (Label "match x")) (== 'x') (anyToken $> "found x - consuming")
--     , MatchCase (Just (Label "match x dupe")) (== 'x') (pure "dupe x - leaving")
--     , MatchCase (Just (Label "match y")) (== 'y') (pure "found y - leaving")
--     ]

-- test_look_ahead_match :: [TestTree]
-- test_look_ahead_match =
--   let parser = lookAheadMatch simpleBlock
--       cases =
--         [ ParserCase "empty" parser "" (errRes [anyTokErr (OffsetStream 0 "")])
--         , ParserCase "non-match" parser "wz" (sucRes (OffsetStream 0 "wz") "default")
--         , ParserCase "match x" parser "xz" (sucRes (OffsetStream 1 "z") "found x - consuming")
--         , ParserCase "match y" parser "yz" (sucRes (OffsetStream 0 "yz") "found y - leaving")
--         ]
--   in  fmap testParserCase cases

-- test_look_ahead_examine :: [TestTree]
-- test_look_ahead_examine =
--   let xpositions = [MatchPos 0 (Just (Label "match x")), MatchPos 1 (Just (Label "match x dupe"))]
--       ypositions = [MatchPos 2 (Just (Label "match y"))]
--       cases =
--         [ ExamineCase "empty" simpleBlock "" LookAheadTestEmpty
--         , ExamineCase "non-match" simpleBlock "wz" LookAheadTestDefault
--         , ExamineCase "match x" simpleBlock "xz" (LookAheadTestMatches (NESeq.unsafeFromSeq (Seq.fromList xpositions)))
--         , ExamineCase "match y" simpleBlock "yz" (LookAheadTestMatches (NESeq.unsafeFromSeq (Seq.fromList ypositions)))
--         ]
--   in  fmap testExamineCase cases

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
      [ testJson
      , testSexp
      ]
