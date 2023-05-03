{-# LANGUAGE OverloadedStrings #-}

module Looksie.Examples
  ( Value (..)
  , jsonParser
  , Arith (..)
  , arithParser
  )
where

import Data.Char (isAlpha)
import Data.Foldable (foldl')
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Looksie (Parser, altP, betweenP, expectP, greedy1P, infixP, sepByP, stripP, takeWhile1P, takeWhileP)

data Value = ValueNull | ValueString !Text | ValueArray !(Seq Value) | ValueObject !(Seq (Text, Value))
  deriving stock (Eq, Ord, Show)

jsonParser :: Parser Void Value
jsonParser = valP
 where
  valP = stripP rawValP
  rawValP =
    altP
      "value"
      [ ("null", nullP)
      , ("str", strP)
      , ("array", arrayP)
      , ("object", objectP)
      ]
  nullP = ValueNull <$ expectP "null"
  rawStrP = betweenP (expectP "\"") (expectP "\"") (takeWhileP (/= '"'))
  strP = ValueString <$> rawStrP
  arrayP = ValueArray <$> betweenP (expectP "[") (expectP "]") (sepByP (expectP ",") valP)
  rawPairP = do
    s <- rawStrP
    stripP (expectP ":")
    v <- rawValP
    pure (s, v)
  pairP = stripP rawPairP
  objectP = ValueObject <$> betweenP (expectP "{") (expectP "}") (sepByP (expectP ",") pairP)

data Arith
  = ArithNum !Int
  | ArithVar !Text
  | ArithNeg Arith
  | ArithMul Arith Arith
  | ArithAdd Arith Arith
  | ArithSub Arith Arith
  deriving stock (Eq, Ord, Show)

arithParser :: Parser Void Arith
arithParser = rootP
 where
  addDigit n d = n * 10 + d
  digitP = altP "digit" (fmap (\i -> let j = T.pack (show i) in (j, i <$ expectP j)) [0 .. 9])
  identP = takeWhile1P isAlpha
  numP = foldl' addDigit 0 <$> greedy1P digitP
  binaryP op f = uncurry f <$> infixP op rootP rootP
  unaryP op f = expectP op *> fmap f rootP
  rawRootP =
    altP
      "root"
      [ ("add", binaryP "+" ArithAdd)
      , ("sub", binaryP "-" ArithSub)
      , ("mul", binaryP "*" ArithMul)
      , ("neg", unaryP "-" ArithNeg)
      , ("paren", betweenP (expectP "(") (expectP ")") rootP)
      , ("num", ArithNum <$> numP)
      , ("var", ArithVar <$> identP)
      ]
  rootP = stripP rawRootP
