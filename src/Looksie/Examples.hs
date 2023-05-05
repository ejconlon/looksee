{-# LANGUAGE OverloadedStrings #-}

module Looksie.Examples
  ( Value (..)
  , jsonParser
  , Arith (..)
  , arithParser
  )
where

import Data.Char (intToDigit, isAlpha)
import Data.Foldable (foldl')
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Looksie (Parser, altP, betweenP, expectP, greedy1P, infixRP, labelP, sepByP, stripP, takeWhile1P, takeWhileP)

data Value = ValueNull | ValueString !Text | ValueArray !(Seq Value) | ValueObject !(Seq (Text, Value))
  deriving stock (Eq, Ord, Show)

jsonParser :: Parser Void Value
jsonParser = valP
 where
  valP = stripP rawValP
  rawValP =
    altP
      [ labelP "null" nullP
      , labelP "str" strP
      , labelP "array" arrayP
      , labelP "object" objectP
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
  digitP = altP (fmap (\i -> let j = T.singleton (intToDigit i) in (i <$ expectP j)) [0 .. 9])
  identP = takeWhile1P isAlpha
  numP = foldl' addDigit 0 <$> greedy1P digitP
  binaryP op f = (\(_, a, b) -> f a b) <$> infixRP (expectP op) rootP rootP
  unaryP op f = expectP op *> fmap f rootP
  rawRootP =
    altP
      [ labelP "add" (binaryP "+" ArithAdd)
      , labelP "sub" (binaryP "-" ArithSub)
      , labelP "mul" (binaryP "*" ArithMul)
      , labelP "neg" (unaryP "-" ArithNeg)
      , labelP "paren" (betweenP (expectP "(") (expectP ")") rootP)
      , labelP "num" (ArithNum <$> numP)
      , labelP "var" (ArithVar <$> identP)
      ]
  rootP = stripP rawRootP
