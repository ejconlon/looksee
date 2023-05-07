{-# LANGUAGE OverloadedStrings #-}

-- | Example parsers
module Looksie.Examples
  ( Json (..)
  , jsonParser
  , Arith (..)
  , arithParser
  , Atom (..)
  , Sexp (..)
  , sexpParser
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAlpha)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import Looksie (Parser, altP, betweenP, decP, doubleStrP, expectP_, infixRP, intP, labelP, sciP, sepByP, space1P, stripEndP, stripP, takeWhile1P)

-- | A JSON value
data Json
  = JsonNull
  | JsonString !Text
  | JsonArray !(Seq Json)
  | JsonObject !(Seq (Text, Json))
  | JsonNum !Scientific
  | JsonBool !Bool
  deriving stock (Eq, Ord, Show)

-- | A JSON parser (modulo some differences in numeric parsing)
jsonParser :: Parser Void Json
jsonParser = stripP valP
 where
  valP =
    altP
      [ labelP "null" nullP
      , labelP "bool" boolP
      , labelP "str" strP
      , labelP "array" arrayP
      , labelP "object" objectP
      , labelP "num" numP
      ]
  boolP = JsonBool <$> (False <$ expectP_ "false" <|> True <$ expectP_ "true")
  numP = JsonNum <$> sciP
  nullP = JsonNull <$ expectP_ "null"
  strP = JsonString <$> doubleStrP
  arrayP = JsonArray <$> betweenP (stripEndP (expectP_ "[")) (expectP_ "]") (sepByP (stripEndP (expectP_ ",")) (stripEndP valP))
  pairP = do
    s <- doubleStrP
    stripP (expectP_ ":")
    v <- valP
    pure (s, v)
  objectP = JsonObject <$> betweenP (stripEndP (expectP_ "{")) (expectP_ "}") (sepByP (stripEndP (expectP_ ",")) (stripEndP pairP))

-- | An arithmetic expression
data Arith
  = ArithNum !Rational
  | ArithVar !Text
  | ArithNeg Arith
  | ArithMul Arith Arith
  | ArithAdd Arith Arith
  | ArithSub Arith Arith
  deriving stock (Eq, Ord, Show)

-- | A parser for arithmetic expressions
arithParser :: Parser Void Arith
arithParser = stripP rootP
 where
  identP = takeWhile1P isAlpha
  binaryP op f = (\(_, a, b) -> f a b) <$> infixRP (stripEndP (expectP_ op)) (stripEndP rootP) rootP
  unaryP op f = expectP_ op *> fmap f rootP
  rootP =
    altP
      [ labelP "add" (binaryP "+" ArithAdd)
      , labelP "sub" (binaryP "-" ArithSub)
      , labelP "mul" (binaryP "*" ArithMul)
      , labelP "neg" (unaryP "-" ArithNeg)
      , labelP "paren" (betweenP (stripEndP (expectP_ "(")) (expectP_ ")") (stripEndP rootP))
      , labelP "num" (ArithNum <$> decP)
      , labelP "var" (ArithVar <$> identP)
      ]

-- | Leaves of S-expression trees
data Atom
  = AtomIdent !Text
  | AtomString !Text
  | AtomInt !Integer
  | AtomSci !Scientific
  deriving stock (Eq, Ord, Show)

-- | An S-expression
data Sexp
  = SexpAtom !Atom
  | SexpList !(Seq Sexp)
  deriving stock (Eq, Ord, Show)

-- | A parser for S-expressions
sexpParser :: Parser Void Sexp
sexpParser = stripP rootP
 where
  identP = takeWhile1P isAlpha
  atomP =
    altP
      [ labelP "ident" (AtomIdent <$> identP)
      , labelP "string" (AtomString <$> doubleStrP)
      , labelP "int" (AtomInt <$> intP)
      , labelP "sci" (AtomSci <$> sciP)
      ]
  listP = betweenP (stripEndP (expectP_ "(")) (expectP_ ")") (stripEndP (sepByP space1P rootP))
  rootP =
    altP
      [ labelP "atom" (SexpAtom <$> atomP)
      , labelP "list" (SexpList <$> listP)
      ]
