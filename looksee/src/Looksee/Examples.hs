{-# LANGUAGE OverloadedStrings #-}

-- | Example parsers
module Looksee.Examples
  ( Json (..)
  , jsonParser
  , Arith (..)
  , arithParser
  , Atom (..)
  , Sexp (..)
  , sexpParser
  )
where

import Data.Char (isAlpha)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import Looksee
  ( Assoc (..)
  , GuardedCaseT (..)
  , Parser
  , chooseP
  , decP
  , doubleStrP
  , guarded
  , guardedWith
  , infixOp
  , labelG
  , numP
  , prattTable
  , prattWithTable
  , prefixOp
  , sciP
  , sepByP
  , space1P
  , stripEndP
  , stripP
  , takeWhile1P
  , textP_
  )

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
    chooseP valCases
  valCases =
    [ labelG "null" (guarded (textP_ "null") (pure JsonNull))
    , labelG "false" (guarded (textP_ "false") (pure (JsonBool False)))
    , labelG "true" (guarded (textP_ "true") (pure (JsonBool True)))
    , labelG "str" (guardedWith doubleStrP (pure . JsonString))
    , labelG "array" (guarded (stripEndP (textP_ "[")) arrayBodyP)
    , labelG "object" (guarded (stripEndP (textP_ "{")) objectBodyP)
    , labelG "num" (guardedWith sciP (pure . JsonNum))
    ]
  valItemCases = fmap stripCase valCases
  stripCase (GuardedCaseT guard parser) = GuardedCaseT guard (stripEndP . parser)
  commaP = guarded (stripEndP (textP_ ",")) (pure ())
  arrayBodyP = JsonArray <$> sepByP commaP valItemCases <* textP_ "]"
  pairCase = guardedWith doubleStrP $ \s -> do
    stripP (textP_ ":")
    v <- stripEndP valP
    pure (s, v)
  objectBodyP = JsonObject <$> sepByP commaP [pairCase] <* textP_ "}"

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
  rootP = prattWithTable opTable atomP
  opTable = prattTable ops
  ops =
    [ prefixOp 30 (stripEndP (textP_ "-")) ArithNeg
    , infixOp AssocLeft 20 (stripP (textP_ "*")) ArithMul
    , infixOp AssocLeft 10 (stripP (textP_ "+")) ArithAdd
    , infixOp AssocLeft 10 (stripP (textP_ "-")) ArithSub
    ]
  atomP =
    chooseP
      [ labelG "paren" (guarded (stripEndP (textP_ "(")) (stripEndP rootP <* textP_ ")"))
      , labelG "num" (guardedWith decP (pure . ArithNum))
      , labelG "var" (guardedWith identP (pure . ArithVar))
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
  numAtomP = either AtomInt AtomSci <$> numP
  atomCases =
    [ labelG "string" (guardedWith doubleStrP (pure . AtomString))
    , labelG "num" (guardedWith numAtomP (pure . id))
    , labelG "ident" (guardedWith identP (pure . AtomIdent))
    ]
  spaceCase = guarded space1P (pure ())
  listP = SexpList <$> stripEndP (sepByP spaceCase rootCases) <* textP_ ")"
  rootP =
    chooseP rootCases
  rootCases =
    labelG "list" (guarded (stripEndP (textP_ "(")) listP) : fmap (labelG "atom" . fmap SexpAtom) atomCases
