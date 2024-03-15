{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Mello.Syntax
  ( Symbol (..)
  , Atom (..)
  , AtomType (..)
  , Brace (..)
  , Doc (..)
  , SexpF (..)
  , Sexp (..)
  , SexpType (..)
  , pattern SexpAtom
  , pattern SexpList
  , pattern SexpQuote
  , pattern SexpUnquote
  , pattern SexpDoc
  , IsSexp (..)
  )
where

import Bowtie (Anno (..), Memo (..))
import Bowtie qualified as B
import Data.Foldable (toList)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Mello.Text (Brace (..), closeBraceChar, openBraceChar)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

newtype Symbol = Symbol {unSymbol :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Leaves of S-expression trees
data Atom
  = AtomSym !Symbol
  | AtomInt !Integer
  | AtomSci !Scientific
  | AtomStr !Text
  | AtomChar !Char
  deriving stock (Eq, Ord, Show)

atomNotNumErr :: a
atomNotNumErr = error "Atom not num"

-- It's a sin to define an instance this partial but it's really
-- useful to have literal syntax.
instance Num Atom where
  (+) = \case
    AtomInt x -> \case
      AtomInt y -> AtomInt (x + y)
      AtomSci y -> AtomSci (fromIntegral x + y)
      _ -> atomNotNumErr
    AtomSci x -> \case
      AtomInt y -> AtomSci (x + fromIntegral y)
      AtomSci y -> AtomSci (x + y)
      _ -> atomNotNumErr
    _ -> atomNotNumErr
  (*) = \case
    AtomInt x -> \case
      AtomInt y -> AtomInt (x * y)
      AtomSci y -> AtomSci (fromIntegral x * y)
      _ -> atomNotNumErr
    AtomSci x -> \case
      AtomInt y -> AtomSci (x * fromIntegral y)
      AtomSci y -> AtomSci (x * y)
      _ -> atomNotNumErr
    _ -> atomNotNumErr
  negate = \case
    AtomInt x -> AtomInt (negate x)
    AtomSci x -> AtomSci (negate x)
    _ -> atomNotNumErr
  abs = \case
    AtomInt x -> AtomInt (abs x)
    AtomSci x -> AtomSci (abs x)
    _ -> atomNotNumErr
  signum = \case
    AtomInt x -> AtomInt (signum x)
    AtomSci x -> AtomSci (signum x)
    _ -> atomNotNumErr
  fromInteger = AtomInt

instance IsString Atom where
  fromString = AtomSym . fromString

instance Pretty Atom where
  pretty = \case
    AtomSym x -> pretty x
    AtomInt x -> pretty x
    AtomSci x -> P.viaShow x
    AtomStr x -> "\"" <> pretty x <> "\""
    AtomChar x -> "'" <> pretty x <> "'"

data AtomType
  = AtomTypeSym
  | AtomTypeInt
  | AtomTypeSci
  | AtomTypeStr
  | AtomTypeChar
  deriving stock (Eq, Ord, Show, Enum, Bounded)

atomType :: Atom -> AtomType
atomType = \case
  AtomSym _ -> AtomTypeSym
  AtomInt _ -> AtomTypeInt
  AtomSci _ -> AtomTypeSci
  AtomStr _ -> AtomTypeStr
  AtomChar _ -> AtomTypeChar

newtype Doc = Doc {unDoc :: Seq Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | An S-expression
data SexpF r
  = SexpAtomF !Atom
  | SexpListF !Brace !(Seq r)
  | SexpQuoteF r
  | SexpUnquoteF r
  | SexpDocF !Doc r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

sexpNotNumErr :: a
sexpNotNumErr = error "Sexp not num"

-- Again, bad instance, but nice to have literal syntax
instance Num (SexpF a) where
  (+) = \case
    SexpAtomF x -> \case
      SexpAtomF y -> SexpAtomF (x + y)
      _ -> sexpNotNumErr
    _ -> sexpNotNumErr
  (*) = \case
    SexpAtomF x -> \case
      SexpAtomF y -> SexpAtomF (x * y)
      _ -> sexpNotNumErr
    _ -> sexpNotNumErr
  negate = \case
    SexpAtomF x -> SexpAtomF (negate x)
    _ -> sexpNotNumErr
  abs = \case
    SexpAtomF x -> SexpAtomF (abs x)
    _ -> sexpNotNumErr
  signum = \case
    SexpAtomF x -> SexpAtomF (signum x)
    _ -> sexpNotNumErr
  fromInteger = SexpAtomF . fromInteger

instance IsString (SexpF r) where
  fromString = SexpAtomF . fromString

instance (Pretty r) => Pretty (SexpF r) where
  pretty = \case
    SexpAtomF a -> pretty a
    SexpListF b rs -> pretty (openBraceChar b) <> P.hsep (fmap pretty (toList rs)) <> pretty (closeBraceChar b)
    SexpQuoteF r -> "`" <> pretty r
    SexpUnquoteF r -> "," <> pretty r
    SexpDocF (Doc d) r ->
      case d of
        Empty -> pretty r
        h :<| t ->
          let h' = (";|" <> pretty h <> "\n")
              t' = fmap (\x -> ";" <> pretty x <> "\n") t
          in  P.hcat (toList (h' :<| (t' :|> pretty r)))

newtype Sexp = Sexp {unSexp :: SexpF Sexp}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, IsString, Pretty)

type instance Base Sexp = SexpF

instance Recursive Sexp where project = unSexp

instance Corecursive Sexp where embed = Sexp

data SexpType
  = SexpTypeAtom !AtomType
  | SexpTypeList !Brace
  | SexpTypeQuote
  | SexpTypeUnquote
  | SexpTypeDoc
  deriving stock (Eq, Ord, Show)

sexpType :: SexpF r -> SexpType
sexpType = \case
  SexpAtomF at -> SexpTypeAtom (atomType at)
  SexpListF b _ -> SexpTypeList b
  SexpQuoteF _ -> SexpTypeQuote
  SexpUnquoteF _ -> SexpTypeUnquote
  SexpDocF _ _ -> SexpTypeDoc

class IsSexp s where
  toSexp :: s -> Sexp

instance IsSexp Sexp where
  toSexp = id

instance IsSexp (Memo SexpF b) where
  toSexp = B.unMkMemo

instance (IsSexp s) => IsSexp (Anno b s) where
  toSexp = toSexp . B.annoVal

instance IsSexp Atom where
  toSexp = Sexp . SexpAtomF

instance IsSexp Integer where
  toSexp = toSexp . AtomInt

instance IsSexp Symbol where
  toSexp = toSexp . AtomSym

instance IsSexp Char where
  toSexp = toSexp . AtomChar

instance IsSexp String where
  toSexp = toSexp . T.pack

instance IsSexp Text where
  toSexp = toSexp . AtomStr

pattern SexpAtom :: Atom -> Sexp
pattern SexpAtom x = Sexp (SexpAtomF x)

pattern SexpList :: Brace -> Seq Sexp -> Sexp
pattern SexpList x y = Sexp (SexpListF x y)

pattern SexpQuote :: Sexp -> Sexp
pattern SexpQuote x = Sexp (SexpQuoteF x)

pattern SexpUnquote :: Sexp -> Sexp
pattern SexpUnquote x = Sexp (SexpUnquoteF x)

pattern SexpDoc :: Doc -> Sexp -> Sexp
pattern SexpDoc x y = Sexp (SexpDocF x y)

{-# COMPLETE SexpAtom, SexpList, SexpQuote, SexpUnquote, SexpDoc #-}
