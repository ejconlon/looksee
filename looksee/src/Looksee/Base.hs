{-# LANGUAGE TemplateHaskell #-}

module Looksee.Base where

import Control.Exception (Exception)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)

data Point = Point
  { pointOffset :: !Int
  , pointLine :: !Int
  , pointCol :: !Int
  }
  deriving stock (Eq, Ord, Show)

-- | Create point at beginning of input
initPoint :: Point
initPoint = Point 0 0 0

nextPoint :: Char -> Point -> Point
nextPoint x (Point o l c) =
  if x == '\n'
    then Point (o + 1) (l + 1) 0
    else Point (o + 1) l (c + 1)

nextPoint' :: Text -> Point -> Point
nextPoint' t p = T.foldl' (flip nextPoint) p t

data Span = Span {spanStart :: !Point, spanEnd :: !Point}
  deriving stock (Eq, Ord, Show)

data Bounds = Bounds {boundsStart :: !Point, boundsEndOffset :: !(Maybe Int)}
  deriving stock (Eq, Ord, Show)

-- | Create bounds with unknown end
initBounds :: Bounds
initBounds = Bounds initPoint Nothing

-- | Create bounds from the given text
textBounds :: Text -> Bounds
textBounds t = Bounds initPoint (Just (T.length t))

-- | A parser label (for error reporting)
newtype Label = Label {unLabel :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | Phase of alternative parsing (for error reporting)
data AltPhase = AltPhaseBranch | AltPhaseCont
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Phase of infix/split parsing (for error reporting)
data InfixPhase = InfixPhaseLeft | InfixPhaseRight | InfixPhaseCont
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Reason for parse failure
data Reason e r
  = ReasonCustom !e
  | ReasonExpect !Text !Text
  | ReasonDemand !Int !Int
  | ReasonLeftover !Int
  | ReasonAlt !(Seq (AltPhase, r))
  | ReasonInfix !(Seq (Int, InfixPhase, r))
  | ReasonFail !Text
  | ReasonLabeled !Label r
  | ReasonLook r
  | ReasonTakeNone
  | ReasonEmpty
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''Reason
deriveBifoldable ''Reason
deriveBitraversable ''Reason

-- | Base functor for 'Err' containing the range and reason for the error
data ErrF e r = ErrF
  { efBounds :: !Bounds
  , efReason :: !(Reason e r)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveBifunctor ''ErrF
deriveBifoldable ''ErrF
deriveBitraversable ''ErrF

-- | A parse error, which may contain multiple sub-errors
newtype Err e = Err {unErr :: ErrF e (Err e)}
  deriving stock (Eq, Ord, Show)

instance Functor Err where
  fmap f = go
   where
    go (Err (ErrF ra re)) = Err (ErrF ra (bimap f go re))

instance Foldable Err where
  foldr f = flip go
   where
    go (Err (ErrF _ re)) z = bifoldr f go z re

instance Traversable Err where
  traverse f = go
   where
    go (Err (ErrF ra re)) = fmap (Err . ErrF ra) (bitraverse f go re)

instance (Typeable e, Show e) => Exception (Err e)

type instance Base (Err e) = ErrF e

instance Recursive (Err e) where
  project = unErr

instance Corecursive (Err e) where
  embed = Err

-- | Span of a parse error
errBounds :: Err e -> Bounds
errBounds = efBounds . unErr

-- | Reason for a parse error
errReason :: Err e -> Reason e (Err e)
errReason = efReason . unErr
