module Looksee.Extra
  ( betweenP
  , sepByP
  , sepBy1P
  , sepBy2P
  , spaceP
  , stripP
  , stripStartP
  , stripEndP
  , signedWithP
  , signedP
  , intP
  , uintP
  , decP
  , udecP
  , sciP
  , usciP
  , numP
  , unumP
  , space1P
  , strip1P
  , stripStart1P
  , stripEnd1P
  , scopeP
  , iterP
  , strP
  , doubleStrP
  , singleStrP
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.State.Strict (MonadState (..), StateT, evalStateT)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Ratio ((%))
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Looksee.V2 (ParserT, charP, charP_, dropWhile1P, dropWhileP, headP, lengthP, optP, takeWhile1P, textP_, transP)

-- | Parse with some local state
scopeP :: (Monad m) => s -> ParserT e (StateT s m) a -> ParserT e m a
scopeP s0 = transP (`evalStateT` s0)

-- | Repeats the parser until it returns a 'Just' value
iterP :: (Functor m) => ParserT e m (Maybe a) -> ParserT e m a
iterP p = go
 where
  go = p >>= maybe go pure

data StrState = StrState !Bool !(Seq Char)

-- | Parse a string with a custom quote character. Supports backslash-escaping.
strP :: (Monad m) => Char -> ParserT e m Text
strP d = do
  textP_ (T.singleton d)
  scopeP (StrState False Empty) $ iterP $ do
    c <- headP
    state $ \ss@(StrState esc buf) ->
      if c == d
        then
          if esc
            then (Nothing, StrState False (buf :|> c))
            else (Just (T.pack (toList buf)), ss)
        else
          if c == '\\'
            then
              if esc
                then (Nothing, StrState False (buf :|> c))
                else (Nothing, StrState True buf)
            else (Nothing, StrState False (buf :|> c))

-- | Parse a double-quoted string
doubleStrP :: (Monad m) => ParserT e m Text
doubleStrP = strP '"'

-- | Parse a single-quoted string
singleStrP :: (Monad m) => ParserT e m Text
singleStrP = strP '\''

-- | Parse between an opening delimiter (first parser) and a closing delimited (second parser)
betweenP :: (Functor m) => ParserT e m x -> ParserT e m y -> ParserT e m a -> ParserT e m a
betweenP px py pa = px *> pa <* py

-- private
sepByTailP :: (Monad m) => ParserT e m () -> ParserT e m a -> Seq a -> ParserT e m (Seq a)
sepByTailP pu pa = go
 where
  go !acc = do
    ma <- optP (pu >> pa)
    case ma of
      Nothing -> pure acc
      Just a -> go (acc :|> a)

-- | Parse a sequence of items delimited by the first parser
sepByP :: (Monad m) => ParserT e m () -> ParserT e m a -> ParserT e m (Seq a)
sepByP pu pa = optP pa >>= maybe (pure Empty) (sepByTailP pu pa . Seq.singleton)

-- | Like 'sepByP' but ensures at least one result.
sepBy1P :: (Monad m) => ParserT e m () -> ParserT e m a -> ParserT e m (Seq a)
sepBy1P pu pa = pa >>= sepByTailP pu pa . Seq.singleton

-- | Like 'sepByP' but ensures at least two results (and at least one delimiter).
sepBy2P :: (Monad m) => ParserT e m () -> ParserT e m a -> ParserT e m (Seq a)
sepBy2P pu pa = do
  a0 <- pa
  pu
  a1 <- pa
  sepByTailP pu pa (Empty :|> a0 :|> a1)

-- | Consumes many spaces at the start of the range
spaceP :: (Monad m) => ParserT e m ()
spaceP = void (dropWhileP isSpace)

-- | Strips spaces before and after parsing
stripP :: (Monad m) => ParserT e m a -> ParserT e m a
stripP p = spaceP *> p <* spaceP

-- | Strips spaces before parsing
stripStartP :: (Monad m) => ParserT e m a -> ParserT e m a
stripStartP p = spaceP *> p

-- | Strips spaces after parsing
stripEndP :: (Monad m) => ParserT e m a -> ParserT e m a
stripEndP p = p <* spaceP

-- | Add signed-ness to any parser with a negate function
signedWithP :: (Monad m) => (a -> a) -> ParserT e m a -> ParserT e m a
signedWithP neg p = do
  ms <- optP (charP '-')
  case ms of
    Nothing -> p
    Just _ -> fmap neg p

-- | Add signed-ness to any numeric parser
signedP :: (Monad m, Num a) => ParserT e m a -> ParserT e m a
signedP = signedWithP negate

-- | Parse an signed integer
intP :: (Monad m) => ParserT e m Integer
intP = signedP uintP

-- | Parse an unsigned integer
uintP :: (Monad m) => ParserT e m Integer
uintP = T.foldl' (\n d -> n * 10 + fromIntegral (digitToInt d)) 0 <$> takeWhile1P isDigit

-- | Parse a signed decimal
decP :: (Monad m) => ParserT e m Rational
decP = signedP udecP

-- | Parse an unsigned decimal
udecP :: (Monad m) => ParserT e m Rational
udecP = do
  whole <- fmap fromInteger uintP
  hasDot <- fmap isJust (optP (charP '.'))
  if hasDot
    then do
      (numerator, places) <- lengthP uintP
      let denominator = 10 ^ places
          part = numerator % denominator
      pure (whole + part)
    else pure whole

-- | Parse a signed scientific number
sciP :: (Monad m) => ParserT e m Scientific
sciP = signedP usciP

-- | Parse an unsigned scientific  number
usciP :: (Monad m) => ParserT e m Scientific
usciP = do
  whole <- uintP
  hasDot <- fmap isJust (optP (charP_ '.'))
  (frac, places) <- if hasDot then lengthP uintP else pure (0, 0)
  hasEx <- fmap isJust (optP (charP_ 'e' <|> charP_ 'E'))
  ex <- if hasEx then fmap fromIntegral intP else pure 0
  let wholeS = Sci.scientific whole ex
      partS = Sci.scientific frac (ex - places)
  pure (wholeS + partS)

-- | Parse a signed integer/scientific number, defaulting to integer if possible.
numP :: (Monad m) => ParserT e m (Either Integer Scientific)
numP = signedWithP (bimap negate negate) unumP

-- | Parse an unsigned integer/scientific number, defaulting to integer if possible.
unumP :: (Monad m) => ParserT e m (Either Integer Scientific)
unumP = do
  whole <- uintP
  hasDot <- fmap isJust (optP (charP_ '.'))
  mayFracPlaces <- if hasDot then fmap Just (lengthP uintP) else pure Nothing
  hasEx <- fmap isJust (optP (charP_ 'e' <|> charP_ 'E'))
  mayEx <- if hasEx then fmap (Just . fromIntegral) intP else pure Nothing
  case (mayFracPlaces, mayEx) of
    (Nothing, Nothing) -> pure (Left whole)
    _ -> do
      let (frac, places) = fromMaybe (0, 0) mayFracPlaces
          ex = fromMaybe 0 mayEx
          wholeS = Sci.scientific whole ex
          partS = Sci.scientific frac (ex - places)
      pure (Right (wholeS + partS))

-- | Like 'spaceP' but ensures at least 1 space removed
space1P :: (Monad m) => ParserT e m ()
space1P = void (dropWhile1P isSpace)

-- | Like 'stripP' but ensures at least 1 space removed
strip1P :: (Monad m) => ParserT e m a -> ParserT e m a
strip1P p = space1P *> p <* space1P

-- | Like 'stripStartP' but ensures at least 1 space removed
stripStart1P :: (Monad m) => ParserT e m a -> ParserT e m a
stripStart1P p = space1P *> p

-- | Like 'stripEndP' but ensures at least 1 space removed
stripEnd1P :: (Monad m) => ParserT e m a -> ParserT e m a
stripEnd1P p = p <* space1P
