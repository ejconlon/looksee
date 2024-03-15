module Mello.Recognize
  (
  )
where

import Control.Foldl (Fold (..))
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Mello.Text (Brace, readCloseBrace, readOpenBrace)

data X e s = X !(Maybe e) !s

foldUntilErr :: (a -> ExceptT e (State s) ()) -> s -> (Maybe e -> s -> b) -> Fold a b
foldUntilErr step initial extract = Fold step' initial' extract'
 where
  step' x@(X me s) a =
    case me of
      Just _ -> x
      Nothing ->
        let (ea, s') = runState (runExceptT (step a)) s
        in  case ea of
              Left e -> X (Just e) s'
              Right _ -> X Nothing s'
  initial' = X Nothing initial
  extract' (X me s) = extract me s

data RecogElem
  = RecogElemString
  | RecogElemChar
  | RecogElemComment
  | RecogElemSlashEsc
  | RecogElemQuote
  | RecogElemUnquote
  | RecogElemBrace !Brace
  deriving stock (Eq, Ord, Show)

newtype RecogErr
  = RecogErrMismatch Brace
  deriving stock (Eq, Ord, Show)

data RecogState = RecogState
  { rsOffset :: !Int
  , rsStack :: ![RecogElem]
  }
  deriving stock (Eq, Ord, Show)

initRecogState :: RecogState
initRecogState = RecogState 0 []

type RecogM = ExceptT RecogErr (State RecogState)

data CharCase
  = CharCaseNewline
  | CharCaseDoubleQuote
  | CharCaseSingleQuote
  | CharCaseOpenComment
  | CharCaseSlashEsc
  | CharCaseOpenBrace !Brace
  | CharCaseCloseBrace !Brace
  deriving stock (Eq, Ord, Show)

readCharCase :: Char -> Maybe CharCase
readCharCase c =
  if
    | c == '\n' -> Just CharCaseNewline
    | c == '"' -> Just CharCaseDoubleQuote
    | c == '\'' -> Just CharCaseSingleQuote
    | c == ';' -> Just CharCaseOpenComment
    | c == '\\' -> Just CharCaseSlashEsc
    | otherwise ->
        case readOpenBrace c of
          Just b -> Just (CharCaseOpenBrace b)
          Nothing -> case readCloseBrace c of
            Just b -> Just (CharCaseCloseBrace b)
            Nothing -> Nothing

stepR :: Char -> RecogM ()
stepR c = goRet
 where
  goRet = goStart <* incOffset
  goStart = do
    mh <- peekStack
    case mh of
      Just RecogElemString -> goString
      Just RecogElemChar -> goChar
      Just RecogElemComment -> goComment
      Just RecogElemSlashEsc -> goSlashEsc
      Just RecogElemQuote -> goQuote
      Just RecogElemUnquote -> goUnquote
      Just (RecogElemBrace b) -> goDefault (Just b)
      Nothing -> goDefault Nothing
  goString = case readCharCase c of
    Just CharCaseDoubleQuote -> popStack
    Just CharCaseSlashEsc -> pushStack RecogElemSlashEsc
    _ -> pure ()
  goChar = case readCharCase c of
    Just CharCaseSingleQuote -> popStack
    Just CharCaseSlashEsc -> pushStack RecogElemSlashEsc
    _ -> pure ()
  goComment = case readCharCase c of
    Just CharCaseNewline -> popStack
    _ -> pure ()
  goSlashEsc = popStack -- just ignore input and leave slash esc mode
  goQuote = error "TODO"
  goUnquote = error "TODO"
  goDefault mb = case readCharCase c of
    Just CharCaseDoubleQuote -> pushStack RecogElemString
    Just CharCaseSingleQuote -> pushStack RecogElemChar
    Just CharCaseOpenComment -> pushStack RecogElemComment
    Just (CharCaseOpenBrace b) -> pushStack (RecogElemBrace b)
    Just (CharCaseCloseBrace b) ->
      case mb of
        Just b0 | b == b0 -> popStack
        _ -> throwError (RecogErrMismatch b)
    _ -> pure ()
  incOffset = modify' (\s -> s {rsOffset = rsOffset s + 1})
  pushStack h = modify' (\s -> s {rsStack = h : rsStack s})
  popStack = modify' $ \s ->
    case rsStack s of
      [] -> s
      _ : t -> s {rsStack = t}
  peekStack = gets $ \s ->
    case rsStack s of
      [] -> Nothing
      h : _ -> Just h

extractR :: Maybe RecogErr -> RecogState -> Either RecogErr Bool
extractR me s = maybe (Right (null (rsStack s))) Left me

-- TODO expose this when quote/unquote recognition is implemented
-- and it's all tested
sexpRecognizer :: Fold Char (Either RecogErr Bool)
sexpRecognizer = foldUntilErr stepR initRecogState extractR
