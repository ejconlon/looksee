{-# LANGUAGE OverloadedStrings #-}

module Test.Looksee.Scala.Parse
  ( expr
  , program
  )
where

import Control.Monad (void)
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Looksee hiding (Parser, eolP)
import Looksee qualified as L
import Test.Looksee.Scala.Syntax

type Parser = L.Parser Void

expr :: Parser Expr
expr = spaceP *> expression <* endP

program :: Parser [Stmt]
program =
  spaceP
    *> chooseElseP
      [guarded (symbol_ "{") compactProgramBody]
      (toList <$> sepByP (guarded semiOrNl (pure ())) [guardedWith stmt pure])
    <* spaceP
    <* endP

expression :: Parser Expr
expression = prattWithTable exprTable atom

exprTable :: PrattTable Void Expr
exprTable = prattTable exprOperators

atom :: Parser Expr
atom = chooseP (exprFormBranches <> atomFallbackBranches)
 where
  atomFallbackBranches =
    [ guarded (symbol_ "_") (pure Placeholder)
    , guardedWith (lexeme number) (pure . Lit)
    , guardedWith (lexeme identifier) (pure . Var)
    ]

exprFormBranches :: [GuardedCase Void Expr]
exprFormBranches =
  [ guarded (symbol_ "if") ifExprBody
  , guarded (symbol_ "while") whileExprBody
  , guarded (symbol_ "for") forExprBody
  , guarded (symbol_ "match") matchExprBody
  , guarded (symbol_ "{") braceExprBody
  , guarded (symbol_ "[") (Lines . toList <$> delimByP (pure ()) (symbol "]") (void (charP '\n')) expression)
  , guarded (symbol_ "(") parenExprBody
  , guardedWith (lexeme constructorName) (\name -> Call name . toList <$> delimByP (symbol_ "(") (symbol ")") comma arg)
  ]

ifExprBody :: Parser Expr
ifExprBody = do
  cond <- expression
  symbol_ "then"
  yes <- expression
  symbol_ "else"
  If cond yes <$> expression

whileExprBody :: Parser Expr
whileExprBody = do
  cond <- expression
  symbol_ "do"
  While cond <$> expression

forExprBody :: Parser Expr
forExprBody = do
  clauses <- toList <$> delimEndByP (symbol_ "{") (symbol "}") semi forClause
  symbol_ "yield"
  For clauses <$> expression

forClause :: Parser ForClause
forClause =
  chooseP
    [ guarded (symbol_ "if") (ForGuard <$> expression)
    , guardedWith (lexeme identifier) (\name -> symbol_ "<-" *> (ForBind name <$> expression))
    ]

braceExprBody :: Parser Expr
braceExprBody =
  chooseP
    [ guarded (symbol_ "val") compactBlockBodyAfterFirstVal
    , guardedWith (lexeme identifier <* assign) recordAfterFirstField
    , guardedWith expression (\first -> List . (first :) . toList <$> delimEndTailP comma (symbol "}") expression)
    , guarded (symbol_ "}") (pure (List []))
    ]

compactBlockBodyAfterFirstVal :: Parser Expr
compactBlockBodyAfterFirstVal = do
  first <- localBodyAfterVal
  semi
  (locals, body) <- localsThenBody []
  pure (Block (first : locals) body)
 where
  localsThenBody acc =
    chooseP
      [ guarded (symbol_ "val") $ do
          local <- localBodyAfterVal
          semi
          localsThenBody (acc <> [local])
      , guardedWith expression $ \body -> do
          optional semi
          symbol_ "}"
          pure (acc, body)
      ]

localBodyAfterVal :: Parser Local
localBodyAfterVal = do
  name <- lexeme identifier
  ty <- optionalAfter (symbol_ ":") typeRef
  assign
  LocalVal name ty <$> expression

recordAfterFirstField :: Text -> Parser Expr
recordAfterFirstField name = do
  value <- expression
  Record <$> go [(name, value)]
 where
  go fields =
    chooseP
      [ guarded (symbol_ "}") (pure fields)
      , guarded semi $
          chooseP
            [ guarded (symbol_ "}") (pure fields)
            , guardedWith (lexeme identifier <* assign) $ \field -> do
                value <- expression
                go (fields <> [(field, value)])
            ]
      ]

parenExprBody :: Parser Expr
parenExprBody = do
  values <- toList <$> delimByP (pure ()) (symbol ")") comma expression
  chooseElseP [guarded (symbol_ "=>") (Lam (tupleParams values) <$> expression)] (pure (singletonOr Tuple values))
 where
  tupleParams =
    fmap
      ( \case
          Var name -> name
          other -> error ("expected lambda parameter, got " <> show other)
      )

matchExprBody :: Parser Expr
matchExprBody = do
  scrutinee <- expression
  cases <- toList <$> delimEndByP (symbol_ "{") (symbol "}") semi matchCase
  pure (Match scrutinee cases)

matchCase :: Parser (Pattern, Maybe Expr, Expr)
matchCase = do
  optional (symbol_ "case")
  pat <- patternP
  guard <- optionalAfter (symbol_ "if") expression
  symbol_ "=>"
  body <- expression
  pure (pat, guard, body)

patternP :: Parser Pattern
patternP = do
  first <- patternAtom
  chooseElseP [guarded (symbol_ "|") (PAlt first <$> patternP)] (pure first)

patternAtom :: Parser Pattern
patternAtom =
  chooseP
    [ guardedWith
        (lexeme constructorName)
        (\name -> PConstructor name . toList <$> maybeDelimByP (symbol_ "(") (symbol ")") comma patternP)
    , guarded (symbol_ "(") (singletonOrP (PTuple . toList) (delimByP (pure ()) (symbol ")") comma patternP))
    , guarded (symbol_ "_") (pure PWildcard)
    , guardedWith (lexeme number) (pure . PLit)
    , guardedWith (lexeme identifier) (pure . PVar)
    ]

arg :: Parser Arg
arg =
  chooseElseP [guardedWith (lexeme identifier <* assign) (\name -> NamedArg name <$> expression)] (PosArg <$> expression)

exprOperators :: [Op Void Expr]
exprOperators =
  [ prefixOp 7 (symbol_ "-") Neg
  , postfixOpWith 11 (symbol ".") (const (flip Select <$> lexeme identifier))
  , postfixOpWith 10 (symbol "(") (const (flip Apply . toList <$> delimByP (pure ()) (symbol ")") comma arg))
  , postfixOp 9 (symbol_ "!") Fact
  , postfixOpWith 9 (symbol_ ":" *> lexeme constructorName) (\ty -> pure (`Ascribe` ty))
  , infixOp AssocRight 8 (symbol_ "^") Pow
  , infixOp AssocLeft 6 (symbol_ "*") Mul
  , infixOp AssocLeft 5 (symbol_ "+") Add
  , infixOp AssocNone 4 (symbol_ ">") Gt
  , infixOp AssocNone 4 (symbol_ "==") Eq
  ]

stmt :: Parser Stmt
stmt = chooseP stmtCases

stmtCases :: [GuardedCase Void Stmt]
stmtCases =
  [ guarded (symbol_ "import") (Import <$> path)
  , guarded (symbol_ "enum") dataStmtBody
  , guarded (symbol_ "given") givenStmtBody
  , guarded (symbol_ "object") objectStmtBody
  , guarded (symbol_ "extension") extensionStmtBody
  , guarded (symbol_ "val") valStmtBody
  , guarded (symbol_ "def") defStmtBody
  ]

compactProgramBody :: Parser [Stmt]
compactProgramBody = toList <$> delimEndByP (pure ()) (symbol "}") semi stmt

dataStmtBody :: Parser Stmt
dataStmtBody = do
  name <- lexeme constructorName
  params <-
    chooseElseP
      [guarded (symbol_ "[") (toList <$> delimByP (pure ()) (symbol "]") comma typeParam)]
      (manyGuarded (guardedWith (lexeme identifier) pure))
  symbol_ "="
  variants <-
    chooseElseP
      [guarded eolP (toList <$> indentBlockP scalaIndent (optionalPipe *> variant <* optionalEol))]
      (sepBy1 variant (symbol_ "|"))
  pure (Data name params variants)

variant :: Parser (Text, [Text])
variant = do
  name <- lexeme constructorName
  fields <- toList <$> maybeDelimByP (symbol_ "(") (symbol ")") comma (lexeme identifier)
  pure (name, fields)

optionalPipe :: Parser ()
optionalPipe = optional (symbol_ "|")

valStmtBody :: Parser Stmt
valStmtBody = do
  name <- lexeme identifier
  ty <- optionalAfter (symbol_ ":") typeRef
  assign
  Val name ty <$> expression

defStmtBody :: Parser Stmt
defStmtBody = do
  name <- lexeme identifier
  tyParams <- chooseElseP [guarded (symbol_ "[") (toList <$> delimByP (pure ()) (symbol "]") comma typeParam)] (pure [])
  params <- paramList
  usingParams <- chooseElseP [guarded (symbol_ "(") usingClauseBody] (pure [])
  ret <- optionalAfter (symbol_ ":") typeRef
  assign
  body <- layoutBody expression
  pure $ case (tyParams, ret, usingParams) of
    ([], Nothing, []) -> Def name params body
    ([], Nothing, params0) -> DefUsing name params params0 body
    _ -> DefGeneric name tyParams params ret usingParams body

givenStmtBody :: Parser Stmt
givenStmtBody = do
  ty <- typeRef
  chooseP
    [ guarded (symbol_ "with") (GivenWith ty <$> layoutStmts)
    , guarded (symbol_ "=") (Given ty <$> layoutBody expression)
    ]

objectStmtBody :: Parser Stmt
objectStmtBody = do
  name <- lexeme constructorName
  body <- layoutStmts
  pure (Object name body)

extensionStmtBody :: Parser Stmt
extensionStmtBody = do
  (name, ty) <- parens $ do
    name <- lexeme identifier
    symbol_ ":"
    ty <- typeRef
    pure (name, ty)
  body <-
    chooseP
      [ guarded (symbol_ "{") (toList <$> delimEndByP (pure ()) (symbol "}") semi (symbol_ "def" *> defStmtBody))
      , guarded eolP (toList <$> indentBlockP scalaIndent (symbol_ "def" *> defStmtBody <* optionalEol))
      ]
  pure (Extension name ty body)

path :: Parser [Text]
path = sepBy1 (lexeme identifier) (charP_ '.')

typeRef :: Parser Text
typeRef = do
  left <- typeApp
  chooseElseP
    [ guarded (symbol_ "?=>") ((\right -> left <> " ?=> " <> right) <$> typeRef)
    , guarded (symbol_ "=>") ((\right -> left <> " => " <> right) <$> typeRef)
    ]
    (pure left)

typeApp :: Parser Text
typeApp =
  chooseP
    [ guarded (symbol_ "(") (("(" <>) . (<> ")") <$> typeRef <* symbol_ ")")
    , guardedWith (lexeme constructorName) $ \name -> do
        args <- toList <$> maybeDelimByP (symbol_ "[") (symbol "]") comma typeRef
        pure $ if null args then name else name <> "[" <> T.intercalate ", " args <> "]"
    ]

typeParam :: Parser Text
typeParam = do
  variance <- chooseElseP [guardedWith (someCharP ("+-" :: String)) (pure . Just)] (pure Nothing)
  name <- chooseP [guardedWith (lexeme constructorName) pure, guardedWith (lexeme identifier) pure]
  pure (maybe name (`T.cons` name) variance)

paramList :: Parser [Param]
paramList = toList <$> delimByP (symbol_ "(") (symbol ")") comma param

usingClauseBody :: Parser [Param]
usingClauseBody = symbol_ "using" *> (toList <$> delimByP (pure ()) (symbol ")") comma param)

param :: Parser Param
param = do
  name <- lexeme identifier
  ty <- optionalAfter (symbol_ ":") typeRef
  defaultValue <- optionalAfter (symbol_ "=") expression
  pure (Param name ty defaultValue)

identifier :: Parser Text
identifier = T.pack <$> ((:) <$> someCharP ['a' .. 'z'] <*> identTail)

constructorName :: Parser Text
constructorName = T.pack <$> ((:) <$> someCharP ['A' .. 'Z'] <*> identTail)

identTail :: Parser String
identTail = toList <$> repeatP (guardedWith (someCharP (['_'] <> ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])) pure)

number :: Parser Int
number = read . toList <$> repeat1P (guardedWith (someCharP ['0' .. '9']) pure)

lexeme :: Parser a -> Parser a
lexeme parser = parser <* hspace

symbol :: Text -> Parser Text
symbol text = lexeme (textP text)

symbol_ :: Text -> Parser ()
symbol_ text = void (symbol text)

hspace :: Parser ()
hspace = void (dropWhileP (== ' '))

comma :: Parser ()
comma = symbol_ ","

semi :: Parser ()
semi = symbol_ ";"

semiOrNl :: Parser ()
semiOrNl = chooseP [guarded semi (pure ()), guarded (charP_ '\n') hspace]

assign :: Parser ()
assign = symbol_ "="

optional :: Parser a -> Parser ()
optional parser = chooseElseP [guardedWith parser (const (pure ()))] (pure ())

optionalAfter :: Parser z -> Parser a -> Parser (Maybe a)
optionalAfter guard parser = chooseElseP [guardedWith guard (const (Just <$> parser))] (pure Nothing)

manyGuarded :: GuardedCase Void a -> Parser [a]
manyGuarded branch = toList <$> repeatP branch

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 item sep = do
  first <- item
  rest <- chooseElseP [guarded (void sep) (sepBy1 item sep)] (pure [])
  pure (first : rest)

parens :: Parser a -> Parser a
parens parser = symbol_ "(" *> parser <* symbol_ ")"

delimEndTailP :: Parser sep -> Parser close -> Parser a -> Parser (Seq a)
delimEndTailP sep close item =
  chooseP
    [ guarded (void close) (pure Empty)
    , guarded (void sep) (chooseElseP [guarded (void close) (pure Empty)] ((:<|) <$> item <*> delimEndTailP sep close item))
    ]

layoutBody :: Parser a -> Parser a
layoutBody = indentedOrInlineP eolP scalaIndent

layoutStmts :: Parser [Stmt]
layoutStmts =
  chooseP
    [guarded (symbol_ "{") compactProgramBody, guarded eolP (toList <$> indentBlockP scalaIndent (stmt <* optionalEol))]

eolP :: Parser ()
eolP = charP_ '\n'

optionalEol :: Parser ()
optionalEol = optional eolP

scalaIndent :: Parser ()
scalaIndent = void (takeWhile1P (== ' '))

singletonOr :: ([a] -> a) -> [a] -> a
singletonOr grouped = \case
  [x] -> x
  xs -> grouped xs
