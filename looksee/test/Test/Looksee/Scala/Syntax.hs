module Test.Looksee.Scala.Syntax
  ( Expr (..)
  , Arg (..)
  , ForClause (..)
  , Local (..)
  , Param (..)
  , Pattern (..)
  , Stmt (..)
  )
where

import Data.Text (Text)

data Expr
  = Lit Int
  | Neg Expr
  | Fact Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Pow Expr Expr
  | Eq Expr Expr
  | Gt Expr Expr
  | If Expr Expr Expr
  | Lam [Text] Expr
  | Call Text [Arg]
  | Select Expr Text
  | Apply Expr [Arg]
  | Ascribe Expr Text
  | Tuple [Expr]
  | For [ForClause] Expr
  | While Expr Expr
  | Block [Local] Expr
  | Match Expr [(Pattern, Maybe Expr, Expr)]
  | Record [(Text, Expr)]
  | Placeholder
  | Var Text
  | List [Expr]
  | Lines [Expr]
  deriving stock (Eq, Show)

data Arg
  = PosArg Expr
  | NamedArg Text Expr
  deriving stock (Eq, Show)

data ForClause
  = ForBind Text Expr
  | ForGuard Expr
  deriving stock (Eq, Show)

data Local = LocalVal Text (Maybe Text) Expr
  deriving stock (Eq, Show)

data Pattern
  = PVar Text
  | PWildcard
  | PLit Int
  | PConstructor Text [Pattern]
  | PTuple [Pattern]
  | PAlt Pattern Pattern
  deriving stock (Eq, Show)

data Param = Param
  { paramName :: Text
  , paramType :: Maybe Text
  , paramDefault :: Maybe Expr
  }
  deriving stock (Eq, Show)

data Stmt
  = Import [Text]
  | Data Text [Text] [(Text, [Text])]
  | Val Text (Maybe Text) Expr
  | Def Text [Param] Expr
  | DefUsing Text [Param] [Param] Expr
  | Given Text Expr
  | GivenWith Text [Stmt]
  | DefGeneric Text [Text] [Param] (Maybe Text) [Param] Expr
  | Object Text [Stmt]
  | Extension Text Text [Stmt]
  deriving stock (Eq, Show)
