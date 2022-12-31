module AST where

data Binding = Binding
  { bindingName :: Ident
  , bindingExpr :: Expr }

data Expr =
    FnExpr Fn
  | BindRef Ident
  | MatchExpr Expr [MatchRule]
  | NumberLiteral String

data Param = Param
  { paramName :: Ident
  , paramType :: Maybe TypeExpr }

newtype TypeExpr = TypeExpr Expr

newtype Ident = Ident { identText :: String }

data Fn = Fn
  { fnParams :: [Param]
  , fnType :: Maybe TypeExpr
  , fnValue :: Expr }

data MatchRule = MatchRule
  { matchExpr :: Expr
  , resultExpr :: Expr }