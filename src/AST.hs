module AST where

data Binding = Binding
  { bindingName :: Ident
  , bindingExpr :: Expr }

data Expr =
    FnExpr Fn
  | BindRef Ident

data Param = Param
  { paramName :: Ident
  , paramType :: Maybe TypeExpr }

newtype TypeExpr = TypeExpr Expr

newtype Ident = Ident String

data Fn = Fn
  { fnParams :: [Param]
  , fnType :: Maybe TypeExpr
  , fnValue :: Expr }