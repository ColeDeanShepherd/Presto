module CodeGen where

import AST
import Data.List (intercalate)

generateCode :: [Binding] -> String
generateCode bindings = intercalate "\n" (map generateBindingCode bindings)

generateBindingCode :: Binding -> String
generateBindingCode binding =
  case bindingExpr binding of
    FnExpr fn ->
      let name = identText (bindingName binding)
          paramsStr = intercalate ", " (map generateParam (fnParams fn))
          body = "return " ++ generateExpr (fnValue fn) ++ ";"
      in "void " ++ name ++ "(" ++ paramsStr ++ ")" ++ " " ++ "{" ++ " " ++ body ++ " " ++ "}"

generateParam :: Param -> String
generateParam p = "int " ++ identText (paramName p)

generateExpr :: Expr -> String
--generateExpr (FnExpr fn) = ""
generateExpr (BindRef ident) = identText ident