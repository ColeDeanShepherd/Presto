module Main where

import AST

{-
id = x -> x
add = (a, b) -> a + b
-}


program =
  [ Binding
      { bindingName = Ident "id"
      , bindingExpr = FnExpr Fn
          { fnParams = [ Param { paramName = Ident "x", paramType = TypeExpr $ BindRef $ Ident "Infer" }]
          , fnType = TypeExpr $ BindRef $ Ident "Infer"
          , fnValue = BindRef $ Ident "x" }
      }
  ]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
