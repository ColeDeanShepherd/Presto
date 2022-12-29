module Main where

import AST
import Parse
import CodeGen (generateCode)

{-
id = x -> x
add = (a, b) -> a + b
-}

sourceCode :: String
sourceCode = "id = (x) -> x"

main :: IO ()
main = do
  putStrLn "Source code:"
  putStrLn sourceCode

  putStrLn ""

  let parseResult = parsePrestoProgram sourceCode in
    case parseResult of
      Right bindings -> do
        putStrLn "Generated code:"
        putStrLn $ generateCode bindings
      Left e -> print ("Parse error: " ++ show e)