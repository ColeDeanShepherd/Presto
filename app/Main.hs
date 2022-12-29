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

compile :: String -> (String, [String])
compile sourceCode =
  let parseResult = parsePrestoProgram sourceCode in
    case parseResult of
      Right bindings -> (generateCode bindings, [])
      Left e -> ("", [show e])

main :: IO ()
main = do
  putStrLn "Source code:"
  putStrLn sourceCode
  putStrLn ""

  let (generatedCode, errors) = compile sourceCode in do
    mapM_ putStrLn errors

    putStrLn "Generated code:"
    putStrLn generatedCode
