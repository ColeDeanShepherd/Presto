module Main where

import AST
import Parse

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

  let parseResult = parsePrestoProgram sourceCode in
    case parseResult of
      Right bindings -> putStrLn "Parse success!"
      Left e -> print e