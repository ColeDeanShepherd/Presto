module Main where

import AST
import Parse
import CodeGen (generateCode)

filePath = "Compile.pst"

compile :: String -> (String, [String])
compile sourceCode =
  let parseResult = parsePrestoProgram sourceCode filePath in
    case parseResult of
      Right bindings -> (generateCode bindings, [])
      Left e -> ("", [show e])

main :: IO ()
main = do
  sourceCode <- readFile filePath

  putStrLn "Source code:"
  putStrLn sourceCode
  putStrLn ""

  let (generatedCode, errors) = compile sourceCode in do
    putStrLn "Errors:"
    mapM_ putStrLn errors
    putStrLn ""

    putStrLn "Generated code:"
    putStrLn generatedCode
