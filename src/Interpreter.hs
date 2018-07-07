module Interpreter (
  evaluate
) where

import Parser

evaluate :: ParseTree -> Double
evaluate (FunctionCall funcExpr args) =
  case funcExpr of
    IdentifierNode text ->
      case text of
        "add" -> foldl (+) (evaluate (head args)) (map evaluate (tail args))
        "sub" -> foldl (-) (evaluate (head args)) (map evaluate (tail args))
        "mul" -> foldl (*) (evaluate (head args)) (map evaluate (tail args))
        "div" -> foldl (/) (evaluate (head args)) (map evaluate (tail args))
        _ -> error ("Unsupported function: " ++ text)
    _ -> error ("Unsupported function expression type: " ++ (show funcExpr))
evaluate (NumberNode text) = read text::Double