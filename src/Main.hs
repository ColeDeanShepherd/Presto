import Lexer
import Parser
import Interpreter

sourceCode = "mul((mul(1, 2)), 3)"

main =
  let tokens = tokenize sourceCode in
    let parseTree = parse tokens in
      do
        print tokens
        print parseTree
        print (evaluate parseTree)