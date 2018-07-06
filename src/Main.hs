import Data.Char
-- use Data.Text instead of [Char]!

-- Lexer
data Token =
    Identifier String
  | NumberToken String
  | LeftParen String
  | RightParen String
  | Comma String
  deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
  | isAlpha c = let (identifier, c') = readIdentifier (c:cs) in identifier:(tokenize c')
  | isDigit c = let (number, c') = readNumber (c:cs) in number:(tokenize c')
  | isSpace c = tokenize cs
  | c == '(' = (LeftParen [c]):(tokenize cs)
  | c == ')' = (RightParen [c]):(tokenize cs)
  | c == ',' = (Comma [c]):(tokenize cs)
  | otherwise = error ("Unknown character: " ++ [c])

readIdentifier :: String -> (Token, String)
readIdentifier str =
  let (tokenText, str') = span isAlpha str in
    (Identifier tokenText, str')

readNumber :: String -> (Token, String)
readNumber str =
  let (tokenText, str') = span isDigit str in
    (NumberToken tokenText, str')

-- Parser
data ParseTree =
  IdentifierNode String
  | NumberNode String
  | FunctionCall ParseTree [ParseTree]
  deriving (Show)

parse :: [Token] -> ParseTree
parse tokens =
  let (expr, tokens') = parseExpression tokens in
    case tokens' of
      [] -> expr
      _ -> error "Expected the end of the source code."

parseExpression :: [Token] -> (ParseTree, [Token])
parseExpression tokens =
  let (prefixExpr, tokens') = parsePrefixExpression tokens in
    parseInfixExpression prefixExpr tokens'

parsePrefixExpression :: [Token] -> (ParseTree, [Token])
parsePrefixExpression (t:ts) =
  case t of
    Identifier text -> (IdentifierNode text, ts)
    NumberToken text -> (NumberNode text, ts)
    _ -> error ("Unknown token: " ++ (show t))

parseInfixExpression :: ParseTree -> [Token] -> (ParseTree, [Token])
parseInfixExpression prefixExpr (t:ts) =
  case t of
    LeftParen _ -> parseFunctionCall prefixExpr (t:ts)
    _ -> (prefixExpr, t:ts)

parseFunctionCall :: ParseTree -> [Token] -> (ParseTree, [Token])
parseFunctionCall funcExpr tokens =
  let (args, t') = parseFunctionCallArgumentsTuple tokens in
    (FunctionCall funcExpr args, t')

parseFunctionCallArgumentsTuple :: [Token] -> ([ParseTree], [Token])
parseFunctionCallArgumentsTuple (t:ts) =
  case t of
    LeftParen _ ->
      let (args, (t':ts')) = parseCommaSeparated parseExpression ts in
        case t' of
          RightParen text -> (args, ts')
          _ -> error ("Expected ')' but encountered" ++ (show t'))
    _ -> error ("Expected '(' but encountered: " ++ (show t))

parseCommaSeparated :: ([Token] -> (ParseTree, [Token])) -> [Token] -> ([ParseTree], [Token])
parseCommaSeparated parseFunc tokens =
  let (parseTree, (t':ts')) = parseFunc tokens in
    case t' of
      Comma _ ->
        let (parseTreesTail, t'') = parseCommaSeparated parseFunc ts' in
          (parseTree:parseTreesTail, t'')
      _ -> ([parseTree], t':ts')

-- Interpreter
evaluate :: ParseTree -> Double
evaluate (FunctionCall funcExpr args) =
  case funcExpr of
    IdentifierNode text ->
      case text of
        "add" -> sum (map evaluate args)
        _ -> error ("Unsupported function: " ++ text)
    _ -> error ("Unsupported function expression type: " ++ (show funcExpr))
evaluate (NumberNode text) = read text::Double

sourceCode = "add(1, 2)"

main =
  let tokens = tokenize sourceCode in
    let parseTree = parse tokens in
      do
        print parseTree
        print (evaluate parseTree)