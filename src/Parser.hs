module Parser (
  ParseTree(..),
  parse
) where

import Lexer

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
    LeftParen _ ->
      let (expr, t':ts') = parseExpression ts in
        case t' of
          RightParen _ -> (expr, ts')
          _ -> error ("Expected ')' but encountered" ++ (show t'))
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
          RightParen _ -> (args, ts')
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