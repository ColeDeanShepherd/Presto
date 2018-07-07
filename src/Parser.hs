module Parser (
  ParseTree(..),
  parse
) where

import Lexer

{-
To-Do:
-Implement & use peekToken & readToken.
-Fill out parser errors array.
-Return maybes.
-}

data ParserState = ParserState {
  tokensLeft :: [Token],
  errors :: [String]
}

data ParseTree =
  IdentifierNode String
  | NumberNode String
  | FunctionCall ParseTree [ParseTree]
  deriving (Show)

parse :: [Token] -> ParseTree
parse tokens = parseGivenState (ParserState { tokensLeft = tokens, errors = [] })

parseGivenState :: ParserState -> ParseTree
parseGivenState parserState =
  let (parserState', expr) = parseExpression parserState in
    let tokens' = (tokensLeft parserState') in
      case tokens' of
        [] -> expr
        _ -> error ("Expected the end of the source code." ++ (show tokens'))

parseExpression :: ParserState -> (ParserState, ParseTree)
parseExpression parserState =
  let (parserState', prefixExpr) = parsePrefixExpression parserState in
    parseInfixExpression parserState' prefixExpr

parsePrefixExpression :: ParserState -> (ParserState, ParseTree)
parsePrefixExpression parserState@(ParserState { tokensLeft = (nextToken:tokensTail), errors = errors }) =
  case nextToken of
    (Token { tokenType = Identifier, tokenText = text }) ->
      (parserState { tokensLeft = tokensTail }, IdentifierNode text)
    (Token { tokenType = NumberToken, tokenText = text }) ->
      (parserState { tokensLeft = tokensTail }, NumberNode text)
    (Token { tokenType = LeftParen, tokenText = _ }) ->
      let parserState' = (parserState { tokensLeft = tokensTail }) in
        let (parserState'', expr) = parseExpression parserState' in
          let nextToken':tokensTail' = (tokensLeft parserState'') in
            case nextToken' of
              (Token { tokenType = RightParen, tokenText = _ }) ->
                ((parserState { tokensLeft = tokensTail' }), expr)
              _ -> error ("Expected ')' but encountered" ++ (show nextToken'))
    _ -> error ("Unknown token: " ++ (show nextToken))

parseInfixExpression :: ParserState -> ParseTree -> (ParserState, ParseTree)
parseInfixExpression parserState prefixExpr =
  let tokens@(nextToken:tokensTail) = (tokensLeft parserState) in
    case nextToken of
      (Token { tokenType = LeftParen, tokenText = _ }) -> parseFunctionCall parserState prefixExpr
      _ -> (parserState, prefixExpr)

parseFunctionCall :: ParserState -> ParseTree -> (ParserState, ParseTree)
parseFunctionCall parserState funcExpr =
  let (parserState', args) = parseFunctionCallArgumentsTuple parserState in
    (parserState', FunctionCall funcExpr args)

parseFunctionCallArgumentsTuple :: ParserState -> (ParserState, [ParseTree])
parseFunctionCallArgumentsTuple parserState =
  let (nextToken:tokensTail) = (tokensLeft parserState) in
    case nextToken of
      (Token { tokenType = LeftParen, tokenText = _ }) ->
        let parserState' = (parserState { tokensLeft = tokensTail }) in
          let (parserState'', args) = parseCommaSeparated parserState' parseExpression in
            let (nextToken'':tokensTail'') = (tokensLeft parserState'') in
              case nextToken'' of
                (Token { tokenType = RightParen, tokenText = _ }) ->
                  let parserState3 = (parserState'' { tokensLeft = tokensTail'' }) in (parserState3, args)
                _ -> error ("Expected ')' but encountered" ++ (show nextToken''))
      _ -> error ("Expected '(' but encountered: " ++ (show nextToken))

parseCommaSeparated :: ParserState -> (ParserState -> (ParserState, ParseTree)) -> (ParserState, [ParseTree])
parseCommaSeparated parserState parseFunc =
  let (parserState', parseTree) = parseFunc parserState in
    let (nextToken':tokensTail') = (tokensLeft parserState') in
      case nextToken' of
        (Token { tokenType = Comma, tokenText = _ }) ->
          let parserState'' = (parserState' { tokensLeft = tokensTail' }) in
            let (parserState3, parseTreesTail) = parseCommaSeparated parserState'' parseFunc in
              (parserState3, parseTree:parseTreesTail)
        _ -> (parserState', [parseTree])