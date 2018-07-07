module Lexer (
  TokenType(..),
  Token(..),
  tokenize
) where

{-
To-Do:
-Implement & use peekChar & readChar.
-Add token text spans.
-Fill out lexer errors array.
-}
import Data.Char

data LexerState = LexerState {
  strLeft :: String,
  errors :: [String]
}

data TokenType =
    Identifier
  | NumberToken
  | LeftParen
  | RightParen
  | Comma
  deriving (Show)

data Token = Token {
  tokenType :: TokenType,
  tokenText :: String
} deriving (Show)

tokenize :: String -> [Token]
tokenize str = tokenizeGivenState (LexerState { strLeft = str, errors = [] })

tokenizeGivenState :: LexerState -> [Token]
tokenizeGivenState (LexerState { strLeft = [] }) = []
tokenizeGivenState lexerState@(LexerState { strLeft = str@(nextChar:strTail) })
  | isAlpha nextChar =
    let (lexerState', identifier) = readIdentifier lexerState in
      case identifier of
        Just identifier -> identifier:(tokenizeGivenState lexerState')
        otherwise -> tokenizeGivenState lexerState'
  | isDigit nextChar =
    let (lexerState', number) = readNumber lexerState in
      case number of
        Just number -> number:(tokenizeGivenState lexerState')
        otherwise -> tokenizeGivenState lexerState'
  | isSpace nextChar = tokenizeGivenState (lexerState { strLeft = strTail })
  | nextChar == '(' =
    let lexerState' = (lexerState { strLeft = strTail }) in
      (Token { tokenType = LeftParen, tokenText = [nextChar] }):(tokenizeGivenState lexerState')
  | nextChar == ')' =
    let lexerState' = (lexerState { strLeft = strTail }) in
      (Token { tokenType = RightParen, tokenText = [nextChar] }):(tokenizeGivenState lexerState')
  | nextChar == ',' =
    let lexerState' = (lexerState { strLeft = strTail }) in
      (Token { tokenType = Comma, tokenText = [nextChar] }):(tokenizeGivenState lexerState')
  | otherwise = error
    ("Unknown character: " ++ [nextChar])

readIdentifier :: LexerState -> (LexerState, Maybe Token)
readIdentifier lexerState =
  let (tokenText, strLeft') = span isAlpha (strLeft lexerState) in
    if tokenText /= [] then
      (
        lexerState { strLeft = strLeft' },
        Just Token { tokenType = Identifier, tokenText = tokenText }
      )
    else
      (
        lexerState { strLeft = strLeft', errors = (errors lexerState) ++ ["Expected an identifier."] },
        Just Token { tokenType = Identifier, tokenText = tokenText }
      )

readNumber :: LexerState -> (LexerState, Maybe Token)
readNumber lexerState =
  let (tokenText, strLeft') = span isDigit (strLeft lexerState) in
    if tokenText /= [] then
      (
        lexerState { strLeft = strLeft' },
        Just Token { tokenType = NumberToken, tokenText = tokenText }
      )
    else
      (
        lexerState { strLeft = strLeft', errors = (errors lexerState) ++ ["Expected a number."] },
        Just Token { tokenType = Identifier, tokenText = tokenText }
      )