module Lexer (
  Token(..),
  tokenize
) where

import Data.Char

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