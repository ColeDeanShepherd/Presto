module Parse where

import Text.ParserCombinators.Parsec
import AST
import Debug.Trace (trace)

parsePrestoProgram :: String -> FilePath -> Either ParseError [Binding]
parsePrestoProgram srcCode filePath = parse programRule filePath srcCode

programRule :: Parser [Binding]
programRule = do
  spaces
  bindings <- sepBy bindingRule spaces
  eof
  return bindings

bindingRule :: Parser Binding
bindingRule = do
  ident <- identRule
  spaces
  char '='
  spaces
  expr <- exprRule
  return (Binding { bindingName = ident, bindingExpr = expr })

exprRule :: Parser Expr
exprRule = do
      numberLiteralRule
  <|> try (FnExpr `fmap` fnRule)
  <|> matchRule
  <|> bindRefRule

typeExprRule :: Parser TypeExpr
typeExprRule = TypeExpr `fmap` exprRule

fnRule :: Parser Fn
fnRule = do
  char '('
  spaces
  params <- sepBy paramRule (spaces >> char ',' >> spaces)
  spaces
  char ')'
  spaces
  string "->"
  spaces
  expr <- exprRule
  return (Fn { fnParams = params, fnType = Nothing, fnValue = expr })

identRule :: Parser Ident
identRule = do
  x <- many1 (letter <|> char '_')
  return (Ident { identText = x })

numberLiteralRule :: Parser Expr
numberLiteralRule = do
  x <- many1 digit
  return (NumberLiteral x)

paramRule :: Parser Param
paramRule = do
  name <- identRule
  paramType <- optionMaybe (do
    spaces
    char ':'
    spaces
    typeExprRule)
  return (Param { paramName = name, paramType = paramType })

bindRefRule :: Parser Expr
bindRefRule = do BindRef `fmap` identRule

matchRule :: Parser Expr
matchRule = do
  string "match"
  spaces
  expr <- exprRule
  spaces
  rules <- sepBy1 (try matchRuleRule) (try (spaces >> char ',' >> spaces))
  return (MatchExpr expr rules)

matchRuleRule :: Parser MatchRule
matchRuleRule = do
  matchExpr <- exprRule
  spaces
  string "->"
  spaces
  resultExpr <- exprRule
  return (MatchRule { matchExpr = matchExpr, resultExpr = resultExpr })