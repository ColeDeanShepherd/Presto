module Parse where

import Text.ParserCombinators.Parsec
import AST

parsePrestoProgram :: String -> Either ParseError [Binding]
parsePrestoProgram = parse programRule "(unknown)"

programRule :: Parser [Binding]
programRule = do
  spaces
  sepBy bindingRule spaces

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
      try $ FnExpr `fmap` fnRule
  <|> bindRefRule

fnRule :: Parser Fn
fnRule = do
  char '('
  spaces
  params <- sepBy paramRule (char ',' >> spaces)
  spaces
  char ')'
  spaces
  string "->"
  spaces
  expr <- exprRule
  return (Fn { fnParams = params, fnType = Nothing, fnValue = expr })

identRule :: Parser Ident
identRule = do
  x <- many letter
  return (Ident { identText = x })

paramRule :: Parser Param
paramRule = do
  name <- identRule
  return (Param { paramName = name, paramType = Nothing })

bindRefRule :: Parser Expr
bindRefRule = do BindRef `fmap` identRule