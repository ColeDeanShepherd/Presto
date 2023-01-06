module Parser

open CompilerCore
open Lexer

(*
Program -> Binding*
Binding -> Identifier "=" Expression
Expression ->
      Function
    | Identifier
Function -> "fn" "(" SepBy(Parameter, ",") ")" "->" Expression
Parameter -> Identifier (":" Expression)?
*)

type ParseNodeType =
    | Program
    | Binding
    | Expression
    | Function
    | Parameter
    | Token
    | Whitespace

type ParseNode = {
    Type: ParseNodeType
    Children: List<ParseNodeType>
}

type ParseOutput = {
    Program: ParseNode
    Errors: List<CompileError>
}

let parse (tokens: List<Token>): ParseOutput = failwith ""