module CodeGenerator

open ASTBuilder
open CompilerCore
open Lexer

type CodeGeneratorState = {
    Program: Program
    GeneratedCode: string
    Errors: List<CompileError>
}

type CodeGeneratorOutput = {
    GeneratedCode: string
    Errors: List<CompileError>
}

let generateString (state: CodeGeneratorState) (str: string): CodeGeneratorState =
    { state with GeneratedCode = state.GeneratedCode + str }

let rec generateMany
    (state: CodeGeneratorState)
    (nodes: List<'a>)
    (generateFn: (CodeGeneratorState -> 'a -> CodeGeneratorState))
    (separator: string)
    (isFirstItem: bool)
    : CodeGeneratorState =
        if nodes.IsEmpty then
            state
        else
            let state =
                if not isFirstItem then
                    generateString state separator
                else
                    state

            let state = generateFn state nodes.Head

            generateMany state nodes.Tail generateFn separator false

let generateSymbol (state: CodeGeneratorState) (token: Token) (expressionId: System.Guid): CodeGeneratorState =
    let symbol = state.Program.ResolvedSymbolsByExpressionId[expressionId]
    // TODO: handle expression not resolved

    match symbol with
    | BindingSymbol binding -> generateString state binding.NameToken.Text
    | ParameterSymbol parameter ->  generateString state parameter.NameToken.Text
    | RecordFieldSymbol recordField -> generateString state recordField.NameToken.Text
    | UnionCaseSymbol unionCase -> generateString state unionCase.NameToken.Text
    | BuiltInSymbol (builtInSymbol, prestoType) -> generateString state builtInSymbol

let rec generateIfThenElse (state: CodeGeneratorState) (ifThenElse: IfThenElse): CodeGeneratorState =
    let state = generateString state "("
    let state = generateString state "("
    let state = generateExpression state ifThenElse.IfExpression
    let state = generateString state ")"
    let state = generateString state " "
    let state = generateString state "?"
    let state = generateString state " "
    let state = generateString state "("
    let state = generateExpression state ifThenElse.ThenExpression
    let state = generateString state ")"
    let state = generateString state " "
    let state = generateString state ":"
    let state = generateString state " "
    let state = generateString state "("
    let state = generateExpression state ifThenElse.ElseExpression
    let state = generateString state ")"
    let state = generateString state ")"

    state

and generateFunctionCallInternal (state: CodeGeneratorState) (functionCall: FunctionCall) (isTypeExpression: bool): CodeGeneratorState =
    let state = generateExpression state functionCall.FunctionExpression
    let state = generateString state (if isTypeExpression then "<" else "(")
    let state = generateMany state functionCall.Arguments generateExpression ", " true
    let state = generateString state (if isTypeExpression then ">" else ")")

    state

and generateMemberAccess (state: CodeGeneratorState) (memberAccess: MemberAccess): CodeGeneratorState =
    let state = generateExpression state memberAccess.LeftExpression
    let state = generateString state "."
    let state = generateString state memberAccess.RightIdentifier.Text

    state

and generateNumberLiteral (state: CodeGeneratorState) (numberLiteral: NumberLiteral): CodeGeneratorState =
    let state = generateString state numberLiteral.Token.Text

    state

and generateExpressionInternal (state: CodeGeneratorState) (expression: Expression) (isTypeExpression: bool): CodeGeneratorState =
    match expression.Value with
    | RecordExpression record -> failwith "Not implemented"
    | UnionExpression union -> failwith "Not implemented"
    | FunctionExpression fn -> failwith "Not implemented"
    | IfThenElseExpression ifThenElse -> generateIfThenElse state ifThenElse
    | FunctionCallExpression call -> generateFunctionCallInternal state call isTypeExpression
    | MemberAccessExpression memberAccess -> generateMemberAccess state memberAccess
    | SymbolReference symbol -> generateSymbol state symbol expression.Id
    | NumberLiteralExpression number -> generateNumberLiteral state number
    
and generateExpression (state: CodeGeneratorState) (expression: Expression): CodeGeneratorState =
    generateExpressionInternal state expression false

and generateTypeReference (state: CodeGeneratorState) (prestoType: PrestoType): CodeGeneratorState =
    let typeReferenceString =
        match prestoType with
        | Nat -> "nat"
        | Text _ -> "string"
        | Boolean -> "bool"
        | Character -> "char"
        | Type -> "Type"
        | RecordType (scopeId, _) -> state.Program.TypeCanonicalNamesByScopeId[scopeId]
        | UnionType scopeId -> state.Program.TypeCanonicalNamesByScopeId[scopeId]
        | FunctionType (scopeId, _, _) -> state.Program.TypeCanonicalNamesByScopeId[scopeId]

    generateString state typeReferenceString

and generateTypeExpression (state: CodeGeneratorState) (expression: Expression): CodeGeneratorState =
    generateExpressionInternal state expression true

let generateParameter (state: CodeGeneratorState) (parameter: Parameter): CodeGeneratorState =
    let state = generateTypeExpression state parameter.TypeExpression
    let state = generateString state " "
    let state = generateString state parameter.NameToken.Text

    state

let generateFunction (state: CodeGeneratorState) (name: string) (fn: Function): CodeGeneratorState =
    let state = generateString state "static"
    let state = generateString state " "

    let returnType = state.Program.TypesByExpressionId[fn.Value.Id]
    let state = generateTypeReference state returnType

    let state = generateString state " "
    let state = generateString state name
    let state = generateString state "("
    let state = generateMany state fn.Parameters generateParameter ", " true
    let state = generateString state ")"
    let state = generateString state " "
    let state = generateString state "{"
    let state = generateString state "return"
    let state = generateString state " "
    let state = generateExpression state fn.Value
    let state = generateString state ";"
    let state = generateString state "}"

    state
    
let generateRecordField (state: CodeGeneratorState) (recordField: RecordField): CodeGeneratorState =
    let state = generateTypeExpression state recordField.TypeExpression
    let state = generateString state " "
    let state = generateString state recordField.NameToken.Text

    state

let generateRecord (state: CodeGeneratorState) (name: string) (record: Record): CodeGeneratorState =
    let state = generateString state "public record "
    let state = generateString state name
    let state = generateString state "("
    let state = generateMany state record.Fields generateRecordField ", " true
    let state = generateString state ")"

    state

let generateUnionCase (state: CodeGeneratorState) (unionCase: UnionCase): CodeGeneratorState =
    let state = generateString state unionCase.NameToken.Text

    state

let generateUnion (state: CodeGeneratorState) (name: string) (union: Union): CodeGeneratorState =
    let state = generateString state "public enum "
    let state = generateString state name
    let state = generateString state "{"
    let state = generateMany state union.Cases generateUnionCase ", " true
    let state = generateString state "}"

    state

let shouldTerminateWithSemicolon (binding: Binding): bool =
    match binding.Value.Value with
    | FunctionExpression fn -> false
    | _ -> true

let generateBinding (state: CodeGeneratorState) (binding: Binding): CodeGeneratorState =
    let state =
        match binding.Value.Value with
        | RecordExpression record -> generateRecord state binding.NameToken.Text record
        | UnionExpression union -> generateUnion state binding.NameToken.Text union
        | FunctionExpression fn -> generateFunction state binding.NameToken.Text fn
        | _ ->
            let state = generateString state binding.NameToken.Text
            let state = generateString state " = "
            let state = generateExpression state binding.Value

            state
    let state =
        if shouldTerminateWithSemicolon binding then
            generateString state ";"
        else state

    state

let generatedCodeHeader =
    "using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;
    
    public static class PrestoProgram {
        static bool eq<T>(T a, T b) => (a != null) ? a.Equals(b) : (b == null);
        static bool not(bool x) => !x;"

let generatedCodeFooter =
    "}"

let generateCode (program: Program): CodeGeneratorOutput =
    let state = { Program = program; GeneratedCode = ""; Errors = [] }

    let state = generateString state generatedCodeHeader
    let state = generateString state "\n\n"
    let state = generateMany state program.Bindings generateBinding "\n" true
    let state = generateString state "\n\n"
    let state = generateString state generatedCodeFooter

    { GeneratedCode = state.GeneratedCode; Errors = state.Errors }