﻿module CodeGenerator

open ASTBuilder
open CompilerCore

type CodeGeneratorOutput = {
    GeneratedCode: string
    Errors: List<CompileError>
}

let generateString (state: CodeGeneratorOutput) (str: string): CodeGeneratorOutput =
    { state with GeneratedCode = state.GeneratedCode + str }

let rec generateMany
    (state: CodeGeneratorOutput)
    (nodes: List<'a>)
    (generateFn: (CodeGeneratorOutput -> 'a -> CodeGeneratorOutput))
    (separator: string)
    (isFirstItem: bool)
    : CodeGeneratorOutput =
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

let generateSymbol (state: CodeGeneratorOutput) (symbol: Symbol): CodeGeneratorOutput =
    match symbol with
    | BindingSymbol binding -> generateString state binding.Name
    | ParameterSymbol parameter ->  generateString state parameter.Name
    | RecordFieldSymbol recordField -> generateString state recordField.NameToken.Text
    | UnionCaseSymbol unionCase -> generateString state unionCase.NameToken.Text
    | BuiltInSymbol builtInSymbol -> generateString state builtInSymbol
    | UnresolvedSymbol token -> { state with Errors = state.Errors @ [{ Description = $"Tried to generate unresolved symbol \"{token.Text}\"."; Position = getSymbolTextPosition symbol }] }

let rec generateFunctionCall (state: CodeGeneratorOutput) (functionCall: FunctionCall): CodeGeneratorOutput =
    let state = generateExpression state functionCall.FunctionExpression
    let state = generateString state "("
    let state = generateMany state functionCall.Arguments generateExpression ", " true
    let state = generateString state ")"

    state

and generateMemberAccess (state: CodeGeneratorOutput) (memberAccess: MemberAccess): CodeGeneratorOutput =
    let state = generateExpression state memberAccess.LeftExpression
    let state = generateString state "."
    let state = generateExpression state memberAccess.RightExpression

    state

and generateNumberLiteral (state: CodeGeneratorOutput) (numberLiteral: NumberLiteral): CodeGeneratorOutput =
    let state = generateString state numberLiteral.Token.Text

    state

and generateExpression (state: CodeGeneratorOutput) (expression: Expression): CodeGeneratorOutput =
    match expression.Value with
    | RecordExpression record -> failwith "Not implemented"
    | UnionExpression union -> failwith "Not implemented"
    | FunctionExpression fn -> failwith "Not implemented"
    | FunctionCallExpression call -> generateFunctionCall state call
    | MemberAccessExpression memberAccess -> generateMemberAccess state memberAccess
    | SymbolReference symbol -> generateSymbol state symbol
    | NumberLiteralExpression number -> generateNumberLiteral state number

let generateParameter (state: CodeGeneratorOutput) (parameter: Parameter): CodeGeneratorOutput =
    let state = generateString state "int"
    let state = generateString state " "
    let state = generateString state parameter.Name

    state

let generateFunction (state: CodeGeneratorOutput) (name: string) (fn: Function): CodeGeneratorOutput =
    let state = generateString state "int"
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
    
let generateRecordField (state: CodeGeneratorOutput) (recordField: RecordField): CodeGeneratorOutput =
    let state = generateExpression state recordField.TypeExpression
    let state = generateString state " "
    let state = generateString state recordField.NameToken.Text

    state

let generateRecord (state: CodeGeneratorOutput) (name: string) (record: Record): CodeGeneratorOutput =
    let state = generateString state "public record "
    let state = generateString state name
    let state = generateString state "("
    let state = generateMany state record.Fields generateRecordField ", " true
    let state = generateString state ")"

    state

let generateUnionCase (state: CodeGeneratorOutput) (unionCase: UnionCase): CodeGeneratorOutput =
    let state = generateString state unionCase.NameToken.Text

    state

let generateUnion (state: CodeGeneratorOutput) (name: string) (union: Union): CodeGeneratorOutput =
    let state = generateString state "public enum "
    let state = generateString state name
    let state = generateString state "{"
    let state = generateMany state union.Cases generateUnionCase ", " true
    let state = generateString state "}"

    state

let generateBinding (state: CodeGeneratorOutput) (binding: Binding): CodeGeneratorOutput =
    let state =
        match binding.Value.Value with
        | RecordExpression record -> generateRecord state binding.Name record
        | UnionExpression union -> generateUnion state binding.Name union
        | FunctionExpression fn -> generateFunction state binding.Name fn
        | _ ->
            let state = generateString state binding.Name
            let state = generateString state " = "
            let state = generateExpression state binding.Value

            state
    let state = generateString state ";"

    state

let generatedCodeHeader =
    "using Nat = uint;
    using Text = string;

    bool eq<T>(T a, T b) => a == b;
    bool not(bool x) => !x;"

let generateCode (program: Program): CodeGeneratorOutput =
    let state = { GeneratedCode = ""; Errors = [] }

    let state = generateString state generatedCodeHeader
    let state = generateMany state program.Bindings generateBinding "\n" true

    state