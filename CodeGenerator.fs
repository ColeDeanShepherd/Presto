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

let rec generateFunctionCall (state: CodeGeneratorState) (functionCall: FunctionCall): CodeGeneratorState =
    let state = generateExpression state functionCall.FunctionExpression
    let state = generateString state "("
    let state = generateMany state functionCall.Arguments generateExpression ", " true
    let state = generateString state ")"

    state

and generateMemberAccess (state: CodeGeneratorState) (memberAccess: MemberAccess): CodeGeneratorState =
    let state = generateExpression state memberAccess.LeftExpression
    let state = generateString state "."
    let state = generateString state memberAccess.RightIdentifier.Text

    state

and generateNumberLiteral (state: CodeGeneratorState) (numberLiteral: NumberLiteral): CodeGeneratorState =
    let state = generateString state numberLiteral.Token.Text

    state

and generateExpression (state: CodeGeneratorState) (expression: Expression): CodeGeneratorState =
    match expression.Value with
    | RecordExpression record -> failwith "Not implemented"
    | UnionExpression union -> failwith "Not implemented"
    | FunctionExpression fn -> failwith "Not implemented"
    | FunctionCallExpression call -> generateFunctionCall state call
    | MemberAccessExpression memberAccess -> generateMemberAccess state memberAccess
    | SymbolReference symbol -> generateSymbol state symbol expression.Id
    | NumberLiteralExpression number -> generateNumberLiteral state number
    
and generateTypeExpression (state: CodeGeneratorState) (expression: Expression): CodeGeneratorState =
    generateExpression state expression
    // TODO: fix

let generateParameter (state: CodeGeneratorState) (parameter: Parameter): CodeGeneratorState =
    let state = generateTypeExpression state parameter.TypeExpression
    let state = generateString state " "
    let state = generateString state parameter.NameToken.Text

    state

let generateFunction (state: CodeGeneratorState) (name: string) (fn: Function): CodeGeneratorState =
    let state = generateString state "static"
    let state = generateString state " "
    let state = generateString state "int" // TODO: fix
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
    let state = generateString state ";"

    state

let generatedCodeHeader =
    "using Nat = System.UInt32;
    using Text = System.String;
    
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