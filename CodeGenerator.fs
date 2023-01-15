module CodeGenerator

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
    | BindingSymbol binding -> { state with GeneratedCode = state.GeneratedCode + binding.Name }
    | ParameterSymbol parameter ->  { state with GeneratedCode = state.GeneratedCode + parameter.Name }

let generateExpression (state: CodeGeneratorOutput) (expression: Expression): CodeGeneratorOutput =
    match expression with
    | FunctionExpression fn -> failwith "Not implemented"
    | SymbolReference symbol -> generateSymbol state symbol

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

let generateBinding (state: CodeGeneratorOutput) (binding: Binding): CodeGeneratorOutput =
    match binding.Value with
    | FunctionExpression fn -> generateFunction state binding.Name fn
    | SymbolReference symbol -> generateSymbol state symbol

let generateCode (program: Program): CodeGeneratorOutput =
    let state = { GeneratedCode = ""; Errors = [] }

    generateMany state program.Bindings generateBinding "\n" true