module CodeGenerator

open ASTBuilder
open Lexer

open type PrestoProgram

type CodeGeneratorState = {
    Program: Program
    GeneratedCode: string
    Errors: List<compile_error>
}

type CodeGeneratorOutput = {
    GeneratedCode: string
    Errors: List<compile_error>
}

let generateString (state: CodeGeneratorState) (str: string): CodeGeneratorState =
    { state with GeneratedCode = state.GeneratedCode + str }

let rec generateManyWithSeparator
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

            generateManyWithSeparator state nodes.Tail generateFn separator false

let rec generateManyWithDelimiter
    (state: CodeGeneratorState)
    (nodes: List<'a>)
    (generateFn: (CodeGeneratorState -> 'a -> CodeGeneratorState))
    (delimiter: string)
    (isFirstItem: bool)
    : CodeGeneratorState =
        if nodes.IsEmpty then
            state
        else
            let state = generateFn state nodes.Head
            let state = generateString state delimiter

            generateManyWithDelimiter state nodes.Tail generateFn delimiter false

let rec generateSymbol (state: CodeGeneratorState) (token: token) (expressionId: System.Guid): CodeGeneratorState =
    let symbol = state.Program.ResolvedSymbolsByExpressionId[expressionId]
    // TODO: handle expression not resolved

    match symbol with
    | BindingSymbol binding -> generateString state binding.NameToken._text
    | TypeParameterSymbol typeParameter -> generateString state typeParameter.NameToken._text
    | ParameterSymbol parameter ->  generateString state parameter.NameToken._text
    | RecordFieldSymbol recordField -> generateString state recordField.NameToken._text
    | UnionCaseSymbol unionCase -> generateString state unionCase.NameToken._text
    | BuiltInSymbol (builtInSymbol, prestoType) ->
        // TODO: make less hacky
        match builtInSymbol with
        | "text" -> generateTypeReference state prestoType
        | "list" -> generateString state "List"
        | "seq" -> generateString state "List"
        | "Console" -> generateString state "Presto.Runtime.Console"
        | _ -> generateString state builtInSymbol

and generateGenericInstantiation (state: CodeGeneratorState) (genericInstantiation: GenericInstantiation): CodeGeneratorState =
    let state = generateExpression state genericInstantiation.Expression
    let state = generateString state "<"
    let state = generateManyWithSeparator state genericInstantiation.TypeArguments generateTypeExpression ", " true
    let state = generateString state ">"
    state

and generateIfThenElse (state: CodeGeneratorState) (ifThenElse: IfThenElse): CodeGeneratorState =
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
    let state = generateManyWithSeparator state functionCall.Arguments generateExpression ", " true
    let state = generateString state (if isTypeExpression then ">" else ")")

    state

and generateBlockChild (state: CodeGeneratorState) (blockChild: BlockChild): CodeGeneratorState =
    match blockChild with
    | BlockChildBinding binding ->
        generateBinding state binding
    | BlockChildExpression expression ->
        generateExpression state expression

and generateBlock (state: CodeGeneratorState) (block: Block): CodeGeneratorState =
    if block.Children.Length <> 1 then
        let state = generateString state "{"
        let state = generateString state System.Environment.NewLine
        let state = generateManyWithDelimiter state block.Children generateBlockChild (";" + System.Environment.NewLine) true
        let state = generateString state System.Environment.NewLine
        let state = generateString state "}"

        state
    else
        let state = generateBlockChild state block.Children.Head

        state

and generateMemberAccess (state: CodeGeneratorState) (memberAccess: MemberAccess): CodeGeneratorState =
    let state = generateExpression state memberAccess.LeftExpression
    let state = generateString state "."
    let state = generateString state memberAccess.RightIdentifier._text

    state

and generateNumberLiteral (state: CodeGeneratorState) (numberLiteral: NumberLiteral): CodeGeneratorState =
    let state = generateString state numberLiteral.Token._text
    let state = generateString state "u"

    state

and generateCharacterLiteral (state: CodeGeneratorState) (characterLiteral: CharacterLiteral): CodeGeneratorState =
    let state = generateString state characterLiteral.Token._text

    state

and generateStringLiteral (state: CodeGeneratorState) (stringLiteral: StringLiteral): CodeGeneratorState =
    let state = generateString state stringLiteral.Token._text

    state

and generateExpressionInternal (state: CodeGeneratorState) (expression: Expression) (isTypeExpression: bool): CodeGeneratorState =
    match expression.Value with
    | RecordExpression record -> failwith "Not implemented"
    | UnionExpression union -> failwith "Not implemented"
    | FunctionExpression fn -> failwith "Not implemented"
    | IfThenElseExpression ifThenElse -> generateIfThenElse state ifThenElse
    | FunctionCallExpression call -> generateFunctionCallInternal state call isTypeExpression
    | BlockExpression block -> generateBlock state block
    | MemberAccessExpression memberAccess -> generateMemberAccess state memberAccess
    | SymbolReference symbol -> generateSymbol state symbol expression.Id
    | GenericInstantiationExpression genericInstantiation -> generateGenericInstantiation state genericInstantiation
    | NumberLiteralExpression number -> generateNumberLiteral state number
    | CharacterLiteralExpression character -> generateCharacterLiteral state character
    | StringLiteralExpression string -> generateStringLiteral state string
    
and generateExpression (state: CodeGeneratorState) (expression: Expression): CodeGeneratorState =
    generateExpressionInternal state expression false

and generateTypeReference (state: CodeGeneratorState) (prestoType: PrestoType): CodeGeneratorState =
    let typeReferenceString =
        match prestoType with
        | Nothing -> "void"
        | Nat -> "nat"
        | Text _ -> "string"
        | Boolean -> "bool"
        | Character -> "char"
        | String -> "string"
        | Type -> "Type"
        | RecordType (scopeId, _) ->
            let canonicalName = state.Program.TypeCanonicalNamesByScopeId[scopeId]

            match canonicalName with
            | "Console" -> "Presto.Runtime.Console"
            | _ -> canonicalName
        | UnionType scopeId -> state.Program.TypeCanonicalNamesByScopeId[scopeId]
        | FunctionType (scopeId, _, _, _) -> state.Program.TypeCanonicalNamesByScopeId[scopeId]
        | TypeParameterType typeParameter -> typeParameter

    generateString state typeReferenceString

and generateTypeExpression (state: CodeGeneratorState) (expression: Expression): CodeGeneratorState =
    generateExpressionInternal state expression true

and generateParameter (state: CodeGeneratorState) (parameter: Parameter): CodeGeneratorState =
    let state = generateTypeExpression state parameter.TypeExpression
    let state = generateString state " "
    let state = generateString state parameter.NameToken._text

    state

and generateTypeParameter (state: CodeGeneratorState) (typeParameter: TypeParameter): CodeGeneratorState =
    let state = generateString state typeParameter.NameToken._text

    state

and generateTypeParameters (state: CodeGeneratorState) (typeParameters: List<TypeParameter>): CodeGeneratorState =
    let state = generateString state "<"
    let state = generateManyWithSeparator state typeParameters generateTypeParameter ", " true
    let state = generateString state ">"
    
    state

and generateFunction (state: CodeGeneratorState) (name: string) (fn: Function): CodeGeneratorState =
    let state = generateString state "public"
    let state = generateString state " "
    let state = generateString state "static"
    let state = generateString state " "

    let returnType = state.Program.TypesByExpressionId[fn.Value.Id]
    let state = generateTypeReference state returnType

    let state = generateString state " "
    let state = generateString state name

    let state =
        if fn.TypeParameters.Length > 0 then
            generateTypeParameters state fn.TypeParameters
        else
            state

    let state = generateString state "("
    let state = generateManyWithSeparator state fn.Parameters generateParameter ", " true
    let state = generateString state ")"
    let state = generateString state " "
    let state = generateString state "{"

    let state =
        if returnType <> PrestoType.Nothing then
            let state = generateString state "return"
            let state = generateString state " "
            let state = generateExpression state fn.Value
            let state = generateString state ";"
            state
        else
            let state = generateExpression state fn.Value
            let state = generateString state ";"
            state

    let state = generateString state "}"

    state
    
and generateRecordField (state: CodeGeneratorState) (recordField: RecordField): CodeGeneratorState =
    let state = generateTypeExpression state recordField.TypeExpression
    let state = generateString state " "
    let state = generateString state recordField.NameToken._text

    state

and generateRecord (state: CodeGeneratorState) (name: string) (record: Record): CodeGeneratorState =
    let state = generateString state "public record "
    let state = generateString state name
    let state = generateString state "("
    let state = generateManyWithSeparator state record.Fields generateRecordField ", " true
    let state = generateString state ")"

    state

and generateUnionCase (state: CodeGeneratorState) (unionCase: UnionCase): CodeGeneratorState =
    let state = generateString state unionCase.NameToken._text

    state

and generateUnion (state: CodeGeneratorState) (name: string) (union: Union): CodeGeneratorState =
    let state = generateString state "public enum "
    let state = generateString state name
    let state = generateString state "{"
    let state = generateManyWithSeparator state union.Cases generateUnionCase ", " true
    let state = generateString state "}"

    state

and shouldTerminateWithSemicolon (binding: Binding): bool =
    match binding.Value.Value with
    | FunctionExpression fn -> false
    | _ -> true

and generateBinding (state: CodeGeneratorState) (binding: Binding): CodeGeneratorState =
    let state =
        match binding.Value.Value with
        | RecordExpression record -> generateRecord state binding.NameToken._text record
        | UnionExpression union -> generateUnion state binding.NameToken._text union
        | FunctionExpression fn -> generateFunction state binding.NameToken._text fn
        | _ ->
            let valueType = state.Program.TypesByExpressionId[binding.Value.Id]

            let state = generateTypeReference state valueType
            let state = generateString state " "
            let state = generateString state binding.NameToken._text
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
    
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {"

let generatedCodeFooter (compileGeneratedCSharpCode: bool) =
    if compileGeneratedCSharpCode then
        "public static void Main(string[] args)
        {
            Presto.Runtime.Console console = new();

            main(console);
        }
        }"
    else
        "}"

let generateCode (program: Program) (compileGeneratedCSharpCode: bool): CodeGeneratorOutput =
    let state = { Program = program; GeneratedCode = ""; Errors = [] }

    let state = generateString state generatedCodeHeader
    let state = generateString state (System.Environment.NewLine + System.Environment.NewLine)
    let state = generateManyWithSeparator state program.Bindings generateBinding System.Environment.NewLine true
    let state = generateString state (System.Environment.NewLine + System.Environment.NewLine)
    let state = generateString state (generatedCodeFooter compileGeneratedCSharpCode)

    { GeneratedCode = state.GeneratedCode; Errors = state.Errors }