module CodeGenerator

open ASTBuilder
open Lexer

open type PrestoProgram

type CodeGeneratorState = {
    Program: Program
    GeneratedCode: string
    Errors: List<CompileError>
    NextResultId: int
}

type CodeGeneratorOutput = {
    GeneratedCode: string
    Errors: List<CompileError>
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

let rec generateSymbol (state: CodeGeneratorState) (token: Token) (expressionId: System.Guid) (isTypeExpression: bool): CodeGeneratorState =
    let symbol = state.Program.ResolvedSymbolsByExpressionId[expressionId]
    // TODO: handle expression not resolved

    let generateName (state: CodeGeneratorState) (name: string) =
        if isTypeExpression then
            generateTypeName state name
        else
            generateString state name

    match symbol with
    | BindingSymbol binding ->
        generateName state binding.NameToken.text
    | TypeParameterSymbol typeParameter ->
        generateName state typeParameter.NameToken.text
    | ParameterSymbol parameter -> 
        generateName state parameter.NameToken.text
    | RecordFieldSymbol recordField ->
        generateName state recordField.NameToken.text
    | UnionCaseSymbol unionCase ->
        generateName state unionCase.NameToken.text
    | BuiltInSymbol (builtInSymbol, prestoType) ->
        // TODO: make less hacky
        match builtInSymbol with
        | "Text" -> generateTypeReference state prestoType
        | "list" -> generateName state "IEnumerable"
        | "Seq" -> generateName state "IEnumerable"
        | "Console" -> generateName state "Presto.Runtime.Console"
        | _ -> generateName state builtInSymbol

and generateGenericInstantiation (state: CodeGeneratorState) (genericInstantiation: GenericInstantiation): CodeGeneratorState =
    let state = generateExpressionInternal state genericInstantiation.Expression true
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

and generateFunctionCallInternal (state: CodeGeneratorState) (expressionId: System.Guid) (functionCall: FunctionCall) (isTypeExpression: bool): CodeGeneratorState =
    let state = generateExpression state functionCall.FunctionExpression

    let state =
        if not isTypeExpression then
            let state =
                let functionType = state.Program.TypesByExpressionId[functionCall.FunctionExpression.Id]

                match functionType with
                | FunctionType ftf ->
                    if not ftf.TypeParamNameAndIds.IsEmpty then
                        let typesByTypeParameterId = state.Program.TypeArgumentsByExpressionId[expressionId]
                        let typeParamIds = List.map fst ftf.TypeParamNameAndIds
                        let typeArgs = List.map (fun id -> typesByTypeParameterId[id]) typeParamIds

                        let state = generateString state "<"
                        let state = generateManyWithSeparator state typeArgs generateTypeReference ", " true
                        let state = generateString state ">"

                        state
                    else
                        state
                | _ -> state

            state
        else
            state

    let state = generateString state (if isTypeExpression then "<" else "(")
    let state = generateManyWithSeparator state functionCall.Arguments generateExpression ", " true
    let state = generateString state (if isTypeExpression then ">" else ")")

    state

and generateBlockChild (state: CodeGeneratorState) (blockChild: BlockChild): CodeGeneratorState =
    match blockChild with
    | BlockChildBinding binding ->
        generateBinding state binding false
    | BlockChildExpression expression ->
        let state =
            match expression.Value with
            | BlockExpression _ -> state
            | ErrorPropagationExpression _ -> state
            | _ -> state //generateString state "_ = "
                    
        generateExpression state expression

and generateBlock (state: CodeGeneratorState) (expression: Expression) (block: Block): CodeGeneratorState =
    let resultType = state.Program.TypesByExpressionId[expression.Id]

    let state = generateString state "__exec<"
    let state = generateTypeReference state resultType
    let state = generateString state ">(() => "

    let state = generateString state "{"
    let state = generateString state System.Environment.NewLine

    let state =
        if not block.Children.IsEmpty then
            let childrenExceptLast = List.take (block.Children.Length - 1) block.Children
            let state = generateManyWithDelimiter state childrenExceptLast generateBlockChild (";" + System.Environment.NewLine) true

            let lastChild = List.last block.Children
            let state = generateString state "return "
            let state = generateBlockChild state lastChild
            let state = generateString state ";"

            state
        else
            let state = generateString state "return Unit.Instance;"

            state

    let state = generateString state System.Environment.NewLine
    let state = generateString state "}"
    
    let state = generateString state ")"

    state

and generateMemberAccess (state: CodeGeneratorState) (memberAccess: MemberAccess): CodeGeneratorState =
    let state = generateExpression state memberAccess.LeftExpression
    let state = generateString state "."
    let state = generateString state memberAccess.RightIdentifier.text

    state

and generateBinaryOperator (state: CodeGeneratorState) (expressionId: System.Guid) (binaryOperator: BinaryOperator): CodeGeneratorState =
    match binaryOperator.Type with
    | BinaryOperatorType.ReverseFunctionComposition ->
        let state = generateString state "ReverseCompose"
        
        let binaryOperatorType = state.Program.TypesByExpressionId[expressionId]
        let leftType = state.Program.TypesByExpressionId[binaryOperator.LeftExpression.Id]
        let rightType = state.Program.TypesByExpressionId[binaryOperator.RightExpression.Id]
        let rightTypeTypeArgs = state.Program.TypeArgumentsByExpressionId[expressionId]

        let state =
            match (binaryOperatorType, leftType, rightType) with
            | (FunctionType bftf, FunctionType lftf, FunctionType rftf) ->
                let state = generateString state "<"
                let state = generateManyWithSeparator state bftf.ParamTypes generateTypeReference ", " true
                let state = generateString state ", "
                let state = generateTypeReference state lftf.ReturnType
                let state = generateString state ", "
                let state = generateTypeReference state bftf.ReturnType
                let state = generateString state ">"
                state
            | _ -> failwith ""


        let state = generateString state "("
        let state = generateExpression state binaryOperator.LeftExpression
        let state = generateString state ", "
        let state = generateExpression state binaryOperator.RightExpression
        let state = generateString state ")"
        
        state
    | _ ->
        let state = generateExpression state binaryOperator.LeftExpression
        let state = generateString state " "
        let state = generateBinaryOperatorType state binaryOperator.Type
        let state = generateString state " "
        let state = generateExpression state binaryOperator.RightExpression

        state

and generateBinaryOperatorType (state: CodeGeneratorState) (_type: BinaryOperatorType): CodeGeneratorState =
    match _type with
    | BinaryOperatorType.Addition -> generateString state "+"
    | BinaryOperatorType.Subtraction -> generateString state "-"
    | BinaryOperatorType.Multiplication -> generateString state "*"
    | BinaryOperatorType.Division -> generateString state "/"
    | BinaryOperatorType.Equality -> generateString state "=="
    | BinaryOperatorType.ReverseFunctionComposition -> failwith ""

and generateNumberLiteral (state: CodeGeneratorState) (numberLiteral: NumberLiteral): CodeGeneratorState =
    let state = generateString state numberLiteral.Token.text

    let state =
        if numberLiteral.Token.text.Contains('.') then
            generateString state "m"
        else
            generateString state "u"

    state

and generateCharacterLiteral (state: CodeGeneratorState) (characterLiteral: CharacterLiteral): CodeGeneratorState =
    let state = generateString state characterLiteral.Token.text

    state

and generateStringLiteral (state: CodeGeneratorState) (stringLiteral: StringLiteral): CodeGeneratorState =
    let state = generateString state stringLiteral.Token.text

    state

and generateParenthesizedExpression (state: CodeGeneratorState) (pe: ParenthesizedExpression2): CodeGeneratorState =
    let state = generateString state "("
    let state = generateExpression state pe.InnerExpression
    let state = generateString state ")"

    state

and generateTupleExpression (state: CodeGeneratorState) (pe: TupleExpression): CodeGeneratorState =
    let state = generateString state "("
    let state = generateManyWithSeparator state pe.Values generateExpression ", " true
    let state = generateString state ")"

    state

and generateResultId (state: CodeGeneratorState): int * CodeGeneratorState =
    (state.NextResultId, { state with NextResultId = state.NextResultId + 1 })

and generateResultName (id: int): string =
    "___result" + id.ToString()

and generateErrorPropagationExpression (state: CodeGeneratorState) (e: ErrorPropagationOperatorExpression): CodeGeneratorState =
    let (resultId, state) = generateResultId state
    let resultName = generateResultName resultId

    let state = generateString state ("var " + resultName + " = ")
    let state = generateExpression state e.InnerExpression
    let state = generateString state ";"
    
    let state = generateString state (System.Environment.NewLine)

    let state = generateString state ("if (" + resultName + ".IsErr) { return Result.ErrUnit(" + resultName + ".Error); }")

    state

and generateSelfTypeExpr (state: CodeGeneratorState) (expressionId: System.Guid) (traitId: System.Guid): CodeGeneratorState =
    let traitName = state.Program.TypeCanonicalNamesByScopeId[traitId]
    let state = generateString state traitName

    let ttf = state.Program.TraitTypeFieldsByScopeId[traitId]
    
    let typeParamNames = List.map snd ttf.TypeParamNameAndIds
    let state = generateString state "<"
    let state = generateManyWithSeparator state typeParamNames generateString ", " true
    let state = generateString state ">"

    state

and generateExpressionInternal (state: CodeGeneratorState) (expression: Expression) (isTypeExpression: bool): CodeGeneratorState =
    match expression.Value with
    | RecordExpression record -> failwith "Not implemented"
    | UnionExpression union -> failwith "Not implemented"
    | FunctionExpression fn -> generateLambdaExpression state fn
    | IfThenElseExpression ifThenElse -> generateIfThenElse state ifThenElse
    | FunctionCallExpression call -> generateFunctionCallInternal state expression.Id call isTypeExpression
    | BlockExpression block -> generateBlock state expression block
    | MemberAccessExpression memberAccess -> generateMemberAccess state memberAccess
    | BinaryOperatorExpression binaryOperator -> generateBinaryOperator state expression.Id binaryOperator
    | SymbolReference symbol -> generateSymbol state symbol expression.Id isTypeExpression
    | GenericInstantiationExpression genericInstantiation -> generateGenericInstantiation state genericInstantiation
    | NumberLiteralExpression number -> generateNumberLiteral state number
    | CharacterLiteralExpression character -> generateCharacterLiteral state character
    | StringLiteralExpression string -> generateStringLiteral state string
    | ParenExpr pe -> generateParenthesizedExpression state pe
    | TupleExpr te -> generateTupleExpression state te
    | ErrorPropagationExpression e -> generateErrorPropagationExpression state e
    | SelfTypeExpr traitId -> generateSelfTypeExpr state expression.Id traitId
    
and generateExpression (state: CodeGeneratorState) (expression: Expression): CodeGeneratorState =
    generateExpressionInternal state expression false

and generateTypeName (state: CodeGeneratorState) (name: string): CodeGeneratorState =
    match name with
    | "Seq" -> generateString state "IEnumerable"
    | "Grouping" -> generateString state "IGrouping"
    | "Nothing" -> generateString state "Unit"
    | _ -> generateString state name

and generateTypeReference (state: CodeGeneratorState) (prestoType: PrestoType): CodeGeneratorState =
    match prestoType with
    | Nothing -> generateString state "Unit"
    | Nat -> generateString state "Nat"
    | Real -> generateString state "Real"
    | Text _ -> generateString state "Text"
    | Boolean -> generateString state "bool"
    | Character -> generateString state "char"
    | Type -> generateString state "Type"
    | RecordType ft ->
        let canonicalName = state.Program.TypeCanonicalNamesByScopeId[ft.ScopeId]

        match canonicalName with
        | "Console" -> generateString state "Presto.Runtime.Console"
        | _ -> generateString state canonicalName
    | UnionType ut -> generateString state state.Program.TypeCanonicalNamesByScopeId[ut.ScopeId]
    | FunctionType ft -> generateString state state.Program.TypeCanonicalNamesByScopeId[ft.ScopeId]
    | TypeParameterType (id, name) -> generateString state name
    | UnionInstanceType (ut, typeParameters) ->
        let canonicalName = state.Program.TypeCanonicalNamesByScopeId[ut.ScopeId]
        let state = generateString state canonicalName

        let state =
            if typeParameters.IsEmpty then
                state
            else
                let state = generateString state "<"
                let state = generateManyWithSeparator state typeParameters generateTypeReference ", " true
                let state = generateString state ">"
                state

        state
    | TraitInstanceType (tct, typeParameters) ->
        let canonicalName = state.Program.TypeCanonicalNamesByScopeId[tct.ScopeId]
        let state = generateTypeName state canonicalName

        let state =
            if typeParameters.IsEmpty then
                state
            else
                let state = generateString state "<"
                let state = generateManyWithSeparator state typeParameters generateTypeReference ", " true
                let state = generateString state ">"
                state

        state
    | RecordInstanceType (rt, typeParameters) ->
        let canonicalName = state.Program.TypeCanonicalNamesByScopeId[rt.ScopeId]
        let state = generateTypeName state canonicalName

        let state =
            if typeParameters.IsEmpty then
                state
            else
                let state = generateString state "<"
                let state = generateManyWithSeparator state typeParameters generateTypeReference ", " true
                let state = generateString state ">"
                state

        state
    | TupleType elementTypes ->
        let state = generateString state "("
        let state = generateManyWithSeparator state elementTypes generateTypeReference ", " true
        let state = generateString state ")"

        state

and generateTypeExpression (state: CodeGeneratorState) (expression: Expression): CodeGeneratorState =
    generateExpressionInternal state expression true

and generateParameter (state: CodeGeneratorState) (parameter: Parameter): CodeGeneratorState =
    let state = generateTypeExpression state parameter.TypeExpression
    let state = generateString state " "
    let state = generateString state parameter.NameToken.text

    state

and generateTypeParameter (state: CodeGeneratorState) (typeParameter: TypeParameter): CodeGeneratorState =
    let state = generateString state typeParameter.NameToken.text

    state

and generateTypeParameters (state: CodeGeneratorState) (typeParameters: List<TypeParameter>): CodeGeneratorState =
    if typeParameters.IsEmpty then
        state
    else
        let state = generateString state "<"
        let state = generateManyWithSeparator state typeParameters generateTypeParameter ", " true
        let state = generateString state ">"
    
        state

and isImplInCSharpFunction (functionExpression: Expression): bool =
    match functionExpression.Value with
    | GenericInstantiationExpression gi ->
        match gi.Expression.Value with
        | SymbolReference token when token.text = "impl_in_CSharp" -> true
        | _ -> false
    | _ -> false

and generateFunction (state: CodeGeneratorState) (name: string) (fn: Function) (isTopLevel: bool): CodeGeneratorState =
    match fn.Value.Value with
    | FunctionCallExpression functionCall when isImplInCSharpFunction functionCall.FunctionExpression ->
        state
    | _ -> 
        let state =
            if isTopLevel then
                let state = generateString state "public"
                let state = generateString state " "
                let state = generateString state "static"
                let state = generateString state " "

                state
            else
                state

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

        let state = generateString state "return"
        let state = generateString state " "
        let state = generateExpression state fn.Value
        let state = generateString state ";"

        let state = generateString state "}"

        state

and generateLambdaExpression (state: CodeGeneratorState) (fn: Function): CodeGeneratorState =
    let state = generateString state "("
    let state = generateManyWithSeparator state fn.Parameters generateParameter ", " true
    let state = generateString state ")"
    let state = generateString state " => "
    let state = generateString state "{"

    let state = generateString state "return"
    let state = generateString state " "
    let state = generateExpression state fn.Value
    let state = generateString state ";"

    let state = generateString state "}"

    state
    
and generateRecordField (state: CodeGeneratorState) (recordField: RecordField): CodeGeneratorState =
    let state = generateTypeExpression state recordField.TypeExpression
    let state = generateString state " "
    let state = generateString state recordField.NameToken.text

    state

and generateRecord (state: CodeGeneratorState) (name: string) (record: Record): CodeGeneratorState =
    let state = generateString state "public record "
    let state = generateString state name

    let state =
        if record.TypeParameters.Length > 0 then
            generateTypeParameters state record.TypeParameters
        else
            state

    let state = generateString state "("
    let state = generateManyWithSeparator state record.Fields generateRecordField ", " true
    let state = generateString state ")"

    state

and generateUnionCase (state: CodeGeneratorState) (unionCase: UnionCase): CodeGeneratorState =
    let state = generateString state unionCase.NameToken.text

    state

and generateUnion (state: CodeGeneratorState) (name: string) (union: Union): CodeGeneratorState =
    let state = generateString state "public enum "
    let state = generateString state name
    let state = generateString state "{"
    let state = generateManyWithSeparator state union.Cases generateUnionCase ", " true
    let state = generateString state "}"

    state

and getFunctionTypeFromFunctionTypeExpr (x: ExpressionValue) =
    match x with
    | FunctionTypeExpression ft -> ft
    | _ -> failwith ""

and isSelfExpr (x: ExpressionValue): bool =
    match x with
    | SelfTypeExpr _ -> true
    | _ -> false

and generateFunctionSignatureWithoutSelfParams (state: CodeGeneratorState) (name: string) (ft: FunctionType): CodeGeneratorState =
    let state = generateTypeExpression state ft.ReturnTypeExpression
    let state = generateString state " "
    let state = generateString state name

    let state =
        if ft.TypeParameters.Length > 0 then
            generateTypeParameters state ft.TypeParameters
        else
            state

    let state = generateString state "("

    let parameters = List.filter<Parameter> (fun p -> not (isSelfExpr p.TypeExpression.Value)) ft.Parameters
    let state = generateManyWithSeparator state parameters generateParameter ", " true

    let state = generateString state ")"

    state

and generateTrait (state: CodeGeneratorState) (name: string) (_trait: Trait): CodeGeneratorState =
    let state = generateString state "public interface "
    let state = generateString state name

    let state =
        if _trait.TypeParameters.Length > 0 then
            generateTypeParameters state _trait.TypeParameters
        else
            state

    let state = generateString state "{"

    let functionTypeExpressions = List.map<Binding, string * FunctionType> (fun b -> (b.NameToken.text, (getFunctionTypeFromFunctionTypeExpr b.Value.Value))) _trait.Bindings
    let generateFunctionSignatureHelper (state: CodeGeneratorState) (x: string * FunctionType) =
        let (name, ft) = x
        generateFunctionSignatureWithoutSelfParams state name ft

    let state =
        if functionTypeExpressions.IsEmpty then
            state
        else
            generateManyWithDelimiter state functionTypeExpressions generateFunctionSignatureHelper (";" + System.Environment.NewLine) true

    let state = generateString state "}"

    state

and shouldTerminateWithSemicolon (binding: Binding): bool =
    match binding.Value.Value with
    | FunctionExpression fn -> false
    | _ -> true
    
and generateTopLevelBinding (state: CodeGeneratorState) (binding: Binding): CodeGeneratorState =
    generateBinding state binding true
    
and generateNonTopLevelBinding (state: CodeGeneratorState) (binding: Binding): CodeGeneratorState =
    generateBinding state binding false
    
and generateBinding (state: CodeGeneratorState) (binding: Binding) (isTopLevel: bool): CodeGeneratorState =
    let state =
        match binding.Value.Value with
        | RecordExpression record -> generateRecord state binding.NameToken.text record
        | UnionExpression union -> generateUnion state binding.NameToken.text union
        | TraitExpression _trait -> generateTrait state binding.NameToken.text _trait
        | FunctionExpression fn ->
            let state = generateFunction state binding.NameToken.text fn isTopLevel

            state
        | _ ->
            let valueType = state.Program.TypesByExpressionId[binding.Value.Id]

            match binding.Value.Value with
            | ErrorPropagationExpression e -> 
                let nextResultName = generateResultName state.NextResultId
                let state = generateExpression state binding.Value
                let state = generateString state System.Environment.NewLine

                let state = generateTypeReference state valueType
                let state = generateString state " "
                let state = generateString state binding.NameToken.text
                let state = generateString state " = "
                let state = generateString state nextResultName
                let state = generateString state ".Value"

                state
            | _ ->

                let state = generateTypeReference state valueType
                let state = generateString state " "
                let state = generateString state binding.NameToken.text
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
    
    using Bool = System.Boolean;
    using Nat = System.UInt32;
    using Real = System.Decimal;
    using Text = System.String;
    
    using Presto.Runtime;
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {"

let generatedCodeFooter (program: Program) (compileLibrary: bool) (generateMain: bool) =
    if (not compileLibrary) && generateMain then
        let mainFnBinding = List.find<Binding> (fun b -> b.NameToken.text = "main") program.Bindings
        let mainFnType = program.TypesByExpressionId[mainFnBinding.Value.Id]
        let mainArgsStr =
            match mainFnType with
            | FunctionType ftf ->
                List.map (fun pt -> "null") ftf.ParamTypes |> String.concat ", "
            | _ -> failwith ""

        "public static void Main(string[] args)
        {
            main(" + mainArgsStr + ");
        }
        }"
    else
        "}"

let generateCode (program: Program) (compileLibrary: bool) (generateMain: bool): CodeGeneratorOutput =
    let state = { Program = program; GeneratedCode = ""; Errors = []; NextResultId = 0 }

    let state = generateString state generatedCodeHeader
    let state = generateString state (System.Environment.NewLine + System.Environment.NewLine)
    let state = generateManyWithSeparator state program.Bindings generateTopLevelBinding System.Environment.NewLine true
    let state = generateString state (System.Environment.NewLine + System.Environment.NewLine)
    let state = generateString state (generatedCodeFooter program compileLibrary generateMain)

    { GeneratedCode = state.GeneratedCode; Errors = state.Errors }