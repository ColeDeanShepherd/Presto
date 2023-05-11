module TypeChecker

open CompilerCore
open ASTBuilder
open Lexer

open type PrestoProgram

type TypeCheckerState = {
    CurrentScopeId: System.Guid
    ScopesById: Map<System.Guid, Scope>
    TypesByExpressionId: Map<System.Guid, PrestoType>
    ResolvedSymbolsByExpressionId: Map<System.Guid, Symbol>
    TypeCanonicalNamesByScopeId: Map<System.Guid, string>
    Errors: List<compile_error>
}

let getTypeScopeId (state: TypeCheckerState) (prestoType: PrestoType): (Option<System.Guid> * TypeCheckerState) =
    match prestoType with
    | Text scopeId -> (Some scopeId, state)
    | RecordType (scopeId, _) -> (Some scopeId, state)
    | UnionType scopeId -> (Some scopeId, state)
    | _ ->
        let error = compile_error(
            description = $"Couldn't access members of type: {prestoType}",
            position = text_position(line_index = 0u, column_index = 0u)
        )

        (None, { state with Errors = state.Errors @ [error] })

let rec resolveSymbolInternal (state: TypeCheckerState) (scope: Scope) (nameToken: token): (Option<Symbol> * TypeCheckerState) =
    if scope.SymbolsByName.ContainsKey nameToken._text then
        (Some scope.SymbolsByName.[nameToken._text], state)
    else if scope.ParentId.IsSome then
        let parentScope = state.ScopesById[scope.ParentId.Value]

        resolveSymbolInternal state parentScope nameToken
    else
        let error = compile_error(
            description = $"Unknown name: {nameToken._text}",
            position = nameToken.position
        )
        let errors = List.append state.Errors [error]
        (None, { state with Errors = errors })

let resolveSymbol (state: TypeCheckerState) (nameToken: token): (Option<Symbol> * TypeCheckerState) =
    let currentScope = state.ScopesById[state.CurrentScopeId]

    resolveSymbolInternal state currentScope nameToken

let pushScope (state: TypeCheckerState) (scopeId: System.Guid): TypeCheckerState =
    let parentScopeId = state.CurrentScopeId
    let parentScope = state.ScopesById[parentScopeId]

    //if not (List.contains scopeId parentScope.ChildIds) then
    //    failwith $"Incorrectly pushed scope {scopeId}"
    //else
    { state with CurrentScopeId = scopeId }

let popScope (state: TypeCheckerState): TypeCheckerState =
    let scope = state.ScopesById[state.CurrentScopeId]

    { state with CurrentScopeId = scope.ParentId.Value }
    
let rec checkMany
    (state: TypeCheckerState)
    (checkFn: (TypeCheckerState -> 'a -> TypeCheckerState * Option<PrestoType>))
    (nodes: List<'a>)
    (accumulator: List<Option<PrestoType>>):
    TypeCheckerState * List<Option<PrestoType>> =
        if nodes.IsEmpty then
            (state, accumulator)
        else
            let (state, optionExpressionType) = checkFn state nodes.Head
            let accumulator = accumulator @ [optionExpressionType]
            checkMany state checkFn nodes.Tail accumulator

and checkUnion (state: TypeCheckerState) (union: Union): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state union.ScopeId
    (popScope state, Some (UnionType union.ScopeId))

and checkRecordField (state: TypeCheckerState) (recordField: RecordField): TypeCheckerState * Option<PrestoType> =
    checkExpression state recordField.TypeExpression

and checkRecord (state: TypeCheckerState) (record: Record): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state record.ScopeId

    let (state, optionFieldTypes) = checkMany state checkRecordField record.Fields []
    let fieldTypes =
        List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionFieldTypes
        |> List.map (fun x -> x.Value)

    let succeededCheckingFields = fieldTypes.Length = optionFieldTypes.Length

    if succeededCheckingFields then
        let prestoType = RecordType (record.ScopeId, fieldTypes)

        (popScope state, Some prestoType)
    else
        (popScope state, None)
    
and checkParameter (state: TypeCheckerState) (parameter: Parameter): TypeCheckerState * Option<PrestoType> =
    checkExpression state parameter.TypeExpression

// TODO: make sure we don't check types of anything twice via symbol references

and checkFunction (state: TypeCheckerState) (expression: Expression) (fn: Function): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state fn.ScopeId

    let (state, optionParameterTypes) = checkMany state checkParameter fn.Parameters []
    let parameterTypes = List.map<Parameter, PrestoType> (fun x -> state.TypesByExpressionId[x.TypeExpression.Id]) fn.Parameters

    let (state, optionSpecifiedReturnType) =
        match fn.OptionReturnTypeExpression with
        | Some returnTypeExpression -> checkExpression state returnTypeExpression
        | None -> (state, None)

    let state =
        match optionSpecifiedReturnType with
        | Some specifiedReturnType ->
            let prestoType = FunctionType (fn.ScopeId, parameterTypes, specifiedReturnType)
            { state with TypesByExpressionId = state.TypesByExpressionId.Add(expression.Id, prestoType) }
        | None -> state

    // TODO: fix recursion when return type isn't specified
    let (state, optionInferredReturnType) = checkExpression state fn.Value

    match optionInferredReturnType with
    | Some inferredReturnType ->

        if optionSpecifiedReturnType.IsSome then
            let state = popScope state

            if inferredReturnType = optionSpecifiedReturnType.Value then
                (state, Some state.TypesByExpressionId[expression.Id])
            else
                let error = compile_error(
                    description = $"Function's specified return type ({optionSpecifiedReturnType.Value}) doesn't match its inferred return type ({inferredReturnType})",
                    position = text_position(line_index = 0u, column_index = 0u)
                )
                let state = { state with Errors = state.Errors @ [error] }

                (state, None)
        else
            let inferredPrestoType = FunctionType (fn.ScopeId, parameterTypes, inferredReturnType)
            (popScope state, Some inferredPrestoType)
    | None -> (popScope state, None)

and areTypesEqual (a: PrestoType) (b: PrestoType): bool =
    a = b

and checkIfThenElse (state: TypeCheckerState) (ifThenElse: IfThenElse): TypeCheckerState * Option<PrestoType> =
    let (state, optionIfExpressionType) = checkExpression state ifThenElse.IfExpression

    match optionIfExpressionType with
    | Some ifExpressionType ->
        if ifExpressionType = PrestoType.Boolean then
            let (state, optionThenExpressionType) = checkExpression state ifThenElse.ThenExpression

            match optionThenExpressionType with
            | Some thenExpressionType ->
                let (state, optionElseExpressionType) = checkExpression state ifThenElse.ElseExpression

                match optionElseExpressionType with
                | Some elseExpressionType ->
                    if areTypesEqual thenExpressionType elseExpressionType then
                        (state, Some thenExpressionType)
                    else
                        let error = compile_error(
                            description = $"\"then\" type ({thenExpressionType}) doesn't match \"else\" type ({elseExpressionType})",
                            position = text_position(line_index = 0u, column_index = 0u)
                        )
                        let state = { state with Errors = state.Errors @ [error] }

                        (state, None) // TODO: error
                | None -> (state, None)
            | None -> (state, None)
        else
            let error = compile_error(
                description = $"\"if\" type ({ifExpressionType}) isn't a bool",
                position = text_position(line_index = 0u, column_index = 0u)
            )
            let state = { state with Errors = state.Errors @ [error] }

            (state, None) // TODO: error
    | None -> (state, None)

and checkFunctionCall (state: TypeCheckerState) (functionCall: FunctionCall): TypeCheckerState * Option<PrestoType> =
    let (state, optionArgumentTypes) = checkMany state checkExpression functionCall.Arguments []

    let argumentTypes = List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionArgumentTypes |> List.map (fun x -> x.Value)
    let succeededCheckingArguments = argumentTypes.Length = optionArgumentTypes.Length

    // TODO: ensure argument types match function parameter types

    if succeededCheckingArguments then
        let (state, optionFunctionType) = checkExpression state functionCall.FunctionExpression

        match optionFunctionType with
        | Some functionType ->
            match functionType with
            | FunctionType (scopeId, paramTypes, returnType) -> (state, Some returnType)
            | _ ->
                let error = compile_error(
                    description = $"Expected function type, got {functionType}",
                    position = text_position(line_index = 0u, column_index = 0u)
                )
                let state = { state with Errors = state.Errors @ [error]}

                (state, Some functionType)
        | None -> (state, None)
    else
        (state, None)

and checkBlockChild (state: TypeCheckerState) (blockChild: BlockChild): TypeCheckerState * Option<PrestoType> =
    match blockChild with
    | BlockChildBinding binding ->
        checkBinding state binding
    | BlockChildExpression expression ->
        checkExpression state expression

and checkBlock (state: TypeCheckerState) (block: Block): TypeCheckerState * Option<PrestoType> =
    // check the types of all non-last bindings & exprs, then check the type of the last expression
    let (state, optionBlockChildTypes) = checkMany state checkBlockChild block.Children []
    
    let blockChildTypes =
        List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionBlockChildTypes
        |> List.map (fun x -> x.Value)

    let succeededCheckingChildren = blockChildTypes.Length = optionBlockChildTypes.Length

    if succeededCheckingChildren then
        (state, Some (List.last blockChildTypes))
    else
        (state, None)
        
and checkMemberAccess (state: TypeCheckerState) (memberAccess: MemberAccess): TypeCheckerState * Option<PrestoType> =
    let (state, optionLeftType) = checkExpression state memberAccess.LeftExpression

    match optionLeftType with
    | Some leftType ->
        let (optionScopeId, state) = getTypeScopeId state leftType

        match optionScopeId with
        | Some scopeId ->
            let scope = state.ScopesById[scopeId]
            let memberName = memberAccess.RightIdentifier._text

            if scope.SymbolsByName.ContainsKey memberName then
                let rightSymbol = scope.SymbolsByName[memberName]

                let (state, optionRightType) = checkSymbol state rightSymbol

                match optionRightType with
                | Some rightType -> (state, Some rightType)
                | None -> (state, None)
            else
                let error = compile_error(
                    description = $"Invalid member: {memberName}",
                    position = memberAccess.RightIdentifier.position
                )

                (
                    { state with Errors = state.Errors @ [error]},
                    None
                )
        | None -> (state, None)
    | None -> (state, None)

and checkSymbol (state: TypeCheckerState) (symbol: Symbol): TypeCheckerState * Option<PrestoType> =
    match symbol with
    | BindingSymbol binding -> checkExpression state binding.Value
    | ParameterSymbol parameter -> checkExpression state parameter.TypeExpression
    | RecordFieldSymbol recordField -> checkExpression state recordField.TypeExpression
    | UnionCaseSymbol unionCase -> (state, None)
    | BuiltInSymbol (builtInSymbol, prestoType) -> (state, Some prestoType)

and checkSymbolReference (state: TypeCheckerState) (token: token) (expressionId: System.Guid): TypeCheckerState * Option<PrestoType> =
    let (optionSymbol, state) = resolveSymbol state token

    match optionSymbol with
    | Some symbol ->
        let state = { state with ResolvedSymbolsByExpressionId = state.ResolvedSymbolsByExpressionId.Add(expressionId, symbol) }
        checkSymbol state symbol
    | None -> (state, None)

and checkNumberLiteral (state: TypeCheckerState) (numberLiteral: NumberLiteral): TypeCheckerState * Option<PrestoType> =
    (state, Some PrestoType.Nat)

and checkExpression (state: TypeCheckerState) (expression: Expression): TypeCheckerState * Option<PrestoType> =
    if state.TypesByExpressionId.ContainsKey expression.Id then
        let prestoType = state.TypesByExpressionId[expression.Id]

        (state, Some prestoType)
    else
        let (state, optionPrestoType) =
            match expression.Value with
            | RecordExpression record -> checkRecord state record
            | UnionExpression union -> checkUnion state union
            | FunctionExpression fn -> checkFunction state expression fn
            | IfThenElseExpression ifThenElse -> checkIfThenElse state ifThenElse
            | FunctionCallExpression call -> checkFunctionCall state call
            | BlockExpression block -> checkBlock state block
            | MemberAccessExpression memberAccess -> checkMemberAccess state memberAccess
            | SymbolReference token -> checkSymbolReference state token expression.Id
            | NumberLiteralExpression number -> checkNumberLiteral state number

        match optionPrestoType with
        | Some prestoType ->
            (
                { state with TypesByExpressionId = state.TypesByExpressionId.Add(expression.Id, prestoType) },
                optionPrestoType
            )
        | None -> (state, None)

and trySetTypesCanonicalName (state: TypeCheckerState) (scopeId: System.Guid) (name: string): TypeCheckerState =
    if not (state.TypeCanonicalNamesByScopeId.ContainsKey scopeId) then
        { state with TypeCanonicalNamesByScopeId = state.TypeCanonicalNamesByScopeId.Add(scopeId, name) }
    else
        state

and checkBinding (state: TypeCheckerState) (binding: Binding): TypeCheckerState * Option<PrestoType> =
    let name = binding.NameToken._text

    let state =
        match binding.Value.Value with
        | RecordExpression record -> trySetTypesCanonicalName state record.ScopeId name
        | UnionExpression union -> trySetTypesCanonicalName state union.ScopeId name
        | FunctionExpression fn -> trySetTypesCanonicalName state fn.ScopeId name
        | _ -> state

    checkExpression state binding.Value

let checkTypes (program: Program): Program * List<compile_error> =
    let state = {
        CurrentScopeId = program.ScopeId
        ScopesById = program.ScopesById
        TypesByExpressionId = program.TypesByExpressionId
        ResolvedSymbolsByExpressionId = program.ResolvedSymbolsByExpressionId
        TypeCanonicalNamesByScopeId = program.TypeCanonicalNamesByScopeId
        Errors = []
    }
    
    let (state, _) = checkMany state checkBinding program.Bindings []

    (
        {
            program with
                ScopesById = state.ScopesById
                TypesByExpressionId = state.TypesByExpressionId
                ResolvedSymbolsByExpressionId = state.ResolvedSymbolsByExpressionId
                TypeCanonicalNamesByScopeId = state.TypeCanonicalNamesByScopeId
        },
        state.Errors
    )
