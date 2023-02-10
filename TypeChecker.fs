module TypeChecker

open CompilerCore
open ASTBuilder
open Lexer

type TypeCheckerState = {
    CurrentScopeId: System.Guid
    ScopesById: Map<System.Guid, Scope>
    TypesByExpressionId: Map<System.Guid, PrestoType>
    ResolvedSymbolsByExpressionId: Map<System.Guid, Symbol>
    TypeCanonicalNamesByScopeId: Map<System.Guid, string>
    Errors: List<CompileError>
}

let getTypeScopeId (state: TypeCheckerState) (prestoType: PrestoType): (Option<System.Guid> * TypeCheckerState) =
    match prestoType with
    | Text scopeId -> (Some scopeId, state)
    | RecordType (scopeId, _) -> (Some scopeId, state)
    | UnionType scopeId -> (Some scopeId, state)
    | _ -> (None, { state with Errors = state.Errors @ [{ Description = $"Couldn't access members of type: {prestoType}"; Position = { ColumnIndex = 0; LineIndex = 0 } }] })

let rec resolveSymbolInternal (state: TypeCheckerState) (scope: Scope) (nameToken: Token): (Option<Symbol> * TypeCheckerState) =
    if scope.SymbolsByName.ContainsKey nameToken.Text then
        (Some scope.SymbolsByName.[nameToken.Text], state)
    else if scope.ParentId.IsSome then
        let parentScope = state.ScopesById[scope.ParentId.Value]

        resolveSymbolInternal state parentScope nameToken
    else
        let errors = List.append state.Errors [{ Description = $"Unknown name: {nameToken.Text}"; Position = nameToken.Position }]
        (None, { state with Errors = errors })

let resolveSymbol (state: TypeCheckerState) (nameToken: Token): (Option<Symbol> * TypeCheckerState) =
    let currentScope = state.ScopesById[state.CurrentScopeId]

    resolveSymbolInternal state currentScope nameToken

let pushScope (state: TypeCheckerState) (scopeId: System.Guid): TypeCheckerState =
    let parentScopeId = state.CurrentScopeId
    let parentScope = state.ScopesById[parentScopeId]

    if not (List.contains scopeId parentScope.ChildIds) then
        failwith $"Incorrectly pushed scope {scopeId}"
    else
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

and checkFunction (state: TypeCheckerState) (fn: Function): TypeCheckerState * Option<PrestoType> =
    let state = pushScope state fn.ScopeId
    let (state, optionParameterTypes) = checkMany state checkParameter fn.Parameters []
    let parameterTypes = List.map<Parameter, PrestoType> (fun x -> state.TypesByExpressionId[x.TypeExpression.Id]) fn.Parameters
    let (state, optionReturnType) = checkExpression state fn.Value

    match optionReturnType with
    | Some returnType ->
        let prestoType = FunctionType (fn.ScopeId, parameterTypes, returnType)

        (popScope state, Some prestoType)
    | None -> (popScope state, None)

and checkFunctionCall (state: TypeCheckerState) (functionCall: FunctionCall): TypeCheckerState * Option<PrestoType> =
    let (state, optionArgumentTypes) = checkMany state checkExpression functionCall.Arguments []

    let argumentTypes = List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionArgumentTypes |> List.map (fun x -> x.Value)
    let succeededCheckingArguments = argumentTypes.Length = optionArgumentTypes.Length

    if succeededCheckingArguments then
        let (state, optionFunctionType) = checkExpression state functionCall.FunctionExpression

        match optionFunctionType with
        | Some functionType ->
            match functionType with
            | FunctionType (scopeId, paramTypes, returnType) -> (state, Some returnType)
            | _ ->
                let state = { state with Errors = state.Errors @ [{ Description = $"Expected function type, got {functionType}"; Position = { LineIndex = 0; ColumnIndex = 0 } }]}

                (state, Some functionType)
        | None -> (state, None)
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
            let memberName = memberAccess.RightIdentifier.Text

            if scope.SymbolsByName.ContainsKey memberName then
                let rightSymbol = scope.SymbolsByName[memberName]

                let (state, optionRightType) = checkSymbol state rightSymbol

                match optionRightType with
                | Some rightType -> (state, Some rightType)
                | None -> (state, None)
            else
                (
                    { state with Errors = state.Errors @ [{ Description = $"Invalid member: {memberName}"; Position = memberAccess.RightIdentifier.Position }]},
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

and checkSymbolReference (state: TypeCheckerState) (token: Token) (expressionId: System.Guid): TypeCheckerState * Option<PrestoType> =
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
            | FunctionExpression fn -> checkFunction state fn
            | FunctionCallExpression call -> checkFunctionCall state call
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

let trySetTypeCanonicalName (state: TypeCheckerState) (scopeId: System.Guid) (name: string): TypeCheckerState =
    if not (state.TypeCanonicalNamesByScopeId.ContainsKey scopeId) then
        { state with TypeCanonicalNamesByScopeId = state.TypeCanonicalNamesByScopeId.Add(scopeId, name) }
    else
        state

let checkBinding (state: TypeCheckerState) (binding: Binding): TypeCheckerState * Option<PrestoType> =
    let name = binding.NameToken.Text

    let state =
        match binding.Value.Value with
        | RecordExpression record -> trySetTypeCanonicalName state record.ScopeId name
        | UnionExpression union -> trySetTypeCanonicalName state union.ScopeId name
        | FunctionExpression fn -> trySetTypeCanonicalName state fn.ScopeId name
        | _ -> state

    checkExpression state binding.Value

let checkTypes (program: Program): Program * List<CompileError> =
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

// resolve symbols
// find types of expressions