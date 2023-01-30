module TypeChecker

open CompilerCore
open ASTBuilder
open Lexer

type TypeCheckerState = {
    CurrentScopeId: System.Guid
    ScopesById: Map<System.Guid, Scope>
    TypesByExpressionId: Map<System.Guid, PrestoType>
    Errors: List<CompileError>
}

let rec resolveSymbolInternal (state: TypeCheckerState) (scope: Scope) (nameToken: Token): (Option<Symbol> * TypeCheckerState) =
    if scope.SymbolsByName.ContainsKey nameToken.Text then
        (Some scope.SymbolsByName.[nameToken.Text], state)
    else if scope.ParentId.IsSome then
        let parentScope = state.ScopesById[scope.ParentId.Value]

        resolveSymbolInternal state parentScope nameToken
    else
        // TODO: init position
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
    (state, Some UnionType)

and checkRecordField (state: TypeCheckerState) (recordField: RecordField): TypeCheckerState * Option<PrestoType> =
    checkExpression state recordField.TypeExpression

and checkRecord (state: TypeCheckerState) (record: Record): TypeCheckerState * Option<PrestoType> =
    let (state, optionFieldTypes) = checkMany state checkRecordField record.Fields []
    let fieldTypes = List.map (fun x -> state.TypesByExpressionId[x.TypeExpression.Id]) record.Fields
    let prestoType = RecordType fieldTypes

    (state, Some prestoType)
    
and checkParameter (state: TypeCheckerState) (parameter: Parameter): TypeCheckerState * Option<PrestoType> =
    checkExpression state parameter.TypeExpression

and checkFunction (state: TypeCheckerState) (fn: Function): TypeCheckerState * Option<PrestoType> =
    let (state, optionParameterTypes) = checkMany state checkParameter fn.Parameters []
    let parameterTypes = List.map<Parameter, PrestoType> (fun x -> state.TypesByExpressionId[x.TypeExpression.Id]) fn.Parameters
    let (state, optionReturnType) = checkExpression state fn.Value

    match optionReturnType with
    | Some returnType ->
        let prestoType = FunctionType (parameterTypes, returnType)

        (state, Some prestoType)
    | None -> (state, None)

and checkFunctionCall (state: TypeCheckerState) (functionCall: FunctionCall): TypeCheckerState * Option<PrestoType> =
    let (state, optionArgumentTypes) = checkMany state checkExpression functionCall.Arguments []

    let argumentTypes = List.filter<Option<PrestoType>> (fun x -> x.IsSome) optionArgumentTypes |> List.map (fun x -> x.Value)
    let succeededCheckingArguments = argumentTypes.Length = optionArgumentTypes.Length

    if succeededCheckingArguments then
        let (state, optionReturnType) = checkExpression state functionCall.FunctionExpression

        match optionReturnType with
        | Some returnType -> (state, Some returnType)
        | None -> (state, None)
    else
        (state, None)
        
and checkMemberAccess (state: TypeCheckerState) (memberAccess: MemberAccess): TypeCheckerState * Option<PrestoType> =
    let (state, optionLeftType) = checkExpression state memberAccess.LeftExpression

    match optionLeftType with
    | Some leftType ->
        let (state, optionRightType) = checkExpression state memberAccess.RightExpression

        // TODO: improve

        match optionRightType with
        | Some rightType -> (state, Some rightType)
        | None -> (state, None)
    | None -> (state, None)

and checkSymbol (state: TypeCheckerState) (symbol: Symbol): TypeCheckerState * Option<PrestoType> =

    match symbol with
    | BindingSymbol binding -> checkExpression state binding.Value
    | ParameterSymbol parameter -> checkExpression state parameter.TypeExpression
    | RecordFieldSymbol recordField -> checkExpression state recordField.TypeExpression
    | UnionCaseSymbol unionCase -> (state, None)
    | BuiltInSymbol builtInSymbol -> (state, None)
    | UnresolvedSymbol token ->
        let (optionSymbol, state) = resolveSymbol state token

        match optionSymbol with
        | Some symbol ->
            // TODO: replace unresolved symbol with resolved symbol
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
            | SymbolReference symbol -> checkSymbol state symbol
            | NumberLiteralExpression number -> checkNumberLiteral state number

        match optionPrestoType with
        | Some prestoType ->
            (
                { state with TypesByExpressionId = state.TypesByExpressionId.Add(expression.Id, prestoType) },
                optionPrestoType
            )
        | None -> (state, None)

let checkBinding (state: TypeCheckerState) (binding: Binding): TypeCheckerState * Option<PrestoType> =
    checkExpression state binding.Value

let checkTypes (program: Program): Program * List<CompileError> =
    let state = { CurrentScopeId = program.ScopeId; ScopesById = program.ScopesById; TypesByExpressionId = program.TypesByExpressionId; Errors = [] }
    let (state, optionBindingTypes) = checkMany state checkBinding program.Bindings []

    (
        { program with ScopesById = state.ScopesById; TypesByExpressionId = state.TypesByExpressionId },
        state.Errors
    )

// resolve symbols
// find types of expressions