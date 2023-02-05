module ASTBuilder

open Parser
open CompilerCore
open Lexer

type PrestoType =
    | Nat
    | Text
    | Boolean
    | Type
    | RecordType of List<PrestoType>
    | UnionType
    | FunctionType of List<PrestoType> * PrestoType
and Symbol =
    | BindingSymbol of Binding
    | ParameterSymbol of Parameter
    | RecordFieldSymbol of RecordField
    | UnionCaseSymbol of UnionCase
    | BuiltInSymbol of string * PrestoType
    | UnresolvedSymbol of Token
and Expression = {
    Id: System.Guid
    Value: ExpressionValue
}
and ExpressionValue =
    | RecordExpression of Record
    | UnionExpression of Union
    | FunctionExpression of Function
    | FunctionCallExpression of FunctionCall
    | MemberAccessExpression of MemberAccess
    | SymbolReference of Symbol
    | NumberLiteralExpression of NumberLiteral
and Binding = {
    NameToken: Token
    Value: Expression
}
and Parameter = {
    NameToken: Token
    TypeExpression: Expression
}
and Function = {
    Parameters: List<Parameter>
    Value: Expression
    ScopeId: System.Guid
}
and FunctionCall = {
    FunctionExpression: Expression
    Arguments: List<Expression>
}
and MemberAccess = {
    LeftExpression: Expression;
    RightExpression: Expression;
}
and RecordField = {
    NameToken: Token
    TypeExpression: Expression
}
and Record = {
    Fields: List<RecordField>
    ScopeId: System.Guid
}
and UnionCase = {
    NameToken: Token
}
and Union = {
    Cases: List<UnionCase>
    ScopeId: System.Guid
}
and NumberLiteral = {
    Token: Token
}
and Scope = {
    SymbolsByName: Map<string, Symbol>
    ParentId: Option<System.Guid>
    ChildIds: List<System.Guid>
}

type Program = {
    Bindings: List<Binding>
    ScopesById: Map<System.Guid, Scope>
    ScopeId: System.Guid
    TypesByExpressionId: Map<System.Guid, PrestoType>
}

type ASTBuilderState = {
    CurrentScopeId: System.Guid
    ScopesById: Map<System.Guid, Scope>
    Errors: List<CompileError>
}

type ASTBuilderOutput = {
    Program: Program
    Errors: List<CompileError>
}

let newExpression (value: ExpressionValue) =
    { Id = System.Guid.NewGuid(); Value = value }

let getSymbolTextPosition (symbol: Symbol): TextPosition =
    match symbol with
    | BindingSymbol binding -> binding.NameToken.Position
    | ParameterSymbol parameter -> parameter.NameToken.Position
    | RecordFieldSymbol recordField -> recordField.NameToken.Position
    | UnionCaseSymbol unionCase -> unionCase.NameToken.Position
    | BuiltInSymbol (builtInSymbol, prestoType) -> { LineIndex = 0; ColumnIndex = 0; }
    | UnresolvedSymbol token -> token.Position
    
let pushScope (state: ASTBuilderState): (Scope * ASTBuilderState) =
    let parentScopeId = state.CurrentScopeId
    let parentScope = state.ScopesById[parentScopeId]

    let scopeId = System.Guid.NewGuid()
    let scope = { SymbolsByName = Map.empty; ParentId = Some state.CurrentScopeId; ChildIds = [] }

    let scopesById = state.ScopesById.Add (scopeId, scope)
    let scopesById = scopesById.Change (parentScopeId, (fun x -> Some { parentScope with ChildIds = List.append parentScope.ChildIds [scopeId] }))

    (scope, { state with CurrentScopeId = scopeId; ScopesById = scopesById })

let popScope (state: ASTBuilderState): ASTBuilderState =
    let scope = state.ScopesById[state.CurrentScopeId]

    { state with CurrentScopeId = scope.ParentId.Value }

let addSymbol (state: ASTBuilderState) (name: string) (symbol: Symbol): (bool * ASTBuilderState) =
    let scope = state.ScopesById[state.CurrentScopeId]
    if not (scope.SymbolsByName.ContainsKey name) then
        let scope = { scope with SymbolsByName = (scope.SymbolsByName.Add (name,symbol)) }
        (true, { state with ScopesById = state.ScopesById.Change(state.CurrentScopeId, (fun x -> Some scope)) })
    else
        (false, { state with Errors = List.append state.Errors [{ Description = $"Name \"{name}\" is already defined."; Position = getSymbolTextPosition symbol }] })

let rec buildMany
    (state: ASTBuilderState)
    (nodes: List<ParseNode>)
    (buildFn: (ASTBuilderState -> ParseNode -> (Option<'a> * ASTBuilderState)))
    (accumulator: List<Option<'a>>)
    : (List<Option<'a>> * ASTBuilderState) =
        if nodes.IsEmpty then
            (accumulator, state)
        else
            let (optionAstNode, state) = buildFn state nodes.Head
            let accumulator = List.append accumulator [optionAstNode]

            buildMany state nodes.Tail buildFn accumulator

let buildAllOrNone
    (state: ASTBuilderState)
    (nodes: List<ParseNode>)
    (buildFn: (ASTBuilderState -> ParseNode -> (Option<'a> * ASTBuilderState)))
    : (Option<List<'a>> * ASTBuilderState) =
        let (optionAstNodes, state) = buildMany state nodes buildFn []
        let succeededBuildingAll = not (List.exists<Option<'a>> (fun x -> x.IsNone) optionAstNodes)

        if succeededBuildingAll then
            (Some (List.map<Option<'a>, 'a> (fun x -> x.Value) optionAstNodes), state)
        else
            (None, state)

let rec buildParameter (state: ASTBuilderState) (node: ParseNode): (Option<Parameter> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Parameter)

    let identifier = childOfTokenType node TokenType.Identifier
    let nameToken = identifier.Token.Value
    let name = nameToken.Text

    let typeExpressionNode = childOfType node ParseNodeType.Expression
    let (optionTypeExpression, state) = buildExpression state typeExpressionNode

    match optionTypeExpression with
    | Some typeExpression ->
        let parameter: Parameter = { NameToken = nameToken; TypeExpression = typeExpression }

        let (success, state) = addSymbol state name (ParameterSymbol parameter)

        if success then
            (Some parameter, state)
        else
            (None, state)
    | None -> (None, state)

and buildFunction (state: ASTBuilderState) (node: ParseNode): (Option<Function> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Function)

    let (scope, state) = pushScope state

    let parameterNodes = childrenOfType node ParseNodeType.Parameter
    let (optionParameters, state) = buildAllOrNone state parameterNodes buildParameter

    match optionParameters with
    | Some parameters ->
        let expressionNode = childOfType node ParseNodeType.Expression
        let (optionExpression, state) = buildExpression state expressionNode

        match optionExpression with
        | Some expression -> (Some { Parameters = parameters; Value = expression; ScopeId = state.CurrentScopeId }, popScope state)
        | None -> (None, popScope state)
    | None -> (None, popScope state)

and buildRecordField (state: ASTBuilderState) (node: ParseNode): (Option<RecordField> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.RecordField)
    
    let identifierNode = childOfTokenType node TokenType.Identifier
    let nameToken = identifierNode.Token.Value;
    let name = nameToken.Text
    
    let typeExpressionNode = childOfType node ParseNodeType.Expression
    let (optionTypeExpression, state) = buildExpression state typeExpressionNode

    match optionTypeExpression with
    | Some typeExpression ->
        let field = { NameToken = nameToken; TypeExpression = typeExpression }
        let (success, state) = addSymbol state name (RecordFieldSymbol field)

        if success then (Some field, state)
        else (None, state)
    | None -> (None, state)

and buildRecord (state: ASTBuilderState) (node: ParseNode): (Option<Record> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Record)

    let (scope, state) = pushScope state
    
    let fieldNodes = childrenOfType node ParseNodeType.RecordField
    let (optionFields, state) = buildAllOrNone state fieldNodes buildRecordField

    match optionFields with
    | Some fields ->
        (
            Some { Fields = fields; ScopeId = state.CurrentScopeId },
            popScope state
        )
    | None -> (None, popScope state)

and buildUnionCase (state: ASTBuilderState) (node: ParseNode): (Option<UnionCase> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.UnionCase)
    
    let identifierNode = childOfTokenType node TokenType.Identifier
    let nameToken = identifierNode.Token.Value;
    let name = nameToken.Text

    let case = { NameToken = nameToken; }
    let (success, state) = addSymbol state name (UnionCaseSymbol case)

    if success then (Some case, state)
    else (None, state)

and buildUnion (state: ASTBuilderState) (node: ParseNode): (Option<Union> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Union)

    let (scope, state) = pushScope state
    
    let caseNodes = childrenOfType node ParseNodeType.UnionCase
    let (optionCases, state) = buildAllOrNone state caseNodes buildUnionCase

    match optionCases with
    | Some cases ->
        (
            Some { Cases = cases; ScopeId = state.CurrentScopeId },
            popScope state
        )
    | None -> (None, popScope state)

and buildFunctionCall (state: ASTBuilderState) (node: ParseNode): (Option<FunctionCall> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.FunctionCall)
    
    let expressionNodes = childrenOfType node ParseNodeType.Expression
    let functionExpressionNode = expressionNodes.Head

    let (optionFunctionExpression, state) = buildExpression state functionExpressionNode

    match optionFunctionExpression with
    | Some functionExpression ->
        let argumentExpressionNodes = expressionNodes.Tail
        let (optionArgumentExpressions, state) = buildAllOrNone state argumentExpressionNodes buildExpression

        match optionArgumentExpressions with
        | Some argumentExpressions ->
            (
                Some { FunctionExpression = functionExpression; Arguments = argumentExpressions },
                state
            )
        | None -> (None, state)
    | None -> (None, state)

and buildMemberAccess (state: ASTBuilderState) (node: ParseNode): (Option<MemberAccess> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.MemberAccess)

    let expressionNodes = childrenOfType node ParseNodeType.Expression
    let leftExpressionNode = expressionNodes[0]
    let (optionLeftExpression, state) = buildExpression state leftExpressionNode

    match optionLeftExpression with
    | Some leftExpression ->
        
        let rightExpressionNode = expressionNodes[1]
        let (optionRightExpression, state) = buildExpression state rightExpressionNode

        match optionRightExpression with
        | Some rightExpression ->
            (Some { LeftExpression = leftExpression; RightExpression = rightExpression; }, state)
        | None -> (None, state)
    | None -> (None, state)

and buildExpression (state: ASTBuilderState) (node: ParseNode): (Option<Expression> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Expression)
    
    let child = nonTriviaChild node

    match child.Type with
    | ParseNodeType.Record ->
        let (optionRecord, state) = buildRecord state child
        
        match optionRecord with
        | Some record -> (Some (newExpression (RecordExpression record)), state)
        | None -> (None, state)
    | ParseNodeType.Union ->
        let (optionUnion, state) = buildUnion state child
        
        match optionUnion with
        | Some union -> (Some (newExpression (UnionExpression union)), state)
        | None -> (None, state)
    | ParseNodeType.Function ->
        let (optionFunction, state) = buildFunction state child

        match optionFunction with
        | Some fn -> (Some (newExpression (FunctionExpression fn)), state)
        | None -> (None, state)
    | ParseNodeType.FunctionCall ->
        let (optionFunctionCall, state) = buildFunctionCall state child

        match optionFunctionCall with
        | Some functionCall -> (Some (newExpression (FunctionCallExpression functionCall)), state)
        | None -> (None, state)
    | ParseNodeType.MemberAccess ->
        let (optionMemberAccess, state) = buildMemberAccess state child

        match optionMemberAccess with
        | Some memberAccess -> (Some (newExpression (MemberAccessExpression memberAccess)), state)
        | None -> (None, state)
    | ParseNodeType.Expression ->
        buildExpression state child
    | ParseNodeType.Token when child.Token.Value.Type = TokenType.Identifier ->
        (Some (newExpression (SymbolReference (UnresolvedSymbol child.Token.Value))), state)
    | ParseNodeType.Token when child.Token.Value.Type = TokenType.NumberLiteral ->
        let numberLiteral = { Token = child.Token.Value }
        (Some (newExpression (NumberLiteralExpression numberLiteral)), state)
    | _ -> (None, { state with Errors = List.append state.Errors [{ Description = $"Unexpected parse node type: {child.Type}"; Position = nodeTextPosition child }] })

let buildBinding (state: ASTBuilderState) (node: ParseNode): (Option<Binding> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Binding)

    let identifierNode = childOfTokenType node TokenType.Identifier
    let expressionNode = childOfType node ParseNodeType.Expression
    let nameToken = identifierNode.Token.Value;
    let name = nameToken.Text

    let (optionExpression, state) = buildExpression state expressionNode
    
    match optionExpression with
    | Some expression ->
        let binding = { NameToken = nameToken; Value = expression }
        let (success, state) = addSymbol state name (BindingSymbol binding)

        if success then (Some binding, state)
        else (None, state)
    | None -> (None, state)

let getInitialScope =
    let scope = {
        SymbolsByName =
            Map.empty
                .Add("Nat", BuiltInSymbol ("Nat", PrestoType.Nat))
                .Add("Text", BuiltInSymbol ("Text", PrestoType.Text))
                .Add("List", BuiltInSymbol ("List", FunctionType ([PrestoType.Type], PrestoType.Type)))
                .Add("eq", BuiltInSymbol ("eq", FunctionType ([PrestoType.Nat; PrestoType.Nat], PrestoType.Boolean)))
                .Add("not", BuiltInSymbol ("not", FunctionType ([PrestoType.Boolean], PrestoType.Boolean)));
        ParentId = None;
        ChildIds = []
    }

    scope

let buildProgram (node: ParseNode): (Option<Program> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Program)
    
    let scopeId = System.Guid.NewGuid()
    let scope = getInitialScope
    let state = { Errors = []; ScopesById = Map.empty.Add (scopeId, scope); CurrentScopeId = scopeId }

    let bindingParseNodes = childrenOfType node ParseNodeType.Binding
    let (optionBindings, state) = buildMany state bindingParseNodes buildBinding []
    let bindings =
        List.filter<Option<Binding>> (fun x -> x.IsSome) optionBindings
        |> List.map (fun x -> x.Value)

    (
        Some {
            Bindings = bindings
            ScopeId = state.CurrentScopeId
            ScopesById = state.ScopesById
            TypesByExpressionId = Map.empty
        },
        state
    )

let buildAst (programNode: ParseNode): ASTBuilderOutput =
    let (optionProgram, state) = buildProgram programNode

    { Program = optionProgram.Value; Errors = state.Errors }