module ASTBuilder

open Parser
open CompilerCore
open Lexer

open type PrestoProgram

(*
Type System
===========

- Nat (unbounded non-negative integer)
- Boolean (should eventually just be an enum)
- Text (any amount of text, unicode internally)
- Character? (individual character in text? what about glyph runs?)
- Type (the type of a type)
- Record (a collection of named fields)
- Union
- Function

Generics
- in "list(int)", "list" is a function that takes a type (int) and returns a type (list of int)


identity = fn (implicit T: type, x: T): T -> x

So, how do we write a function like "first_or_default"?

first_or_default = fn (x: list(T), default: T): T -> ...

"T" is a type variable. One way to view it is as an implicit parameter. We *could* require it be explicit like so:

first_or_default = fn (t: Type, x: list(t), default: t): t -> ...

If we think about more complex functions like so:

clamp = fn (x: nat, min: nat, max: nat, proof: proof(min <= max)): nat -> ...


token_type: a Union type with the possible values: identifier, number_literal, etc.
token: a Record type with the fields: _type, _text, position, was_inserted
list(token): An expression that evaluates to an internal type (let's call it __list_of_token). "list" is a function that takes a type, and returns a type. If we evaluate this twice with the same args, the types are the same (list(token) == list(token)).
fn (state: tokenize_state): bool -> ...: A function type with one arg of type "tokenize_state" that returns "bool"
*)

type PrestoType =
    | Nothing
    | Boolean
    | Nat
    | Character
    | Text of System.Guid
    | RecordType of System.Guid * List<PrestoType>
    | UnionType of System.Guid
    | FunctionType of System.Guid * List<string> * List<PrestoType> * PrestoType (* scope ID, type param names, param types, return type *)
    | TypeParameterType of string (* name of type parameter *)
    | TypeClassType of System.Guid * List<PrestoType> (* type class id, type arguments *)
    | TypeClassInstanceType of System.Guid * List<PrestoType> (* type class id, type arguments *)
    | Type
and Symbol =
    | BindingSymbol of Binding
    | TypeParameterSymbol of TypeParameter
    | ParameterSymbol of Parameter
    | RecordFieldSymbol of RecordField
    | UnionCaseSymbol of UnionCase
    | BuiltInSymbol of string * PrestoType
and Expression = {
    Id: System.Guid
    Value: ExpressionValue
}
and ExpressionValue =
    | RecordExpression of Record
    | UnionExpression of Union
    | TypeClassExpression of TypeClass
    | FunctionExpression of Function
    | IfThenElseExpression of IfThenElse
    | BlockExpression of Block
    | FunctionCallExpression of FunctionCall
    | MemberAccessExpression of MemberAccess
    | GenericInstantiationExpression of GenericInstantiation
    | SymbolReference of token
    | NumberLiteralExpression of NumberLiteral
    | CharacterLiteralExpression of CharacterLiteral
and Binding = {
    NameToken: token
    Value: Expression
}
and TypeParameter = {
    NameToken: token
}
and Parameter = {
    NameToken: token
    TypeExpression: Expression
}
and Function = {
    TypeParameters: List<TypeParameter>
    Parameters: List<Parameter>
    OptionReturnTypeExpression: Option<Expression>
    Value: Expression
    ScopeId: System.Guid
}
and FunctionCall = {
    FunctionExpression: Expression
    TypeArguments: List<Expression>
    Arguments: List<Expression>
}
and GenericInstantiation = {
    Expression: Expression
    TypeArguments: List<Expression>
}
and MemberAccess = {
    LeftExpression: Expression;
    RightIdentifier: token;
}
and RecordField = {
    NameToken: token
    TypeExpression: Expression
}
and Record = {
    Fields: List<RecordField>
    ScopeId: System.Guid
}
and UnionCase = {
    NameToken: token
}
and Union = {
    Cases: List<UnionCase>
    ScopeId: System.Guid
}
and TypeClass = {
    TypeParameters: List<TypeParameter>
    Functions: List<Function>
    ScopeId: System.Guid
}
and NumberLiteral = {
    Token: token
}
and CharacterLiteral = {
    Token: token
}
and Scope = {
    SymbolsByName: Map<string, Symbol>
    ParentId: Option<System.Guid>
    ChildIds: List<System.Guid>
}
and IfThenElse = {
    IfExpression: Expression
    ThenExpression: Expression
    ElseExpression: Expression
}

and BlockChild =
    | BlockChildBinding of Binding
    | BlockChildExpression of Expression

and Block = {
    Children: List<BlockChild>
}

type Program = {
    Bindings: List<Binding>
    TypesByExpressionId: Map<System.Guid, PrestoType>
    ResolvedSymbolsByExpressionId: Map<System.Guid, Symbol>
    
    ScopesById: Map<System.Guid, Scope>
    ScopeId: System.Guid

    TypeCanonicalNamesByScopeId: Map<System.Guid, string>
}

type ASTBuilderState = {
    // Program fields
    Bindings: List<Binding>
    TypesByExpressionId: Map<System.Guid, PrestoType>
    ResolvedSymbolsByExpressionId: Map<System.Guid, Symbol>
    ScopesById: Map<System.Guid, Scope>
    ScopeId: System.Guid
    TypeCanonicalNamesByScopeId: Map<System.Guid, string>
    
    CurrentScopeId: System.Guid
    Errors: List<compile_error>
}

type ASTBuilderOutput = {
    Program: Program
    Errors: List<compile_error>
}

let newExpression (value: ExpressionValue) =
    { Id = System.Guid.NewGuid(); Value = value }

let getSymbolTextPosition (symbol: Symbol): text_position =
    match symbol with
    | BindingSymbol binding -> binding.NameToken.position
    | TypeParameterSymbol typeParameter -> typeParameter.NameToken.position
    | ParameterSymbol parameter -> parameter.NameToken.position
    | RecordFieldSymbol recordField -> recordField.NameToken.position
    | UnionCaseSymbol unionCase -> unionCase.NameToken.position
    | BuiltInSymbol (builtInSymbol, prestoType) -> text_position(file_name = "", line_index = 0u, column_index = 0u)
    
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
        let error = compile_error(
            description = $"Name \"{name}\" is already defined.",
            position = getSymbolTextPosition symbol
        )

        (false, { state with Errors = List.append state.Errors [error] })

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

let rec buildTypeParameter (state: ASTBuilderState) (node: ParseNode): (Option<TypeParameter> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.TypeParameter)

    let identifier = childOfTokenType node token_type.identifier
    let nameToken = identifier.Token.Value
    let name = nameToken._text
    
    let typeParameter: TypeParameter = { NameToken = nameToken }

    let (success, state) = addSymbol state name (TypeParameterSymbol typeParameter)

    if success then
        (Some typeParameter, state)
    else
        (None, state)

let rec buildParameter (state: ASTBuilderState) (node: ParseNode): (Option<Parameter> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Parameter)

    let identifier = childOfTokenType node token_type.identifier
    let nameToken = identifier.Token.Value
    let name = nameToken._text

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

    let typeParameterNodes = childrenOfType node ParseNodeType.TypeParameter
    let (optionTypeParameters, state) = buildAllOrNone state typeParameterNodes buildTypeParameter

    match optionTypeParameters with
    | Some typeParameters ->
        let parameterNodes = childrenOfType node ParseNodeType.Parameter
        let (optionParameters, state) = buildAllOrNone state parameterNodes buildParameter

        match optionParameters with
        | Some parameters ->
            let expressionNodes = childrenOfType node ParseNodeType.Expression

            // if only one expression, then that is the value, if 2 exprs, then 1st is return type, 2nd is value
            let (optionReturnTypeExpression, optionExpression, state) =
                if expressionNodes.Length = 1 then
                    let (optionExpression, state) = buildExpression state expressionNodes[0]

                    (None, optionExpression, state)
                else if expressionNodes.Length = 2 then
                    let (optionReturnTypeExpression, state) = buildExpression state expressionNodes[0]

                    match optionReturnTypeExpression with
                    | Some returnTypeExpression ->
                        let (optionExpression, state) = buildExpression state expressionNodes[1]

                        (Some returnTypeExpression, optionExpression, state)
                    | None -> (None, None, state)
                else
                    failwith "Function parse node contains more than 2 expression nodes."

            match optionExpression with
            | Some expression ->
                (
                    Some {
                        TypeParameters = typeParameters
                        Parameters = parameters
                        OptionReturnTypeExpression = optionReturnTypeExpression
                        Value = expression
                        ScopeId = state.CurrentScopeId
                    },
                    popScope state
                )
            | None -> (None, popScope state)
        | None -> (None, popScope state)
    | None -> (None, popScope state)

and buildIfThenElse (state: ASTBuilderState) (node: ParseNode): (Option<IfThenElse> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.IfThenElse)

    let expressionNodes = childrenOfType node ParseNodeType.Expression
    let (optionExpressions, state) = buildAllOrNone state expressionNodes buildExpression

    match optionExpressions with
    | Some expressions ->
        (
            Some {
                IfExpression = expressions[0]
                ThenExpression = expressions[1]
                ElseExpression = expressions[2]
            },
            state
        )
    | None -> (None, state)

and buildRecordField (state: ASTBuilderState) (node: ParseNode): (Option<RecordField> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.RecordField)
    
    let identifierNode = childOfTokenType node token_type.identifier
    let nameToken = identifierNode.Token.Value;
    let name = nameToken._text
    
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
    
    let identifierNode = childOfTokenType node token_type.identifier
    let nameToken = identifierNode.Token.Value;
    let name = nameToken._text

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

and buildTypeArgument (state: ASTBuilderState) (node: ParseNode): (Option<Expression> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.TypeArgument)

    let expressionNode = childOfType node ParseNodeType.Expression
    let (optionExpression, state) = buildExpression state expressionNode

    match optionExpression with
    | Some expression -> (Some expression, state)
    | None -> (None, state)

and buildFunctionCall (state: ASTBuilderState) (node: ParseNode): (Option<FunctionCall> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.FunctionCall)
    
    let expressionNodes = childrenOfType node ParseNodeType.Expression
    let functionExpressionNode = expressionNodes.Head

    let (optionFunctionExpression, state) = buildExpression state functionExpressionNode

    match optionFunctionExpression with
    | Some functionExpression ->
        let typeArgumentNodes = childrenOfType node ParseNodeType.TypeArgument
        let (optionTypeArguments, state) = buildAllOrNone state typeArgumentNodes buildTypeArgument

        match optionTypeArguments with
        | Some typeArguments ->
            let argumentExpressionNodes = expressionNodes.Tail
            let (optionArgumentExpressions, state) = buildAllOrNone state argumentExpressionNodes buildExpression

            match optionArgumentExpressions with
            | Some argumentExpressions ->
                (
                    Some {
                        FunctionExpression = functionExpression
                        TypeArguments = typeArguments
                        Arguments = argumentExpressions
                    },
                    state
                )
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

and buildBlockChild (state: ASTBuilderState) (node: ParseNode): (Option<BlockChild> * ASTBuilderState) =
    assert ((node.Type = ParseNodeType.Binding) || (node.Type = ParseNodeType.Expression))

    match node.Type with
    | ParseNodeType.Binding ->
        let (optionBinding, state) = buildBinding state node

        match optionBinding with
        | Some binding -> (Some (BlockChildBinding binding), state)
        | None -> (None, state)
    | ParseNodeType.Expression ->
        let (optionExpression, state) = buildExpression state node

        match optionExpression with
        | Some expression -> (Some (BlockChildExpression expression), state)
        | None -> (None, state)
    | _ -> failwith ""

and buildBlock (state: ASTBuilderState) (node: ParseNode): (Option<Block> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Block)
    
    let childNodes = childrenOfTypes node [ParseNodeType.Binding; ParseNodeType.Expression]
    let (optionChildren, state) = buildAllOrNone state childNodes buildBlockChild

    match optionChildren with
    | Some children ->
        let successResult =
            (
                Some { Children = children },
                state
            )

        if children.IsEmpty then
            successResult
        else
            match (List.last children) with
            | BlockChildExpression expression ->
                successResult
            | BlockChildBinding binding ->
                // TODO: error
                (None, state)
    | None -> (None, state)

and buildMemberAccess (state: ASTBuilderState) (node: ParseNode): (Option<MemberAccess> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.MemberAccess)

    let expressionNodes = childrenOfType node ParseNodeType.Expression
    let leftExpressionNode = expressionNodes[0]
    let (optionLeftExpression, state) = buildExpression state leftExpressionNode

    match optionLeftExpression with
    | Some leftExpression ->
        
        let rightExpressionNode = expressionNodes[1]
        let rightIdentifier = childOfTokenType rightExpressionNode token_type.identifier

        (Some { LeftExpression = leftExpression; RightIdentifier = rightIdentifier.Token.Value; }, state)
    | None -> (None, state)

and buildGenericInstantiation (state: ASTBuilderState) (node: ParseNode): (Option<GenericInstantiation> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.GenericInstantiation)
    
    let expressionNodes = childrenOfType node ParseNodeType.Expression
    let expressionNode = expressionNodes.Head

    let (optionExpression, state) = buildExpression state expressionNode

    match optionExpression with
    | Some expression ->
        let typeArgumentNodes = childrenOfType node ParseNodeType.TypeArgument
        let (optionTypeArguments, state) = buildAllOrNone state typeArgumentNodes buildTypeArgument

        match optionTypeArguments with
        | Some typeArguments ->
            (
                Some {
                    Expression = expression
                    TypeArguments = typeArguments
                },
                state
            )
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
    | ParseNodeType.IfThenElse ->
        let (optionIfThenElse, state) = buildIfThenElse state child

        match optionIfThenElse with
        | Some ifThenElse -> (Some (newExpression (IfThenElseExpression ifThenElse)), state)
        | None -> (None, state)
    | ParseNodeType.FunctionCall ->
        let (optionFunctionCall, state) = buildFunctionCall state child

        match optionFunctionCall with
        | Some functionCall -> (Some (newExpression (FunctionCallExpression functionCall)), state)
        | None -> (None, state)
    | ParseNodeType.Block ->
        let (optionBlock, state) = buildBlock state child

        match optionBlock with
        | Some block -> (Some (newExpression (BlockExpression block)), state)
        | None -> (None, state)
    | ParseNodeType.MemberAccess ->
        let (optionMemberAccess, state) = buildMemberAccess state child

        match optionMemberAccess with
        | Some memberAccess -> (Some (newExpression (MemberAccessExpression memberAccess)), state)
        | None -> (None, state)
    | ParseNodeType.GenericInstantiation ->
        let (optionGenericInstantiation, state) = buildGenericInstantiation state child

        match optionGenericInstantiation with
        | Some genericInstantiation -> (Some (newExpression (GenericInstantiationExpression genericInstantiation)), state)
        | None -> (None, state)
    | ParseNodeType.Expression ->
        buildExpression state child
    | ParseNodeType.Token when child.Token.Value._type = token_type.identifier ->
        (Some (newExpression (SymbolReference child.Token.Value)), state)
    | ParseNodeType.Token when child.Token.Value._type = token_type.number_literal ->
        let numberLiteral: NumberLiteral = { Token = child.Token.Value }
        (Some (newExpression (NumberLiteralExpression numberLiteral)), state)
    | ParseNodeType.Token when child.Token.Value._type = token_type.character_literal ->
        let characterLiteral: CharacterLiteral = { Token = child.Token.Value }
        (Some (newExpression (CharacterLiteralExpression characterLiteral)), state)
    | _ ->
        let error = compile_error(
            description = $"Unexpected parse node type: {child.Type}",
            position = nodeTextPosition child
        )
        (None, { state with Errors = List.append state.Errors [error] })

and buildBinding (state: ASTBuilderState) (node: ParseNode): (Option<Binding> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Binding)

    let identifierNode = childOfTokenType node token_type.identifier
    let expressionNode = childOfType node ParseNodeType.Expression
    let nameToken = identifierNode.Token.Value;
    let name = nameToken._text

    let (optionExpression, state) = buildExpression state expressionNode
    
    match optionExpression with
    | Some expression ->
        let binding = { NameToken = nameToken; Value = expression }
        let (success, state) = addSymbol state name (BindingSymbol binding)

        if success then (Some binding, state)
        else (None, state)
    | None -> (None, state)

let getInitialScopesById =
    let scopesById: Map<System.Guid, Scope> = Map.empty

    let scopeId = System.Guid.NewGuid()

    let textScopeId = System.Guid.NewGuid()
    let textScope = {
        SymbolsByName =
            Map.empty;
        ParentId = Some scopeId;
        ChildIds = []
    }
    let scopesById = scopesById.Add(textScopeId, textScope)

    let seqScopeId = System.Guid.NewGuid()

    let scope = {
        SymbolsByName =
            Map.empty
                .Add("nothing", BuiltInSymbol ("nothing", PrestoType.Nothing))
                .Add("bool", BuiltInSymbol ("bool", PrestoType.Boolean))
                .Add("true", BuiltInSymbol ("true", PrestoType.Boolean))
                .Add("false", BuiltInSymbol ("false", PrestoType.Boolean))
                .Add("nat", BuiltInSymbol ("nat", PrestoType.Nat))
                .Add("char", BuiltInSymbol ("char", PrestoType.Character))
                .Add("text", BuiltInSymbol ("text", PrestoType.Text textScopeId))

                .Add(
                    "seq",
                    BuiltInSymbol (
                        "seq",
                        TypeClassType (seqScopeId, [PrestoType.TypeParameterType "t"])
                    )
                )

                .Add(
                    "eq",
                    BuiltInSymbol (
                        "eq",
                        FunctionType (
                            System.Guid.NewGuid(),
                            ["t"],
                            [TypeParameterType "t"; TypeParameterType "t"],
                            PrestoType.Boolean
                        )
                    )
                )
                .Add(
                    "not_eq",
                    BuiltInSymbol (
                        "not_eq",
                        FunctionType (
                            System.Guid.NewGuid(),
                            ["t"],
                            [TypeParameterType "t"; TypeParameterType "t"],
                            PrestoType.Boolean
                        )
                    )
                )

                .Add("not", BuiltInSymbol ("not", FunctionType (System.Guid.NewGuid(), [], [PrestoType.Boolean], PrestoType.Boolean)))

                .Add("sum", BuiltInSymbol ("sum", FunctionType (System.Guid.NewGuid(), [], [PrestoType.Nat; PrestoType.Nat], PrestoType.Nat)))
                .Add("length", BuiltInSymbol ("length", FunctionType (System.Guid.NewGuid(), [], [PrestoType.Text textScopeId], PrestoType.Nat)))
                .Add("difference", BuiltInSymbol ("difference", FunctionType (System.Guid.NewGuid(), [], [PrestoType.Nat; PrestoType.Nat], PrestoType.Nat)))
                .Add("product", BuiltInSymbol ("product", FunctionType (System.Guid.NewGuid(), [], [PrestoType.Nat; PrestoType.Nat], PrestoType.Nat)))
                .Add("quotient", BuiltInSymbol ("quotient", FunctionType (System.Guid.NewGuid(), [], [PrestoType.Nat; PrestoType.Nat], PrestoType.Nat)))

                .Add(
                    "last_or_default",
                    BuiltInSymbol (
                        "last_or_default",
                        FunctionType (
                            System.Guid.NewGuid(),
                            ["t"],
                            [
                                TypeClassInstanceType (seqScopeId, [TypeParameterType "t"])
                                TypeParameterType "t"
                            ],
                            (PrestoType.TypeParameterType "t")
                        )
                    )
                );
        ParentId = None;
        ChildIds = [textScopeId]
    }
    let scopesById = scopesById.Add(scopeId, scope)

    (scopeId, scopesById)

let buildCompilationUnit (state: ASTBuilderState) (node: ParseNode): (Option<Program> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.CompilationUnit)
    
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
            TypesByExpressionId = state.TypesByExpressionId
            ResolvedSymbolsByExpressionId = state.ResolvedSymbolsByExpressionId
            TypeCanonicalNamesByScopeId = state.TypeCanonicalNamesByScopeId
        },
        state
    )

let buildAst (compilationUnitNode: ParseNode) (program: Program): ASTBuilderOutput =
    let state = {
        Bindings = program.Bindings
        TypesByExpressionId = program.TypesByExpressionId
        ResolvedSymbolsByExpressionId = program.ResolvedSymbolsByExpressionId
        ScopesById = program.ScopesById
        ScopeId = program.ScopeId
        TypeCanonicalNamesByScopeId = program.TypeCanonicalNamesByScopeId
        
        CurrentScopeId = program.ScopeId
        Errors = []
    }

    let (optionProgram, state) = buildCompilationUnit state compilationUnitNode

    { Program = optionProgram.Value; Errors = state.Errors }