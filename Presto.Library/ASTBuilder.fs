﻿module ASTBuilder

open Parser
open CompilerCore
open Lexer

open type PrestoProgram

(*
Type System
===========

- Nat (unbounded non-negative integer)
- Boolean (should eventually just be an enum)
- Text (any amount of Text, unicode internally)
- Character? (individual character in Text? what about glyph runs?)
- Type (the type of a type)
- Record (a collection of named fields)
- Enum
- Function

Generics
- in "list(int)", "list" is a function that takes a type (int) and returns a type (list of int)


identity = fn (implicit T: type, x: T): T -> x

So, how do we write a function like "first_or_default"?

first_or_default = fn (x: list(T), default: T): T -> ...

"T" is a type variable. One way to view it is as an implicit parameter. We *could* require it be explicit like so:

first_or_default = fn (t: Type, x: list(t), default: t): t -> ...

If we think about more complex functions like so:

clamp = fn (x: Nat, min: Nat, max: Nat, proof: proof(min <= max)): Nat -> ...


TokenType: a Enum type with the possible values: identifier, number_literal, etc.
Token: a Record type with the fields: _type, text, position, was_inserted
List(Token): An expression that evaluates to an internal type (let's call it __list_of_token). "list" is a function that takes a type, and returns a type. If we evaluate this twice with the same args, the types are the same (list(TokenType) == list(TokenType)).
fn (state: TokenizeState): bool -> ...: A function type with one arg of type "TokenizeState" that returns "bool"
*)

type PrestoType =
    | Nothing
    | Boolean
    | Nat
    | Real
    | Character
    | Text
    | TypeParameterType of System.Guid * string (* id, name *)
    | TupleType of List<PrestoType> (* value types *)
    | RecordType of RecordTypeFields
    | RecordInstanceType of RecordTypeFields * List<PrestoType> (* record id, type arguments *)
    | EnumType of EnumTypeFields
    | EnumInstanceType of EnumTypeFields * List<PrestoType> (* enum id, type arguments *)
    | FunctionType of FunctionTypeFields
    | TraitType of TraitTypeFields
    | TraitInstanceType of TraitTypeFields * List<PrestoType> (* id, type arguments *)
    | SelfType of System.Guid * System.Guid (* expression id, trait id *)
    | Type
and RecordTypeFields = {
    ScopeId: System.Guid
    TypeParamNameAndIds: List<System.Guid * string>
    FieldTypes: List<PrestoType>
}
and EnumTypeFields = {
    ScopeId: System.Guid
    TypeParamNameAndIds: List<System.Guid * string>
    Constructors: List<EnumTypeConstructor>
}
and FunctionTypeFields = {
    ScopeId: System.Guid
    TypeParamNameAndIds: List<System.Guid * string>
    ParamTypes: List<PrestoType>
    ReturnType: PrestoType
}
and TraitTypeFields = {
    ScopeId: System.Guid
    TypeParamNameAndIds: List<System.Guid * string>
    FunctionTypesByName: Map<string, PrestoType>
}
and EnumTypeConstructor = {
    Name: string
    Parameters: List<string * PrestoType>
}
and Symbol =
    | BindingSymbol of Binding
    | TypeParameterSymbol of TypeParameter
    | ParameterSymbol of Parameter
    | RecordFieldSymbol of RecordField
    | EnumCaseSymbol of EnumCase
    | BuiltInSymbol of string * PrestoType
and Expression = {
    Id: System.Guid
    Value: ExpressionValue
}
and ExpressionValue =
    | RecordExpression of Record
    | EnumExpression of Enum
    | TraitExpression of Trait
    | FunctionExpression of Function
    | FunctionTypeExpression of FunctionType
    | IfThenElseExpression of IfThenElse
    | BlockExpression of Block
    | FunctionCallExpression of FunctionCall
    | MemberAccessExpression of MemberAccess
    | BinaryOperatorExpression of BinaryOperator
    | GenericInstantiationExpression of GenericInstantiation
    | SymbolReference of Token
    | NumberLiteralExpression of NumberLiteral
    | CharacterLiteralExpression of CharacterLiteral
    | StringLiteralExpression of StringLiteral
    | ParenExpr of ParenthesizedExpression2
    | ErrorPropagationExpression of ErrorPropagationOperatorExpression
    | TupleExpr of TupleExpression
    | SelfTypeExpr of System.Guid (* scope id *)
and Binding = {
    NameToken: Token
    Value: Expression
}
and TypeParameter = {
    Id: System.Guid
    NameToken: Token
}
and Parameter = {
    NameToken: Token
    TypeExpression: Expression
}
and Function = {
    TypeParameters: List<TypeParameter>
    Parameters: List<Parameter>
    OptionReturnTypeExpression: Option<Expression>
    Value: Expression
    ScopeId: System.Guid
}
and FunctionType = {
    TypeParameters: List<TypeParameter>
    Parameters: List<Parameter>
    ReturnTypeExpression: Expression
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
    RightIdentifier: Token;
}
and BinaryOperatorType =
    | Addition
    | Subtraction
    | Multiplication
    | Division
    | Equality
    | ReverseFunctionComposition
and BinaryOperator = {
    Type: BinaryOperatorType
    LeftExpression: Expression;
    RightExpression: Expression;
}
and ParenthesizedExpression2 = {
    InnerExpression: Expression;
}
and TupleExpression = {
    Values: List<Expression>
}
and ErrorPropagationOperatorExpression = {
    InnerExpression: Expression;
}
and RecordField = {
    NameToken: Token
    TypeExpression: Expression
}
and Record = {
    TypeParameters: List<TypeParameter>
    Fields: List<RecordField>
    ScopeId: System.Guid
}
and EnumCase = {
    NameToken: Token
    Parameters: List<Parameter>
}
and Enum = {
    TypeParameters: List<TypeParameter>
    Cases: List<EnumCase>
    ScopeId: System.Guid
}
and Trait = {
    TypeParameters: List<TypeParameter>
    Bindings: List<Binding>
    ScopeId: System.Guid
}
and NumberLiteral = {
    Token: Token
}
and CharacterLiteral = {
    Token: Token
}
and StringLiteral = {
    Token: Token
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
    TypeArgumentsByExpressionId: Map<System.Guid, Map<System.Guid, PrestoType>>
    ResolvedSymbolsByExpressionId: Map<System.Guid, Symbol>
    
    ScopesById: Map<System.Guid, Scope>
    ScopeId: System.Guid

    TypeCanonicalNamesByScopeId: Map<System.Guid, string>
    TraitTypeFieldsByScopeId: Map<System.Guid, TraitTypeFields>
}

type ASTBuilderState = {
    // Program fields
    Bindings: List<Binding>
    TypesByExpressionId: Map<System.Guid, PrestoType>
    ResolvedSymbolsByExpressionId: Map<System.Guid, Symbol>
    ScopesById: Map<System.Guid, Scope>
    ScopeId: System.Guid
    
    TypeCanonicalNamesByScopeId: Map<System.Guid, string>
    TraitTypeFieldsByScopeId: Map<System.Guid, TraitTypeFields>
    
    TypeArgumentsByExpressionId: Map<System.Guid, Map<System.Guid, PrestoType>>
    
    CurrentScopeId: System.Guid
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
    | BindingSymbol binding -> binding.NameToken.position
    | TypeParameterSymbol typeParameter -> typeParameter.NameToken.position
    | ParameterSymbol parameter -> parameter.NameToken.position
    | RecordFieldSymbol recordField -> recordField.NameToken.position
    | EnumCaseSymbol enumCase -> enumCase.NameToken.position
    | BuiltInSymbol (builtInSymbol, prestoType) -> TextPosition(file_path = "", line_index = 0u, column_index = 0u)
    
let getParentScopeId (state: ASTBuilderState): System.Guid =
    state.ScopesById[state.CurrentScopeId].ParentId.Value

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
        let error = CompileError(
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

    let identifier = childOfTokenType node TokenType.identifier
    let nameToken = identifier.Token.Value
    let name = nameToken.text
    
    let typeParameter: TypeParameter = { Id = System.Guid.NewGuid(); NameToken = nameToken }

    let (success, state) = addSymbol state name (TypeParameterSymbol typeParameter)

    if success then
        (Some typeParameter, state)
    else
        (None, state)

let rec buildParameter (state: ASTBuilderState) (node: ParseNode): (Option<Parameter> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Parameter)

    let identifier = childOfTokenType node TokenType.identifier
    let nameToken = identifier.Token.Value
    let name = nameToken.text

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

and buildFunctionType (state: ASTBuilderState) (node: ParseNode): (Option<FunctionType> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.FunctionType)

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

            if expressionNodes.Length <> 1 then
                failwith "Function type parse node doesn't contain 1 expression nodes."
            else
                let (optionReturnTypeExpression, state) = buildExpression state expressionNodes[0]

                match optionReturnTypeExpression with
                | Some returnTypeExpression ->
                    (
                        Some {
                            TypeParameters = typeParameters
                            Parameters = parameters
                            ReturnTypeExpression = returnTypeExpression
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
    
    let identifierNode = childOfTokenType node TokenType.identifier
    let nameToken = identifierNode.Token.Value;
    let name = nameToken.text
    
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

    let typeParameterNodes = childrenOfType node ParseNodeType.TypeParameter
    let (optionTypeParameters, state) = buildAllOrNone state typeParameterNodes buildTypeParameter

    match optionTypeParameters with
    | Some typeParameters ->
        let fieldNodes = childrenOfType node ParseNodeType.RecordField
        let (optionFields, state) = buildAllOrNone state fieldNodes buildRecordField

        match optionFields with
        | Some fields ->
            (
                Some { TypeParameters = typeParameters; Fields = fields; ScopeId = state.CurrentScopeId },
                popScope state
            )
        | None -> (None, popScope state)
    | None -> (None, popScope state)

and buildEnumCase (state: ASTBuilderState) (node: ParseNode): (Option<EnumCase> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.EnumCase)
    
    let identifierNode = childOfTokenType node TokenType.identifier
    let nameToken = identifierNode.Token.Value;
    let name = nameToken.text

    let case = { NameToken = nameToken; Parameters = [] }
    let (success, state) = addSymbol state name (EnumCaseSymbol case)

    if success then (Some case, state)
    else (None, state)

and buildEnum (state: ASTBuilderState) (node: ParseNode): (Option<Enum> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Enum)

    let (scope, state) = pushScope state

    let typeParameterNodes = childrenOfType node ParseNodeType.TypeParameter
    let (optionTypeParameters, state) = buildAllOrNone state typeParameterNodes buildTypeParameter

    match optionTypeParameters with
    | Some typeParameters ->
        let caseNodes = childrenOfType node ParseNodeType.EnumCase
        let (optionCases, state) = buildAllOrNone state caseNodes buildEnumCase

        match optionCases with
        | Some cases ->
            (
                Some { TypeParameters = typeParameters; Cases = cases; ScopeId = state.CurrentScopeId },
                popScope state
            )
        | None -> (None, popScope state)
    | None -> (None, popScope state)

and buildTrait (state: ASTBuilderState) (node: ParseNode): (Option<Trait> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Trait)

    let (scope, state) = pushScope state

    let typeParameterNodes = childrenOfType node ParseNodeType.TypeParameter
    let (optionTypeParameters, state) = buildAllOrNone state typeParameterNodes buildTypeParameter

    match optionTypeParameters with
    | Some typeParameters ->
        let bindingNodes = childrenOfType node ParseNodeType.Binding
        let (optionBindings, state) = buildAllOrNone state bindingNodes buildBinding

        // TODO: check that bindings are all function types

        match optionBindings with
        | Some bindings ->
            (
                Some { TypeParameters = typeParameters; Bindings = bindings; ScopeId = state.CurrentScopeId },
                popScope state
            )
        | None -> (None, popScope state)
    | None -> (None, popScope state)

and buildTypeArgument (state: ASTBuilderState) (node: ParseNode): (Option<Expression> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.TypeArgument)

    let expressionNode = childOfType node ParseNodeType.Expression
    let (optionExpression, state) = buildExpression state expressionNode

    match optionExpression with
    | Some expression -> (Some expression, state)
    | None -> (None, state)

and buildFunctionCallOrPartialFunctionApplication (state: ASTBuilderState) (node: ParseNode): (Option<FunctionCall> * ASTBuilderState) =
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
        let rightIdentifier = childOfTokenType rightExpressionNode TokenType.identifier

        (Some { LeftExpression = leftExpression; RightIdentifier = rightIdentifier.Token.Value; }, state)
    | None -> (None, state)

and buildBinaryOperator (state: ASTBuilderState) (node: ParseNode) (_type: BinaryOperatorType): (Option<BinaryOperator> * ASTBuilderState) =
    let expressionNodes = childrenOfType node ParseNodeType.Expression
    let leftExpressionNode = expressionNodes[0]
    let (optionLeftExpression, state) = buildExpression state leftExpressionNode

    match optionLeftExpression with
    | Some leftExpression ->
        let rightExpressionNode = expressionNodes[1]
        let (optionRightExpression, state) = buildExpression state rightExpressionNode

        match optionRightExpression with
        | Some rightExpression ->
            (Some { Type =_type; LeftExpression = leftExpression; RightExpression = rightExpression; }, state)
        | None -> (None, state)
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
    | ParseNodeType.Enum ->
        let (optionEnum, state) = buildEnum state child
        
        match optionEnum with
        | Some enum -> (Some (newExpression (EnumExpression enum)), state)
        | None -> (None, state)
    | ParseNodeType.Trait ->
        let (optionTrait, state) = buildTrait state child
        
        match optionTrait with
        | Some _trait -> (Some (newExpression (TraitExpression _trait)), state)
        | None -> (None, state)
    | ParseNodeType.Function ->
        let (optionFunction, state) = buildFunction state child

        match optionFunction with
        | Some fn -> (Some (newExpression (FunctionExpression fn)), state)
        | None -> (None, state)
    | ParseNodeType.FunctionType ->
        let (optionFunctionType, state) = buildFunctionType state child

        match optionFunctionType with
        | Some fn -> (Some (newExpression (FunctionTypeExpression fn)), state)
        | None -> (None, state)
    | ParseNodeType.IfThenElse ->
        let (optionIfThenElse, state) = buildIfThenElse state child

        match optionIfThenElse with
        | Some ifThenElse -> (Some (newExpression (IfThenElseExpression ifThenElse)), state)
        | None -> (None, state)
    | ParseNodeType.FunctionCall ->
        let (optionFunctionCall, state) = buildFunctionCallOrPartialFunctionApplication state child

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
    | ParseNodeType.Addition ->
        let (optionOperator, state) = buildBinaryOperator state child BinaryOperatorType.Addition

        match optionOperator with
        | Some operator -> (Some (newExpression (BinaryOperatorExpression operator)), state)
        | None -> (None, state)
    | ParseNodeType.Subtraction ->
        let (optionOperator, state) = buildBinaryOperator state child BinaryOperatorType.Subtraction

        match optionOperator with
        | Some operator -> (Some (newExpression (BinaryOperatorExpression operator)), state)
        | None -> (None, state)
    | ParseNodeType.Multiplication ->
        let (optionOperator, state) = buildBinaryOperator state child BinaryOperatorType.Multiplication

        match optionOperator with
        | Some operator -> (Some (newExpression (BinaryOperatorExpression operator)), state)
        | None -> (None, state)
    | ParseNodeType.Division ->
        let (optionOperator, state) = buildBinaryOperator state child BinaryOperatorType.Division

        match optionOperator with
        | Some operator -> (Some (newExpression (BinaryOperatorExpression operator)), state)
        | None -> (None, state)
    | ParseNodeType.Equals ->
        let (optionOperator, state) = buildBinaryOperator state child BinaryOperatorType.Equality

        match optionOperator with
        | Some operator -> (Some (newExpression (BinaryOperatorExpression operator)), state)
        | None -> (None, state)
    | ParseNodeType.ReverseFunctionComposition ->
        let (optionOperator, state) = buildBinaryOperator state child BinaryOperatorType.ReverseFunctionComposition

        match optionOperator with
        | Some operator -> (Some (newExpression (BinaryOperatorExpression operator)), state)
        | None -> (None, state)
    | ParseNodeType.GenericInstantiation ->
        let (optionGenericInstantiation, state) = buildGenericInstantiation state child

        match optionGenericInstantiation with
        | Some genericInstantiation -> (Some (newExpression (GenericInstantiationExpression genericInstantiation)), state)
        | None -> (None, state)
    | ParseNodeType.ParenthesizedExpression ->
        let expressionNodes = childrenOfType child ParseNodeType.Expression
        let (optionInnerExpr, state) = buildExpression state expressionNodes[0]

        match optionInnerExpr with
        | Some innerExpr ->
            let a: ParenthesizedExpression2 = { InnerExpression = innerExpr } 
            (Some (newExpression (ParenExpr a)), state)
        | None -> (None, state)
    | ParseNodeType.TupleExpression ->
        let expressionNodes = childrenOfType child ParseNodeType.Expression
        
        let (optionExpressions, state) = buildAllOrNone state expressionNodes buildExpression

        match optionExpressions with
        | Some expressions ->
            let a: TupleExpression = { Values = expressions }
            (Some (newExpression (TupleExpr a)), state)
        | None -> (None, state)
    | ParseNodeType.ErrorPropagationOperator ->
        let (optionInnerExpr, state) = buildExpression state child.Children[0]

        match optionInnerExpr with
        | Some innerExpr ->
            let a: ErrorPropagationOperatorExpression = { InnerExpression = innerExpr } 
            (Some (newExpression (ErrorPropagationExpression a)), state)
        | None -> (None, state)
    | ParseNodeType.Expression ->
        buildExpression state child
    | ParseNodeType.Token when child.Token.Value._type = TokenType.identifier ->
        (Some (newExpression (SymbolReference child.Token.Value)), state)
    | ParseNodeType.Token when child.Token.Value._type = TokenType.number_literal ->
        let numberLiteral: NumberLiteral = { Token = child.Token.Value }
        (Some (newExpression (NumberLiteralExpression numberLiteral)), state)
    | ParseNodeType.Token when child.Token.Value._type = TokenType.character_literal ->
        let characterLiteral: CharacterLiteral = { Token = child.Token.Value }
        (Some (newExpression (CharacterLiteralExpression characterLiteral)), state)
    | ParseNodeType.Token when child.Token.Value._type = TokenType.string_literal ->
        let stringLiteral: StringLiteral = { Token = child.Token.Value }
        (Some (newExpression (StringLiteralExpression stringLiteral)), state)
    | ParseNodeType.Token when child.Token.Value._type = TokenType.Self_keyword ->
        (Some (newExpression (SelfTypeExpr (getParentScopeId state))), state)
    | _ ->
        let error = CompileError(
            description = $"Unexpected parse node type: {child.Type}",
            position = nodeTextPosition child
        )
        (None, { state with Errors = List.append state.Errors [error] })

and buildBinding (state: ASTBuilderState) (node: ParseNode): (Option<Binding> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Binding)

    let identifierNode = childOfTokenType node TokenType.identifier
    let expressionNode = childOfType node ParseNodeType.Expression
    let nameToken = identifierNode.Token.Value;
    let name = nameToken.text

    let (optionExpression, state) = buildExpression state expressionNode
    
    match optionExpression with
    | Some expression ->
        let binding = { NameToken = nameToken; Value = expression }
        let (success, state) = addSymbol state name (BindingSymbol binding)

        if success then (Some binding, state)
        else (None, state)
    | None -> (None, state)
    
let resultScopeId = System.Guid.NewGuid()

let getInitialScopesById =
    let scopesById: Map<System.Guid, Scope> = Map.empty

    let scopeId = System.Guid.NewGuid()
    
    let implInCSharpTGuid = System.Guid.NewGuid()
    
    let resultTScopeId = System.Guid.NewGuid()
    let resultEScopeId = System.Guid.NewGuid()

    let scope = {
        SymbolsByName =
            Map.empty
                .Add("Nothing", BuiltInSymbol ("Nothing", PrestoType.Nothing))
                .Add("Nat", BuiltInSymbol ("Nat", PrestoType.Nat))
                .Add("Real", BuiltInSymbol ("Real", PrestoType.Real))
                .Add("Char", BuiltInSymbol ("Char", PrestoType.Character))
                .Add("Text", BuiltInSymbol ("Text", PrestoType.Text))
                
                .Add("Bool", BuiltInSymbol ("Bool", PrestoType.Boolean))
                .Add("true", BuiltInSymbol ("true", PrestoType.Boolean))
                .Add("false", BuiltInSymbol ("false", PrestoType.Boolean))
                
                .Add(
                    "Result",
                    BuiltInSymbol (
                        "Result",
                        EnumType {
                            ScopeId = resultScopeId
                            TypeParamNameAndIds = [(resultTScopeId, "T"); (resultEScopeId, "E")]
                            Constructors = [
                                { Name = "Ok"; Parameters = [("value", TypeParameterType (resultTScopeId, "T"))] }
                                { Name = "Err"; Parameters = [("error", TypeParameterType (resultEScopeId, "E"))] }
                            ]
                        }
                    )
                )

                .Add(
                    "impl_in_CSharp",
                    BuiltInSymbol (
                        "impl_in_CSharp",
                        FunctionType {
                            ScopeId = System.Guid.NewGuid()
                            TypeParamNameAndIds = [(implInCSharpTGuid, "T")]
                            ParamTypes = []
                            ReturnType = TypeParameterType (implInCSharpTGuid, "T")
                        }
                    )
                );
        ParentId = None;
        ChildIds = []
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
            TypeArgumentsByExpressionId = state.TypeArgumentsByExpressionId
            ResolvedSymbolsByExpressionId = state.ResolvedSymbolsByExpressionId
            TypeCanonicalNamesByScopeId = state.TypeCanonicalNamesByScopeId
            TraitTypeFieldsByScopeId = state.TraitTypeFieldsByScopeId
        },
        state
    )

let buildAst (compilationUnitNode: ParseNode) (program: Program): ASTBuilderOutput =
    let state: ASTBuilderState = {
        Bindings = program.Bindings
        TypesByExpressionId = program.TypesByExpressionId
        ResolvedSymbolsByExpressionId = program.ResolvedSymbolsByExpressionId
        ScopesById = program.ScopesById
        ScopeId = program.ScopeId
        TypeCanonicalNamesByScopeId = program.TypeCanonicalNamesByScopeId
        TraitTypeFieldsByScopeId = program.TraitTypeFieldsByScopeId
        TypeArgumentsByExpressionId = program.TypeArgumentsByExpressionId
        
        CurrentScopeId = program.ScopeId
        Errors = []
    }

    let (optionProgram, state) = buildCompilationUnit state compilationUnitNode

    { Program = optionProgram.Value; Errors = state.Errors }