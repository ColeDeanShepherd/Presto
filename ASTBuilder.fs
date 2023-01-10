module ASTBuilder

open Parser
open CompilerCore
open Lexer

type Parameter = {
    Name: string
}

type Expression =
    | FunctionExpression of Function
    | BindingReference of Binding
and Binding = {
    Name: string
    Value: Expression
}
and Function = {
    Parameters: List<Parameter>
    Value: Expression
}

type Program = {
    Bindings: List<Binding>
}

type ASTBuilderState = {
    Errors: List<CompileError>
}

type ASTBuilderOutput = {
    Program: Program
    Errors: List<CompileError>
}

let rec buildMany
    (state: ASTBuilderState)
    (nodes: List<ParseNode>)
    (buildFn: (ASTBuilderState -> ParseNode -> (Option<'a> * ASTBuilderState)))
    (accumulator: List<'a>)
    : (List<'a> * ASTBuilderState) =
        if nodes.IsEmpty then
            (accumulator, state)
        else
            let (optionAstNode, state) = buildFn state nodes.Head
            let accumulator =
                match optionAstNode with
                | Some astNode -> List.append accumulator [astNode]
                | None -> accumulator

            buildMany state nodes.Tail buildFn accumulator
            
let resolveBindingReference (state: ASTBuilderState) (node: ParseNode): (Option<Binding> * ASTBuilderState) =
    assert ((node.Type = ParseNodeType.Token) && (node.Token.Value.Type = TokenType.Identifier))


let buildParameter (state: ASTBuilderState) (node: ParseNode): (Option<Parameter> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Parameter)

let buildFunction (state: ASTBuilderState) (node: ParseNode): (Option<Function> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Function)

    let parameterNodes = childrenOfType node ParseNodeType.Function
    let (parameters, state) = buildMany state parameterNodes buildParameter []

    // continue compiling if parameter fails?

    let expressionNode = childOfType node ParseNodeType.Expression
    let (optionExpression, state) = buildExpression state expressionNode


and buildExpression (state: ASTBuilderState) (node: ParseNode): (Option<Expression> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Expression)
    
    let child = nonTriviaChild node

    match child.Type with
    | ParseNodeType.Function ->
        let (optionFunction, state) = buildFunction state child

        match optionFunction with
        | Some fn -> (Some (FunctionExpression fn), state)
        | None -> (None, state)
    | ParseNodeType.Token when child.Token.Value.Type = TokenType.Identifier ->
        let (optionBinding, state) = resolveBindingReference state child
        
        match optionBinding with
        | Some binding -> (Some (BindingReference binding), state)
        | None -> (None, state)
    | _ -> (None, { state with Errors = List.append state.Errors [{ Description = $"Unexpected parse node type: {child.Type}"; Position = nodeTextPosition child }] })

// TODO: handle duplicate bindings
let buildBinding (state: ASTBuilderState) (node: ParseNode): (Option<Binding> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Binding)

    let identifierNode = childOfTokenType node TokenType.Identifier
    let expressionNode = childOfType node ParseNodeType.Expression
    
    ({ Name = identifierNode.Token.Value; Value =  }, state)

let buildProgram (state: ASTBuilderState) (node: ParseNode): (Option<Program> * ASTBuilderState) =
    assert (node.Type = ParseNodeType.Program)
    
    let bindingParseNodes = childrenOfType node ParseNodeType.Binding
    let (bindings, state) = buildMany<Binding> state bindingParseNodes buildBinding []

    ({ Bindings = bindings }, state)

let buildAst (programNode: ParseNode): ASTBuilderOutput =
    let state = { Errors = [] }

    let (optionProgram, state) = buildProgram state programNode

    { Program = optionProgram.Value; Errors = state.Errors }