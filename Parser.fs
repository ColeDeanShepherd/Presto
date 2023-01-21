module Parser

open CompilerCore
open Lexer

(*
Program -> Binding*
Binding -> Identifier "=" Expression
Expression ->
      Function
    | Record
    | Union
    | Identifier
TypeExpression -> Expression
Function -> "fn" "(" SepBy(Parameter, ",") ")" "->" Expression
Record -> "record" SepBy(Whitespace, Field)
Union -> "union" SepBy(Whitespace, UnionCase)
Field -> Identifier ":" TypeExpression
Parameter -> Identifier (":" Expression)?
*)

type ParseNodeType =
    | Program
    | Binding
    | Expression
    | Record
    | RecordField
    | Union
    | UnionItem
    | Function
    | Parameter
    | Token
    | Whitespace

type ParseNode = {
    Type: ParseNodeType
    Children: List<ParseNode>
    Token: Option<Token>
}

type ParseOutput = {
    Program: ParseNode
    Errors: List<CompileError>
}

type ParseState = {
    Tokens: List<Token>
    NextTokenIndex: int
    Errors: List<CompileError>
}

let rec nodeTextPosition (node: ParseNode): TextPosition =
    match node.Token with
    | Some token -> token.Position
    | None -> nodeTextPosition node.Children.Head

let nonTriviaChild (node: ParseNode): ParseNode =
    List.find (fun c -> (c.Type <> ParseNodeType.Whitespace)) node.Children

let childOfTokenType (node: ParseNode) (tokenType: TokenType): ParseNode =
    List.find (fun c -> (c.Type = ParseNodeType.Token) && (c.Token.Value.Type = tokenType)) node.Children

let childOfType (node: ParseNode) (nodeType: ParseNodeType): ParseNode =
    List.find (fun c -> c.Type = nodeType) node.Children

let childrenOfType (node: ParseNode) (nodeType: ParseNodeType): List<ParseNode> =
    List.filter (fun c -> c.Type = nodeType) node.Children

let isDone (state: ParseState): bool =
    state.NextTokenIndex >= state.Tokens.Length

let tryPeekToken (state: ParseState): Option<Token> =
    if state.NextTokenIndex < state.Tokens.Length then
        Some state.Tokens[state.NextTokenIndex]
    else
        None

let tryPeekExpectedToken (state: ParseState) (tokenType: TokenType): Option<Token> =
    if (state.NextTokenIndex < state.Tokens.Length) && (state.Tokens[state.NextTokenIndex].Type = tokenType) then
        Some state.Tokens[state.NextTokenIndex]
    else
        None

let currentTextPosition (state: ParseState): TextPosition =
    if state.NextTokenIndex < state.Tokens.Length then
        state.Tokens[state.NextTokenIndex].Position
    else
        { LineIndex = 0; ColumnIndex = 0; }

let peekToken (state: ParseState): Option<Token> * ParseState =
    let optionNextToken = tryPeekToken state

    match optionNextToken with
    | Some nextToken ->
        (optionNextToken, state)
    | None ->
        let error = {
            Description = "Unexpectedly reached the end of the tokens."
            Position = currentTextPosition state
        }

        (
            optionNextToken,
            { state with Errors = List.append state.Errors [error] }
        )

let readToken (state: ParseState): Option<Token> * ParseState =
    let (optionNextToken, state) = peekToken state

    match optionNextToken with
    | Some nextToken -> (optionNextToken, { state with NextTokenIndex = state.NextTokenIndex + 1 })
    | None -> (optionNextToken, state)

let readExpectedToken (state: ParseState) (expectedTokenType: TokenType): Option<Token> * ParseState =
    let (optionNextToken, state) = peekToken state

    match optionNextToken with
    | Some nextToken ->
        if nextToken.Type = expectedTokenType then
            (optionNextToken, { state with NextTokenIndex = state.NextTokenIndex + 1 })
        else
            let virtualErrorDescriptionPart =
                if nextToken.WasInserted then
                    "lexer-inserted "
                else
                    ""

            let error = {
                Description = $"Encountered unexpected {virtualErrorDescriptionPart}token: {nextToken.Text}"
                Position = currentTextPosition state
            }

            (None, { state with Errors = state.Errors @ [error] })
    | None -> (optionNextToken, state)

let parseToken (state: ParseState) (tokenType: TokenType): Option<ParseNode> * ParseState =
    let (optionNextToken, state) = readExpectedToken state tokenType

    match optionNextToken with
    | Some nextToken -> (Some { Type = ParseNodeType.Token; Children = []; Token = Some nextToken }, state)
    | None -> (None, state)

let parseWhitespace (state: ParseState): List<ParseNode> * ParseState = 
    let whitespace =
           List.skip state.NextTokenIndex state.Tokens
        |> List.takeWhile (fun t -> t.Type = TokenType.Whitespace)
        |> List.map (fun t -> { Type = ParseNodeType.Whitespace; Children = []; Token = Some t })

    (
        whitespace,
        {
            Tokens = state.Tokens
            NextTokenIndex = state.NextTokenIndex + whitespace.Length
            Errors = state.Errors
        }
    )

let parseWhileTokensLeft
    (state: ParseState)
    (parseFn: (ParseState -> Option<ParseNode> * ParseState))
    (accumulator: List<ParseNode>):
    List<ParseNode> * ParseState =
        let maybeNextToken = tryPeekToken state

        match maybeNextToken with
        | Some nextToken ->
            let (maybeNode, state) = parseFn state
            match maybeNode with
            | Some node -> (List.append accumulator [node], state)
            | None -> (accumulator, state)
        | None -> (accumulator, state)

let rec parseSeparatedByToken
    (state: ParseState)
    (parseFn: (ParseState -> Option<ParseNode> * ParseState))
    (tokenType: TokenType)
    (accumulator: List<ParseNode>):
    List<ParseNode> * ParseState =
        let optionNextToken = tryPeekToken state

        match optionNextToken with
        | Some nextToken ->
            let (maybeNode, state) = parseFn state

            match maybeNode with
            | Some node ->
                let accumulator = List.append accumulator [node]
                let optionNextToken = tryPeekExpectedToken state tokenType

                match optionNextToken with
                | Some nextToken ->
                    let (optionSeparator, state) = parseToken state tokenType

                    match optionSeparator with
                    | Some separator ->
                        let accumulator = List.append accumulator [separator]
                        
                        parseSeparatedByToken state parseFn tokenType accumulator
                    | None -> (accumulator, state)
                | None -> (accumulator, state)
            | None -> (accumulator, state)
        | None -> (accumulator, state)

let rec parseSeparatedByWhitespace
    (state: ParseState)
    (parseFn: (ParseState -> Option<ParseNode> * ParseState))
    (optionTerminatorTokenType: Option<TokenType>)
    (accumulator: List<ParseNode>):
    List<ParseNode> * ParseState =
        let optionNextToken = tryPeekToken state

        match optionNextToken with
        | Some nextToken ->
            if optionTerminatorTokenType.IsNone || (nextToken.Type <> optionTerminatorTokenType.Value) then
                let (maybeNode, state) = parseFn state

                match maybeNode with
                | Some node ->
                    let (whitespace, state) = parseWhitespace state
                    let newNodes = List.append [node] whitespace
                    let accumulator = List.append accumulator newNodes

                    if (whitespace.Length > 0) && not (isDone state) then
                        parseSeparatedByWhitespace state parseFn optionTerminatorTokenType accumulator
                    else
                        (accumulator, state)
                | None -> (accumulator, state)
            else
                (accumulator, state)
        | None -> (accumulator, state)

let rec parseOptionalWhitespaceSeparatedUntilDone
    (state: ParseState)
    (parseFn: (ParseState -> Option<ParseNode> * ParseState))
    (accumulator: List<ParseNode>):
    List<ParseNode> * ParseState =
        let optionNextToken = tryPeekToken state

        match optionNextToken with
        | Some nextToken ->
            let (maybeNode, state) = parseFn state

            match maybeNode with
            | Some node ->
                let (whitespace, state) = parseWhitespace state
                let newNodes = List.append [node] whitespace
                let accumulator = List.append accumulator newNodes

                if not (isDone state) then
                    parseOptionalWhitespaceSeparatedUntilDone state parseFn accumulator
                else
                    (accumulator, state)
            | None -> (accumulator, state)
        | None -> (accumulator, state)

let parseParameter (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.Identifier

    match optionIdentifier with
    | Some identifier ->
        (
            Some {
                Type = ParseNodeType.Parameter
                Children = List.concat [ [identifier] ]
                Token = None
            },
            state
        )
        //let optionNextToken = tryPeekExpectedToken state TokenType.Colon

        //match optionNextToken with
        //| Some nextToken = 
        //| None -> (None, state)
    | None -> (None, state)

let rec parseRecordField (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.Identifier

    match optionIdentifier with
    | Some identifier ->
        let (whitespace1, state) = parseWhitespace state
        let (optionColon, state) = parseToken state TokenType.Colon

        match optionColon with
        | Some colon ->
            let (whitespace2, state) = parseWhitespace state
            let (optionTypeExpression, state) = parseExpression state

            match optionTypeExpression with
            | Some typeExpression ->
                (
                    Some {
                        Type = ParseNodeType.RecordField
                        Children = List.concat [ [identifier]; whitespace1; [colon]; whitespace2; [typeExpression] ]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

and parseRecord (state: ParseState): Option<ParseNode> * ParseState =
    let (optionRecordToken, state) = parseToken state TokenType.RecordKeyword

    match optionRecordToken with
    | Some recordToken ->
        let (whitespace1, state) = parseWhitespace state
        let (optionLeftCurlyBracket, state) = parseToken state TokenType.LeftCurlyBracket

        match optionLeftCurlyBracket with
        | Some leftCurlyBracket -> 
            let (whitespace2, state) = parseWhitespace state
            let (fields, state) = parseSeparatedByWhitespace state parseRecordField (Some TokenType.RightCurlyBracket) []
            let (whitespace3, state) = parseWhitespace state
            let (optionRightCurlyBracket, state) = parseToken state TokenType.RightCurlyBracket

            match optionRightCurlyBracket with
            | Some rightCurlyBracket ->
                (
                    Some {
                        Type = ParseNodeType.Record
                        Children = List.concat [ [recordToken]; whitespace1; [leftCurlyBracket]; whitespace2; fields; whitespace3; [rightCurlyBracket] ]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

and parseUnionCase (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.Identifier

    match optionIdentifier with
    | Some identifier ->
        (
            Some {
                Type = ParseNodeType.UnionItem
                Children = List.concat [ [identifier] ]
                Token = None
            },
            state
        )
    | None -> (None, state)

and parseUnion (state: ParseState): Option<ParseNode> * ParseState =
    let (optionUnionToken, state) = parseToken state TokenType.UnionKeyword

    match optionUnionToken with
    | Some unionToken ->
        let (whitespace1, state) = parseWhitespace state
        let (optionLeftCurlyBracket, state) = parseToken state TokenType.LeftCurlyBracket

        match optionLeftCurlyBracket with
        | Some leftCurlyBracket -> 
            let (whitespace2, state) = parseWhitespace state
            let (cases, state) = parseSeparatedByWhitespace state parseUnionCase (Some TokenType.RightCurlyBracket) []
            let (whitespace3, state) = parseWhitespace state
            let (optionRightCurlyBracket, state) = parseToken state TokenType.RightCurlyBracket

            match optionRightCurlyBracket with
            | Some rightCurlyBracket ->
                (
                    Some {
                        Type = ParseNodeType.Union
                        Children = List.concat [ [unionToken]; whitespace1; [leftCurlyBracket]; whitespace2; cases; whitespace3; [rightCurlyBracket] ]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

and parseFunction (state: ParseState): Option<ParseNode> * ParseState =
    let (optionFnToken, state) = parseToken state TokenType.FnKeyword

    match optionFnToken with
    | Some fnToken ->
        let (whitespace1, state) = parseWhitespace state
        let (optionLeftParen, state) = parseToken state TokenType.LeftParen

        match optionLeftParen with
        | Some leftParen ->
            let (whitespace2, state) = parseWhitespace state
            let (parameters, state) = parseSeparatedByToken state parseParameter TokenType.Comma []
            let (whitespace3, state) = parseWhitespace state
            let (optionRightParen, state) = parseToken state TokenType.RightParen

            match optionRightParen with
            | Some rightParen ->
                let (whitespace4, state) = parseWhitespace state
                let (optionMinus, state) = parseToken state TokenType.Minus

                match optionMinus with
                | Some minus ->
                    let (optionGreaterThan, state) = parseToken state TokenType.GreaterThan

                    match optionGreaterThan with
                    | Some greaterThan ->
                        let (whitespace5, state) = parseWhitespace state
                        let (optionResult, state) = parseExpression state

                        match optionResult with
                        | Some result ->
                            (
                                Some {
                                    Type = ParseNodeType.Function
                                    Children = List.concat [ [fnToken]; whitespace1; [leftParen]; whitespace2; parameters; whitespace3; [rightParen]; whitespace4; [minus]; [greaterThan]; whitespace5; [result] ]
                                    Token = None
                                },
                                state
                            )
                        | None -> (None, state)
                    | None -> (None, state)
                | None -> (None, state)
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

and parseExpression (state: ParseState): Option<ParseNode> * ParseState =
    let (optionNextToken, state) = peekToken state
    
    match optionNextToken with
    | Some nextToken ->
        match nextToken.Type with
        | TokenType.FnKeyword ->
            let (optionValue, state) = parseFunction state

            match optionValue with
            | Some value ->
                (
                    Some {
                        Type = ParseNodeType.Expression
                        Children = [value]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | TokenType.RecordKeyword ->
            let (optionValue, state) = parseRecord state

            match optionValue with
            | Some value ->
                (
                    Some {
                        Type = ParseNodeType.Expression
                        Children = [value]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | TokenType.UnionKeyword ->
            let (optionValue, state) = parseUnion state

            match optionValue with
            | Some value ->
                (
                    Some {
                        Type = ParseNodeType.Expression
                        Children = [value]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | TokenType.Identifier ->
            let (optionValue, state) = parseToken state TokenType.Identifier

            match optionValue with
            | Some value ->
                (
                    Some {
                        Type = ParseNodeType.Expression
                        Children = [value]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | _ ->
            let error = {
                Description = $"Encountered unexpected token: {nextToken.Text}"
                Position = currentTextPosition state
            }

            (None, { state with Errors = List.append state.Errors [error] })
    | None -> (None, state)

let parseBinding (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.Identifier

    match optionIdentifier with
    | Some identifier ->
        let (whitespace1, state) = parseWhitespace state
        let (optionEqualsSign, state) = parseToken state TokenType.Equals
        
        match optionEqualsSign with
        | Some equalsSign ->
            let (whitespace2, state) = parseWhitespace state
            let (optionExpression, state) = parseExpression state

            match optionExpression with
            | Some expression ->
                (
                    Some {
                        Type = ParseNodeType.Binding
                        Children = List.concat [ [identifier]; whitespace1; [equalsSign]; whitespace2; [expression] ]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

let parseProgram (state: ParseState): ParseNode * ParseState =
    let (initialWhitespace, state) = parseWhitespace state
    let (children, state) = parseOptionalWhitespaceSeparatedUntilDone state parseBinding []
    let program: ParseNode = {
        Type = ParseNodeType.Program
        Children = List.append initialWhitespace children
        Token = None
    }

    (program, state)

let parse (tokens: List<Token>): ParseOutput =
    let seedState: ParseState = {
        Tokens = tokens
        NextTokenIndex = 0
        Errors = []
    }
    let (program, finalState) = parseProgram seedState

    { Program = program; Errors = finalState.Errors }
