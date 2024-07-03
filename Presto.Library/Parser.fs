module Parser

open CompilerCore
open Lexer

open type PrestoProgram

(*
CompilationUnit -> Binding*
Binding -> Identifier "=" Expression
Expression ->
      Function
    | Record
    | Enum
    | Identifier
    | IfThenElse
TypeExpression -> Expression
Function -> "fn" "(" SepBy(Parameter, ",") ")" "->" Expression
Record -> "record" SepBy(Whitespace, Field)
Enum -> "enum" SepBy(Whitespace, EnumCase)
Field -> Identifier ":" TypeExpression
Parameter -> Identifier (":" Expression)?
IfThenElse -> "if" Expression "then" Expression "else" Expression
*)

type ParseNodeType =
    | CompilationUnit
    | Binding
    | Expression
    | Record
    | RecordField
    | Enum
    | EnumCase
    | Trait
    | Function
    | FunctionType
    | TypeParameter
    | TypeArgument
    | Parameter
    | FunctionCall
    | MemberAccess
    | IfThenElse
    | Block
    | GenericInstantiation
    | Token
    | Whitespace
    | Comment
    | Addition
    | Subtraction
    | Multiplication
    | Division
    | Equals
    | ParenthesizedExpression
    | ErrorPropagationOperator
    | TupleExpression
    | ReverseFunctionComposition

type ParseNode = {
    Type: ParseNodeType
    Children: List<ParseNode>
    Token: Option<Token>
}

type ParseOutput = {
    CompilationUnit: ParseNode
    Errors: List<CompileError>
}

type ParseState = {
    Tokens: List<Token>
    NextTokenIndex: int
    Errors: List<CompileError>
}

let rec nodeTextPosition (node: ParseNode): TextPosition =
    match node.Token with
    | Some token -> token.position
    | None -> nodeTextPosition node.Children.Head

let isTriviaParseNode (parseNodeType: ParseNodeType): bool =
    (parseNodeType = ParseNodeType.Whitespace) || (parseNodeType = ParseNodeType.Comment)

let nonTriviaChild (node: ParseNode): ParseNode =
    List.find (fun c -> not (isTriviaParseNode c.Type)) node.Children

let childOfTokenType (node: ParseNode) (tokenType: TokenType): ParseNode =
    List.find (fun c -> (c.Type = ParseNodeType.Token) && (c.Token.Value._type = tokenType)) node.Children

let childOfType (node: ParseNode) (nodeType: ParseNodeType): ParseNode =
    List.find (fun c -> c.Type = nodeType) node.Children

let childrenOfType (node: ParseNode) (nodeType: ParseNodeType): List<ParseNode> =
    List.filter (fun c -> c.Type = nodeType) node.Children

let childrenOfTypes (node: ParseNode) (nodeTypes: List<ParseNodeType>): List<ParseNode> =
    List.filter (fun c -> List.contains c.Type nodeTypes) node.Children

let isDone (state: ParseState): bool =
    state.NextTokenIndex >= state.Tokens.Length

let tryPeekToken (state: ParseState): Option<Token> =
    if state.NextTokenIndex < state.Tokens.Length then
        Some state.Tokens[state.NextTokenIndex]
    else
        None

let tryPeekTokenIndexAfterTrivia (state: ParseState): Option<int> =
       Seq.indexed state.Tokens
    |> Seq.skip state.NextTokenIndex
    |> Seq.tryFind (fun (i, t) -> not (isTriviaToken t._type))
    |> Option.bind (fun (i, t) -> Some i)

let tryPeekTokenAfterTrivia (state: ParseState): Option<Token> =
       Seq.skip state.NextTokenIndex state.Tokens
    |> Seq.tryFind (fun t -> not (isTriviaToken t._type))
    
let currentTextPosition (state: ParseState): TextPosition =
    if state.NextTokenIndex < state.Tokens.Length then
        state.Tokens[state.NextTokenIndex].position
    else
        TextPosition(file_path = "", line_index = 0u, column_index = 0u)

let peekTokenAfterTrivia (state: ParseState): Option<Token> * ParseState =
    let optionNextToken = tryPeekTokenAfterTrivia state

    match optionNextToken with
    | Some _ ->
        (optionNextToken, state)
    | None ->
        let error = CompileError(
            description = "Unexpectedly reached the end of the tokens.", position = currentTextPosition state
        )

        (
            optionNextToken,
            { state with Errors = state.Errors @ [error] }
        )

let rec tryPeekTokenSequenceIgnoreTrivia (state: ParseState) (tokenSequence: List<TokenType>): bool =
    if tokenSequence.IsEmpty then
        true
    else
        let optionTokenIndex = tryPeekTokenIndexAfterTrivia state

        match optionTokenIndex with
        | Some tokenIndex ->
            let TokenType = state.Tokens[tokenIndex]

            if TokenType._type = tokenSequence.Head then
                let state = { state with NextTokenIndex = tokenIndex + 1 }

                tryPeekTokenSequenceIgnoreTrivia state tokenSequence.Tail
            else
                false
        | None -> false

let getUnexpectedTokenError (state: ParseState) (expectedTokenType: TokenType) (nextToken: Token): CompileError =
    let virtualErrorDescriptionPart =
        if nextToken.was_inserted then
            "lexer-inserted "
        else
            ""

    CompileError(
        description = $"Expected {expectedTokenType} but encountered unexpected {virtualErrorDescriptionPart} token: \"{nextToken.text}\"",
        position = currentTextPosition state
    )

let tryPeekExpectedTokenAfterTrivia (state: ParseState) (expectedTokenType: TokenType): Option<Token> * ParseState =
    let (optionNextToken, state) = peekTokenAfterTrivia state

    match optionNextToken with
    | Some nextToken ->
        if nextToken._type = expectedTokenType then
            (Some nextToken, state)
        else
            (None, state)
    | None -> (None, state)

let peekExpectedTokenAfterTrivia (state: ParseState) (expectedTokenType: TokenType): Option<Token> * ParseState =
    let (optionNextToken, state) = peekTokenAfterTrivia state

    match optionNextToken with
    | Some nextToken ->
        if nextToken._type = expectedTokenType then
            (Some nextToken, state)
        else
            let error = getUnexpectedTokenError state expectedTokenType nextToken

            (None, { state with Errors = state.Errors @ [error] })
    | None -> (None, state)

let tryPeekExpectedToken (state: ParseState) (tokenType: TokenType): Option<Token> =
    if (state.NextTokenIndex < state.Tokens.Length) && (state.Tokens[state.NextTokenIndex]._type = tokenType) then
        Some state.Tokens[state.NextTokenIndex]
    else
        None

let peekToken (state: ParseState): Option<Token> * ParseState =
    let optionNextToken = tryPeekToken state

    match optionNextToken with
    | Some nextToken ->
        (optionNextToken, state)
    | None ->
        let error = CompileError(
            description = "Unexpectedly reached the end of the tokens.",
            position = currentTextPosition state
        )

        (
            optionNextToken,
            { state with Errors = List.append state.Errors [error] }
        )
        
let peekExpectedToken (state: ParseState) (expectedTokenType: TokenType): Option<Token> * ParseState =
    let (optionNextToken, state) = peekToken state

    match optionNextToken with
    | Some nextToken ->
        if nextToken._type = expectedTokenType then
            (Some nextToken, state)
        else
            let error = getUnexpectedTokenError state expectedTokenType nextToken

            (None, { state with Errors = state.Errors @ [error] })
    | None -> (None, state)

let readToken (state: ParseState): Option<Token> * ParseState =
    let (optionNextToken, state) = peekToken state

    match optionNextToken with
    | Some nextToken -> (optionNextToken, { state with NextTokenIndex = state.NextTokenIndex + 1 })
    | None -> (optionNextToken, state)

let readExpectedToken (state: ParseState) (expectedTokenType: TokenType): Option<Token> * ParseState =
    let (optionNextToken, state) = peekToken state

    match optionNextToken with
    | Some nextToken ->
        if nextToken._type = expectedTokenType then
            (optionNextToken, { state with NextTokenIndex = state.NextTokenIndex + 1 })
        else
            let error = getUnexpectedTokenError state expectedTokenType nextToken

            (None, { state with Errors = state.Errors @ [error] })
    | None -> (optionNextToken, state)

let parseToken (state: ParseState) (tokenType: TokenType): Option<ParseNode> * ParseState =
    let (optionNextToken, state) = readExpectedToken state tokenType

    match optionNextToken with
    | Some nextToken -> (Some { Type = ParseNodeType.Token; Children = []; Token = Some nextToken }, state)
    | None -> (None, state)

let parseTrivia (state: ParseState): List<ParseNode> * ParseState = 
    let whitespace =
           List.skip state.NextTokenIndex state.Tokens
        |> List.takeWhile (fun t -> isTriviaToken t._type)
        |> List.map (
            fun t ->
                {
                    Type = (
                        match t._type with
                        | TokenType.whitespace -> ParseNodeType.Whitespace
                        | TokenType.comment -> ParseNodeType.Comment
                        | _ -> failwith "Assert failed"
                    )
                    Children = []
                    Token = Some t
                }
            )

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
    (optionTerminatorTokenType: Option<TokenType>)
    (accumulator: List<ParseNode>):
    List<ParseNode> * ParseState =
        let optionNextToken = tryPeekToken state

        match optionNextToken with
        | Some nextToken ->
            if optionTerminatorTokenType.IsNone || (nextToken._type <> optionTerminatorTokenType.Value) then
                let (maybeNode, state) = parseFn state

                match maybeNode with
                | Some node ->
                    let (whitespace, state) = parseTrivia state
                    let accumulator = accumulator @ [node] @ whitespace
                    let optionNextToken = tryPeekExpectedToken state tokenType

                    match optionNextToken with
                    | Some nextToken ->
                        let (optionSeparator, state) = parseToken state tokenType

                        match optionSeparator with
                        | Some separator ->
                            let (whitespace2, state) = parseTrivia state
                            let accumulator = accumulator @ [separator] @ whitespace2
                        
                            parseSeparatedByToken state parseFn tokenType optionTerminatorTokenType accumulator
                        | None -> (accumulator, state)
                    | None -> (accumulator, state)
                | None -> (accumulator, state)
            else (accumulator, state)
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
            if optionTerminatorTokenType.IsNone || (nextToken._type <> optionTerminatorTokenType.Value) then
                let (maybeNode, state) = parseFn state

                match maybeNode with
                | Some node ->
                    let (whitespace, state) = parseTrivia state
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
                let (whitespace, state) = parseTrivia state
                let newNodes = List.append [node] whitespace
                let accumulator = List.append accumulator newNodes

                if not (isDone state) then
                    parseOptionalWhitespaceSeparatedUntilDone state parseFn accumulator
                else
                    (accumulator, state)
            | None -> (accumulator, state)
        | None -> (accumulator, state)

and parseTypeParameter (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.identifier

    match optionIdentifier with
    | Some identifier ->
        (
            Some {
                Type = ParseNodeType.TypeParameter
                Children = [identifier]
                Token = None
            },
            state
        )
    | None -> (None, state)

and parseParameter (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.identifier

    match optionIdentifier with
    | Some identifier ->
        let (whitespace1, state) = parseTrivia state
        let (optionColon, state) = parseToken state TokenType.colon

        match optionColon with
        | Some colon ->
            let (whitespace2, state) = parseTrivia state
            let (optionTypeExpression, state) = parseExpression state

            match optionTypeExpression with
            | Some typeExpression ->
                (
                    Some {
                        Type = ParseNodeType.Parameter
                        Children = List.concat [ [identifier]; whitespace1; [colon]; whitespace2; [typeExpression] ]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

and parseRecordField (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.identifier

    match optionIdentifier with
    | Some identifier ->
        let (whitespace1, state) = parseTrivia state
        let (optionColon, state) = parseToken state TokenType.colon

        match optionColon with
        | Some colon ->
            let (whitespace2, state) = parseTrivia state
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
    let (optionRecordToken, state) = parseToken state TokenType.record_keyword

    match optionRecordToken with
    | Some recordToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionNextToken, state) = peekToken state

        match optionNextToken with
        | Some nextToken ->
            let (typeParametersNodes, state) =
                if nextToken._type = TokenType.left_square_bracket then
                    let (optionLessThan, state) = parseToken state TokenType.left_square_bracket
                    
                    match optionLessThan with
                    | Some lessThan ->
                        let (whitespace2, state) = parseTrivia state
                        let (typeParameters, state) = parseSeparatedByToken state parseTypeParameter TokenType.comma (Some TokenType.right_square_bracket) []
                        let (whitespace3, state) = parseTrivia state
                        let (optionGreaterThan, state) = parseToken state TokenType.right_square_bracket

                        match optionGreaterThan with
                        | Some greaterThan ->
                            let (trailingWhiteSpace, state) = parseTrivia state

                            ((List.concat [ [lessThan]; whitespace2; typeParameters; whitespace3; [greaterThan]; trailingWhiteSpace ]), state)
                        | None -> ([], state)
                    | None -> ([], state)
                else
                    ([], state)

            let (optionLeftCurlyBracket, state) = parseToken state TokenType.left_curly_bracket

            match optionLeftCurlyBracket with
            | Some leftCurlyBracket -> 
                let (whitespace2, state) = parseTrivia state
                let (fields, state) = parseSeparatedByWhitespace state parseRecordField (Some TokenType.right_curly_bracket) []
                let (whitespace3, state) = parseTrivia state
                let (optionRightCurlyBracket, state) = parseToken state TokenType.right_curly_bracket

                match optionRightCurlyBracket with
                | Some rightCurlyBracket ->
                    (
                        Some {
                            Type = ParseNodeType.Record
                            Children = List.concat [ [recordToken]; whitespace1; typeParametersNodes; [leftCurlyBracket]; whitespace2; fields; whitespace3; [rightCurlyBracket] ]
                            Token = None
                        },
                        state
                    )
                | None -> (None, state)
            | None -> (None, state)
        | _ -> (None, state)
    | None -> (None, state)

and parseEnumCase (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.identifier

    match optionIdentifier with
    | Some identifier ->
        let (optionLeftParen, state) = tryPeekExpectedTokenAfterTrivia state TokenType.left_paren

        match optionLeftParen with
        | Some _ ->
            let (whitespace1, state) = parseTrivia state
            let (optionLeftParen, state) = parseToken state TokenType.left_paren
            let leftParen = optionLeftParen.Value
            let (whitespace2, state) = parseTrivia state
            let (parameters, state) = parseSeparatedByToken state parseParameter TokenType.comma (Some TokenType.right_paren) []
            let (whitespace3, state) = parseTrivia state
            let (optionRightParen, state) = parseToken state TokenType.right_paren

            match optionRightParen with
            | Some rightParen ->
                (
                    Some {
                        Type = ParseNodeType.EnumCase
                        Children = List.concat [ [identifier]; whitespace1; [leftParen]; whitespace2; parameters; whitespace3; [rightParen]; ]
                        Token = None
                    },
                    state
                )
            | None -> (None, state)
        | None ->
            (
                Some {
                    Type = ParseNodeType.EnumCase
                    Children = List.concat [ [identifier] ]
                    Token = None
                },
                state
            )
    | None -> (None, state)

and parseTrait (state: ParseState): Option<ParseNode> * ParseState =
    let (optionTraitToken, state) = parseToken state TokenType.trait_keyword

    match optionTraitToken with
    | Some traitToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionNextToken, state) = peekToken state

        match optionNextToken with
        | Some nextToken ->
            let (typeParametersNodes, state) =
                if nextToken._type = TokenType.left_square_bracket then
                    let (optionLessThan, state) = parseToken state TokenType.left_square_bracket
                    
                    match optionLessThan with
                    | Some lessThan ->
                        let (whitespace2, state) = parseTrivia state
                        let (typeParameters, state) = parseSeparatedByToken state parseTypeParameter TokenType.comma (Some TokenType.right_square_bracket) []
                        let (whitespace3, state) = parseTrivia state
                        let (optionGreaterThan, state) = parseToken state TokenType.right_square_bracket

                        match optionGreaterThan with
                        | Some greaterThan ->
                            let (trailingWhiteSpace, state) = parseTrivia state

                            ((List.concat [ [lessThan]; whitespace2; typeParameters; whitespace3; [greaterThan]; trailingWhiteSpace ]), state)
                        | None -> ([], state)
                    | None -> ([], state)
                else
                    ([], state)

            let (optionLeftCurlyBracket, state) = parseToken state TokenType.left_curly_bracket

            match optionLeftCurlyBracket with
            | Some leftCurlyBracket -> 
                let (whitespace2, state) = parseTrivia state
                let (fields, state) = parseSeparatedByWhitespace state parseBinding (Some TokenType.right_curly_bracket) []
                let (whitespace3, state) = parseTrivia state
                let (optionRightCurlyBracket, state) = parseToken state TokenType.right_curly_bracket

                match optionRightCurlyBracket with
                | Some rightCurlyBracket ->
                    (
                        Some {
                            Type = ParseNodeType.Trait
                            Children = List.concat [ [traitToken]; whitespace1; typeParametersNodes; [leftCurlyBracket]; whitespace2; fields; whitespace3; [rightCurlyBracket] ]
                            Token = None
                        },
                        state
                    )
                | None -> (None, state)
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

and parseEnum (state: ParseState): Option<ParseNode> * ParseState =
    let (optionEnumToken, state) = parseToken state TokenType.enum_keyword

    match optionEnumToken with
    | Some enumToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionNextToken, state) = peekToken state

        match optionNextToken with
        | Some nextToken ->
            let (typeParametersNodes, state) =
                if nextToken._type = TokenType.left_square_bracket then
                    let (optionLessThan, state) = parseToken state TokenType.left_square_bracket
                    
                    match optionLessThan with
                    | Some lessThan ->
                        let (whitespace2, state) = parseTrivia state
                        let (typeParameters, state) = parseSeparatedByToken state parseTypeParameter TokenType.comma (Some TokenType.right_square_bracket) []
                        let (whitespace3, state) = parseTrivia state
                        let (optionGreaterThan, state) = parseToken state TokenType.right_square_bracket

                        match optionGreaterThan with
                        | Some greaterThan ->
                            let (trailingWhiteSpace, state) = parseTrivia state

                            ((List.concat [ [lessThan]; whitespace2; typeParameters; whitespace3; [greaterThan]; trailingWhiteSpace ]), state)
                        | None -> ([], state)
                    | None -> ([], state)
                else
                    ([], state)
            let (optionLeftCurlyBracket, state) = parseToken state TokenType.left_curly_bracket

            match optionLeftCurlyBracket with
            | Some leftCurlyBracket -> 
                let (whitespace2, state) = parseTrivia state
                let (cases, state) = parseSeparatedByWhitespace state parseEnumCase (Some TokenType.right_curly_bracket) []
                let (whitespace3, state) = parseTrivia state
                let (optionRightCurlyBracket, state) = parseToken state TokenType.right_curly_bracket

                match optionRightCurlyBracket with
                | Some rightCurlyBracket ->
                    (
                        Some {
                            Type = ParseNodeType.Enum
                            Children = List.concat [ [enumToken]; whitespace1; typeParametersNodes; [leftCurlyBracket]; whitespace2; cases; whitespace3; [rightCurlyBracket] ]
                            Token = None
                        },
                        state
                    )
                | None -> (None, state)
            | None -> (None, state)
        | None -> (None, state)
    | None -> (None, state)

and parseIfThenElse (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIfToken, state) = parseToken state TokenType.if_keyword

    match optionIfToken with
    | Some ifToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionIfExpression, state) = parseExpression state

        match optionIfExpression with
        | Some ifExpression ->
            let (whitespace2, state) = parseTrivia state
            let (optionThenToken, state) = parseToken state TokenType.then_keyword

            match optionThenToken with
            | Some thenToken ->
                let (whitespace3, state) = parseTrivia state
                let (optionThenExpression, state) = parseExpression state

                match optionThenExpression with
                | Some thenExpression ->
                    let (whitespace4, state) = parseTrivia state
                    let (optionElseToken, state) = parseToken state TokenType.else_keyword

                    match optionElseToken with
                    | Some elseToken ->
                        let (whitespace5, state) = parseTrivia state
                        let (optionElseExpression, state) = parseExpression state

                        match optionElseExpression with
                        | Some elseExpression ->
                            (
                                Some {
                                    Type = ParseNodeType.IfThenElse
                                    Children = List.concat [ [ifToken]; whitespace1; [ifExpression]; whitespace2; [thenToken]; whitespace3; [thenExpression]; whitespace4; [elseToken]; whitespace5; [elseExpression] ]
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

and parseBindingOrExpression (state: ParseState): Option<ParseNode> * ParseState =
    if (tryPeekTokenSequenceIgnoreTrivia state [TokenType.identifier; TokenType.equals]) then
        parseBinding state
    else
        parseExpression state

and parseBlock (state: ParseState): Option<ParseNode> * ParseState =
    let (optionLeftCurlyBracket, state) = parseToken state TokenType.left_curly_bracket

    match optionLeftCurlyBracket with
    | Some leftCurlyBracket ->
        let (whitespace1, state) = parseTrivia state
        let (children, state) = parseSeparatedByWhitespace state parseBindingOrExpression (Some TokenType.right_curly_bracket) []
        let (optionRightCurlyBracket, state) = parseToken state TokenType.right_curly_bracket

        match optionRightCurlyBracket with
        | Some rightCurlyBracket ->
            (
                Some {
                    Type = ParseNodeType.Block
                    Children = List.concat [ [leftCurlyBracket]; whitespace1; children; [rightCurlyBracket] ]
                    Token = None
                },
                state
            )
        | None -> (None, state)
    | None -> (None, state)

and parseParenthesizedExpressionOrTuple (state: ParseState): Option<ParseNode> * ParseState =
    let (optionLeftParen, state) = parseToken state TokenType.left_paren
    
    match optionLeftParen with
    | Some leftParen ->
        let (whitespace1, state) = parseTrivia state
        let (innerNodes, state) = parseSeparatedByToken state parseExpression TokenType.comma (Some TokenType.right_paren) []
        let (whitespace2, state) = parseTrivia state
        let (optionRightParen, state) = parseToken state TokenType.right_paren

        match optionRightParen with
        | Some rightParen ->
            let expressionNodes = List.filter (fun c -> c.Type = ParseNodeType.Expression) innerNodes

            match expressionNodes.Length with
            | 0 ->
                let error = CompileError(
                    description = "Empty tuples are not allowed",
                    position = currentTextPosition state
                )

                (None, { state with Errors = List.append state.Errors [error] })
            | 1 ->
                (
                    Some {
                        Type = ParseNodeType.ParenthesizedExpression
                        Children = List.concat [[leftParen]; whitespace1; innerNodes; whitespace2; [rightParen]]
                        Token = None
                    },
                    state
                )
            | _ ->
                (
                    Some {
                        Type = ParseNodeType.TupleExpression
                        Children = List.concat [[leftParen]; whitespace1; innerNodes; whitespace2; [rightParen]]
                        Token = None
                    },
                    state
                )
        | None -> (None, state)
    | None -> (None, state)

and parseFunctionReturnType (state: ParseState): List<ParseNode> * ParseState =
    // read whitespace, then the colon, then more whitespace, then parse an expression
    let (whitespace1, state) = parseTrivia state
    let (optionColon, state) = parseToken state TokenType.colon

    match optionColon with
    | Some colon ->
        let (whitespace2, state) = parseTrivia state
        let (optionReturnTypeExpression, state) = parseExpression state

        match optionReturnTypeExpression with
        | Some returnTypeExpression ->
            (List.concat [whitespace1; [colon]; whitespace2; [returnTypeExpression]], state)
        | None -> ([], state)
    | None ->
        ([], state)

and parseFunctionOrFunctionType (state: ParseState): Option<ParseNode> * ParseState =
    let (optionFnToken, state) = parseToken state TokenType.fn_keyword

    match optionFnToken with
    | Some fnToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionNextToken, state) = peekToken state

        match optionNextToken with
        | Some nextToken ->
            let (typeParametersNodes, state) =
                if nextToken._type = TokenType.left_square_bracket then
                    let (optionLessThan, state) = parseToken state TokenType.left_square_bracket
                    
                    match optionLessThan with
                    | Some lessThan ->
                        let (whitespace2, state) = parseTrivia state
                        let (typeParameters, state) = parseSeparatedByToken state parseTypeParameter TokenType.comma (Some TokenType.right_square_bracket) []
                        let (whitespace3, state) = parseTrivia state
                        let (optionGreaterThan, state) = parseToken state TokenType.right_square_bracket

                        match optionGreaterThan with
                        | Some greaterThan ->
                            let (trailingWhiteSpace, state) = parseTrivia state

                            ((List.concat [ [lessThan]; whitespace2; typeParameters; whitespace3; [greaterThan]; trailingWhiteSpace ]), state)
                        | None -> ([], state)
                    | None -> ([], state)
                else
                    ([], state)

            let (optionLeftParen, state) = parseToken state TokenType.left_paren

            match optionLeftParen with
            | Some leftParen ->
                let (whitespace4, state) = parseTrivia state
                let (parameters, state) = parseSeparatedByToken state parseParameter TokenType.comma (Some TokenType.right_paren) []
                let (whitespace5, state) = parseTrivia state
                let (optionRightParen, state) = parseToken state TokenType.right_paren

                match optionRightParen with
                | Some rightParen ->
                    // If next TokenType is ':', then parse it & a return type expression.
                    let (optionColonToken, state) = tryPeekExpectedTokenAfterTrivia state TokenType.colon
                    let (returnTypeNodes, state) =
                        match optionColonToken with
                        | Some _ -> parseFunctionReturnType state
                        | None -> ([], state)

                    let (optionRightArrow, state) = tryPeekExpectedTokenAfterTrivia state TokenType.right_arrow
                    
                    match optionRightArrow with
                    | None ->
                        if returnTypeNodes.IsEmpty then
                            let error = CompileError(
                                description = "Function types must specify their return types",
                                position = currentTextPosition state
                            )

                            (None, { state with Errors = state.Errors @ [error] })
                        else
                            (
                                Some {
                                    Type = ParseNodeType.FunctionType
                                    Children = List.concat [ [fnToken]; whitespace1; typeParametersNodes; [leftParen]; whitespace4; parameters; whitespace5; [rightParen]; returnTypeNodes; ]
                                    Token = None
                                },
                                state
                            )
                    | Some _ ->
                        let (whitespace6, state) = parseTrivia state
                        let (optionRightArrow, state) = parseToken state TokenType.right_arrow

                        match optionRightArrow with
                        | Some rightArrow ->
                            let (whitespace7, state) = parseTrivia state
                            let (optionResult, state) = parseExpression state

                            match optionResult with
                            | Some result ->
                                (
                                    Some {
                                        Type = ParseNodeType.Function
                                        Children = List.concat [ [fnToken]; whitespace1; typeParametersNodes; [leftParen]; whitespace4; parameters; whitespace5; [rightParen]; returnTypeNodes; whitespace6; [rightArrow]; whitespace7; [result] ]
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

    
and parseTypeArgument (state: ParseState): Option<ParseNode> * ParseState =
    let (optionExpression, state) = parseExpression state

    match optionExpression with
    | Some expression ->
        (
            Some {
                Type = ParseNodeType.TypeArgument
                Children = [expression]
                Token = None
            },
            state
        )
    | None -> (None, state)

and parseFunctionCall (state: ParseState) (prefixExpression: ParseNode): Option<ParseNode> * ParseState =
    let (optionLeftSquareBracket, state) = peekToken state

    // I want to modify the let statement below to return all parsed nodes in addition to the state.
    let (typeArgumentNodes, state) =
        match optionLeftSquareBracket with
        | Some leftSquareBracket ->
            match leftSquareBracket._type with
            | TokenType.left_square_bracket ->
                let (optionLeftSquareBracket, state) = parseToken state TokenType.left_square_bracket
                match optionLeftSquareBracket with
                | Some leftSquareBracket ->
                    let (whitespace1, state) = parseTrivia state
                    let (typeArguments, state) = parseSeparatedByToken state parseTypeArgument TokenType.comma (Some TokenType.right_square_bracket) []
                    let (whitespace2, state) = parseTrivia state
                    let (optionGreaterThan, state) = parseToken state TokenType.right_square_bracket
                    match optionGreaterThan with
                    | Some greaterThan ->
                        let (trailingWhiteSpace, state) = parseTrivia state
                    
                        (List.concat [ [leftSquareBracket]; whitespace1; typeArguments; whitespace2; [greaterThan]; trailingWhiteSpace ], state)
                    | None -> ([], state)
                | None -> ([], state)
            | _ -> ([], state)
        | None -> ([], state)

    let (optionLeftParen, state) = parseToken state TokenType.left_paren

    match optionLeftParen with
    | Some leftParen ->
        let (whitespace1, state) = parseTrivia state
        let (arguments, state) = parseSeparatedByToken state parseExpression TokenType.comma (Some TokenType.right_paren) []
        let (whitespace2, state) = parseTrivia state
        let (optionRightParen, state) = parseToken state TokenType.right_paren

        match optionRightParen with
        | Some rightParen ->
            (
                Some {
                    Type = ParseNodeType.FunctionCall
                    Children = List.concat [ [prefixExpression]; typeArgumentNodes; [leftParen]; whitespace1; arguments; whitespace2; [rightParen] ]
                    Token = None
                },
                state
            )
        | None -> (None, state)
    | None -> (None, state)

and parseGenericInstantiation (state: ParseState) (prefixExpression: ParseNode): Option<ParseNode> * ParseState =
    let (optionLeftSquareBracket, state) = peekToken state

    match optionLeftSquareBracket with
        | Some leftSquareBracket ->
            match leftSquareBracket._type with
            | TokenType.left_square_bracket ->
                let (optionLeftSquareBracket, state) = parseToken state TokenType.left_square_bracket
                match optionLeftSquareBracket with
                | Some leftSquareBracket ->
                    let (whitespace1, state) = parseTrivia state
                    let (typeArguments, state) = parseSeparatedByToken state parseTypeArgument TokenType.comma (Some TokenType.right_square_bracket) []
                    let (whitespace2, state) = parseTrivia state
                    let (optionGreaterThan, state) = parseToken state TokenType.right_square_bracket
                    match optionGreaterThan with
                    | Some greaterThan ->
                        let innerNode = {
                            Type = ParseNodeType.GenericInstantiation
                            Children = List.concat [ [prefixExpression]; [leftSquareBracket]; whitespace1; typeArguments; whitespace2; [greaterThan] ]
                            Token = None
                        }
                        
                        (
                            Some {
                                Type = ParseNodeType.Expression
                                Children = [innerNode]
                                Token = None
                            },
                            state
                        )
                    | None -> (None, state)
                | None -> (None, state)
            | _ -> (None, state)
        | None -> (None, state)

and parseErrorPropagationExpression (state: ParseState) (prefixExpression: ParseNode): Option<ParseNode> * ParseState =
    let (optionQuestionMark, state) = parseToken state TokenType.question_mark

    match optionQuestionMark with
        | Some questionMark ->
            let innerNode = {
                Type = ParseNodeType.ErrorPropagationOperator
                Children = List.concat [ [prefixExpression]; [questionMark] ]
                Token = None
            }
                        
            (
                Some innerNode,
                state
            )
        | None -> (None, state)

and parseMemberAccess (state: ParseState) (prefixExpression: ParseNode) (rightBindingPower: int): Option<ParseNode> * ParseState =
    let (optionPeriod, state) = parseToken state TokenType.period

    match optionPeriod with
    | Some period ->
        let (whitespace1, state) = parseTrivia state
        let (optionIdentifier, state) = parseToken state TokenType.identifier

        match optionIdentifier with
        | Some identifier ->
            let rightExpression = {
                Type = ParseNodeType.Expression
                Children = [identifier]
                Token = None
            }

            let accessNode =
                {
                    Type = ParseNodeType.MemberAccess
                    Children = List.concat [ [prefixExpression]; [period]; whitespace1; [rightExpression]; ]
                    Token = None
                }

            (
                Some {
                    Type = ParseNodeType.Expression
                    Children = [accessNode]
                    Token = None
                },
                state
            )
        | None -> (None, state)
    | None -> (None, state)

and parsePrefixExpression (state: ParseState): Option<ParseNode> * ParseState = 
    let (optionNextToken, state) = peekToken state

    let wrapInExpressionNode (parseFnOutput: Option<ParseNode> * ParseState): Option<ParseNode> * ParseState =
        let (optionNode, state) = parseFnOutput

        match optionNode with
        | Some node ->
            (
                Some {
                    Type = ParseNodeType.Expression
                    Children = [node]
                    Token = None
                },
                state
            )
        | None -> parseFnOutput
    
    match optionNextToken with
    | Some nextToken ->
        match nextToken._type with
        | TokenType.fn_keyword ->
            wrapInExpressionNode (parseFunctionOrFunctionType state)
        | TokenType.record_keyword ->
            wrapInExpressionNode (parseRecord state)
        | TokenType.enum_keyword ->
            wrapInExpressionNode (parseEnum state)
        | TokenType.trait_keyword ->
            wrapInExpressionNode (parseTrait state)
        | TokenType.if_keyword ->
            wrapInExpressionNode (parseIfThenElse state)
        | TokenType.left_curly_bracket ->
            wrapInExpressionNode (parseBlock state)
        | TokenType.Self_keyword ->
            wrapInExpressionNode (parseToken state TokenType.Self_keyword)
        | TokenType.identifier ->
            wrapInExpressionNode (parseToken state TokenType.identifier)
        | TokenType.number_literal ->
            wrapInExpressionNode (parseToken state TokenType.number_literal)
        | TokenType.character_literal ->
            wrapInExpressionNode (parseToken state TokenType.character_literal)
        | TokenType.string_literal ->
            wrapInExpressionNode (parseToken state TokenType.string_literal)
        | TokenType.left_paren ->
            wrapInExpressionNode (parseParenthesizedExpressionOrTuple state)
        | _ ->
            let error = CompileError(
                description = $"Encountered unexpected TokenType: \"{nextToken.text}\"",
                position = currentTextPosition state
            )

            (None, { state with Errors = List.append state.Errors [error] })
    | None -> (None, state)

and parseExpressionInternal (state: ParseState) (minBindingPower: int): Option<ParseNode> * ParseState =
    let (optionPrefixExpression, state) = parsePrefixExpression state

    match optionPrefixExpression with
    | Some prefixExpression ->
        let (optionExpression, state) = tryParsePostfixAndInfixExpressions state prefixExpression minBindingPower

        match optionExpression with
        | Some expression ->
            (
                Some {
                    Type = ParseNodeType.Expression
                    Children = [expression]
                    Token = None
                },
                state
            )
        | None -> (None, state)
    | None -> (None, state)

and parseExpression (state: ParseState): Option<ParseNode> * ParseState =
    parseExpressionInternal state 0

and getPostfixLeftBindingPower (tokenType: TokenType): Option<int> =
    match tokenType with
    | TokenType.left_paren -> Some 12
    | TokenType.left_square_bracket -> Some 12
    | TokenType.question_mark -> Some 11
    | _ -> None

and getInfixBindingPowers (tokenType: TokenType): Option<int * int> =
    match tokenType with
    | TokenType.period -> Some (13, 14)
    | TokenType.double_greater_than -> Some (10, 9)
    | TokenType.asterisk -> Some (7, 8)
    | TokenType.forward_slash -> Some (7, 8)
    | TokenType.plus_sign -> Some (5, 6)
    | TokenType.minus -> Some (5, 6)
    | TokenType.equality_operator -> Some (3, 4)
    | _ -> None

and tryParsePostfixAndInfixExpressions (state: ParseState) (prefixExpression: ParseNode) (minBindingPower: int): Option<ParseNode> * ParseState =
    let optionNextToken = tryPeekTokenAfterTrivia state

    match optionNextToken with
    | Some nextToken ->
        let optionPostfixLeftBindingPower = getPostfixLeftBindingPower nextToken._type

        match optionPostfixLeftBindingPower with
        | Some postfixLeftBindingPower ->
            if postfixLeftBindingPower < minBindingPower then
                (Some prefixExpression, state)
            else
                if nextToken._type = TokenType.left_paren then
                    let (whitespace, state) = parseTrivia state
                    let prefixExpression = { prefixExpression with Children = prefixExpression.Children @ whitespace }

                    let (optionFunctionCall, state) = parseFunctionCall state prefixExpression

                    match optionFunctionCall with
                    | Some functionCall ->
                        let expressionNode =
                            {
                                Type = ParseNodeType.Expression
                                Children = [functionCall]
                                Token = None
                            }
                        tryParsePostfixAndInfixExpressions state expressionNode minBindingPower
                    | None ->
                        (None, state)
                else if nextToken._type = TokenType.left_square_bracket then
                    let (whitespace, state) = parseTrivia state
                    let prefixExpression = { prefixExpression with Children = prefixExpression.Children @ whitespace }

                    // parse type arguments
                    let (optionGenericInstantiation, state) = parseGenericInstantiation state prefixExpression

                    match optionGenericInstantiation with
                    | Some genericInstantiation ->
                        let expressionNode =
                            {
                                Type = ParseNodeType.Expression
                                Children = [genericInstantiation]
                                Token = None
                            }
                        tryParsePostfixAndInfixExpressions state expressionNode minBindingPower
                    | None ->
                        (None, state)
                else if nextToken._type = TokenType.question_mark then
                    let (whitespace, state) = parseTrivia state
                    let prefixExpression = { prefixExpression with Children = prefixExpression.Children @ whitespace }

                    let (optionErrorPropagationExpression, state) = parseErrorPropagationExpression state prefixExpression

                    match optionErrorPropagationExpression with
                    | Some errorPropagationExpression ->
                        let expressionNode =
                            {
                                Type = ParseNodeType.Expression
                                Children = [errorPropagationExpression]
                                Token = None
                            }
                        tryParsePostfixAndInfixExpressions state expressionNode minBindingPower
                    | None ->
                        (None, state)
                else
                    failwith "Assert failed"
        | None ->
            let optionInfixBindingPowers = getInfixBindingPowers nextToken._type

            match optionInfixBindingPowers with
            | Some infixBindingPowers ->
                let (leftBindingPower, rightBindingPower) = infixBindingPowers

                if leftBindingPower < minBindingPower then
                    (Some prefixExpression, state)
                else
                    let parseSingleTokenBinaryOperator (tokenType: TokenType) (parseNodeType: ParseNodeType) =
                        let (whitespace, state) = parseTrivia state
                        let prefixExpression = { prefixExpression with Children = prefixExpression.Children @ whitespace }

                        let (optionBinOpToken, state) = parseToken state tokenType

                        match optionBinOpToken with
                        | Some binOpToken ->
                            let (whitespace1, state) = parseTrivia state
                            let (optionRightExpression, state) = parseExpressionInternal state rightBindingPower

                            match optionRightExpression with
                            | Some rightExpression ->
                                let accessNode =
                                    {
                                        Type = parseNodeType
                                        Children = List.concat [ [prefixExpression]; [binOpToken]; whitespace1; [rightExpression]; ]
                                        Token = None
                                    }

                                let expressionNode =
                                    {
                                        Type = ParseNodeType.Expression
                                        Children = [accessNode]
                                        Token = None
                                    }
                            
                                tryParsePostfixAndInfixExpressions state expressionNode minBindingPower
                            | None -> (None, state)
                        | None -> (None, state)

                    match nextToken._type with
                    | TokenType.period ->
                        let (whitespace, state) = parseTrivia state
                        let prefixExpression = { prefixExpression with Children = prefixExpression.Children @ whitespace }

                        let (optionMemberAccess, state) = parseMemberAccess state prefixExpression rightBindingPower

                        match optionMemberAccess with
                        | Some memberAccess ->
                            tryParsePostfixAndInfixExpressions state memberAccess minBindingPower
                        | None -> (None, state)
                    | TokenType.plus_sign ->
                        parseSingleTokenBinaryOperator TokenType.plus_sign ParseNodeType.Addition
                    | TokenType.minus ->
                        parseSingleTokenBinaryOperator TokenType.minus ParseNodeType.Subtraction
                    | TokenType.asterisk ->
                        parseSingleTokenBinaryOperator TokenType.asterisk ParseNodeType.Multiplication
                    | TokenType.forward_slash ->
                        parseSingleTokenBinaryOperator TokenType.forward_slash ParseNodeType.Division
                    | TokenType.equality_operator ->
                        parseSingleTokenBinaryOperator TokenType.equality_operator ParseNodeType.Equals
                    | TokenType.double_greater_than ->
                        parseSingleTokenBinaryOperator TokenType.double_greater_than ParseNodeType.ReverseFunctionComposition
                    | _ -> failwith "Assert failed"
            | None -> (Some prefixExpression, state)
    | None -> (Some prefixExpression, state)

and parseBinding (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state TokenType.identifier

    match optionIdentifier with
    | Some identifier ->
        if identifier.Token.Value.text = "_" then
            let error = CompileError(
                description = $"\"_\" is a reserved identifier",
                position = currentTextPosition state
            )

            (None, { state with Errors = List.append state.Errors [error] })
        else
        let (whitespace1, state) = parseTrivia state
        let (optionEqualsSign, state) = parseToken state TokenType.equals
        
        match optionEqualsSign with
        | Some equalsSign ->
            let (whitespace2, state) = parseTrivia state
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

let parseCompilationUnit (state: ParseState): ParseNode * ParseState =
    let (initialWhitespace, state) = parseTrivia state
    let (children, state) = parseOptionalWhitespaceSeparatedUntilDone state parseBinding []
    let compilationUnit: ParseNode = {
        Type = ParseNodeType.CompilationUnit
        Children = List.append initialWhitespace children
        Token = None
    }

    (compilationUnit, state)

let parse (tokens: List<Token>): ParseOutput =
    let seedState: ParseState = {
        Tokens = tokens
        NextTokenIndex = 0
        Errors = []
    }
    let (compilationUnit, finalState) = parseCompilationUnit seedState

    { CompilationUnit = compilationUnit; Errors = finalState.Errors }
