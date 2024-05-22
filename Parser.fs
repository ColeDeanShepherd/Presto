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
    | Union
    | Identifier
    | IfThenElse
TypeExpression -> Expression
Function -> "fn" "(" SepBy(Parameter, ",") ")" "->" Expression
Record -> "record" SepBy(Whitespace, Field)
Union -> "union" SepBy(Whitespace, UnionCase)
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
    | Union
    | UnionCase
    | Function
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

type ParseNode = {
    Type: ParseNodeType
    Children: List<ParseNode>
    Token: Option<token>
}

type ParseOutput = {
    CompilationUnit: ParseNode
    Errors: List<compile_error>
}

type ParseState = {
    Tokens: List<token>
    NextTokenIndex: int
    Errors: List<compile_error>
}

let rec nodeTextPosition (node: ParseNode): text_position =
    match node.Token with
    | Some token -> token.position
    | None -> nodeTextPosition node.Children.Head

let isTriviaParseNode (parseNodeType: ParseNodeType): bool =
    (parseNodeType = ParseNodeType.Whitespace) || (parseNodeType = ParseNodeType.Comment)

let nonTriviaChild (node: ParseNode): ParseNode =
    List.find (fun c -> not (isTriviaParseNode c.Type)) node.Children

let childOfTokenType (node: ParseNode) (tokenType: token_type): ParseNode =
    List.find (fun c -> (c.Type = ParseNodeType.Token) && (c.Token.Value._type = tokenType)) node.Children

let childOfType (node: ParseNode) (nodeType: ParseNodeType): ParseNode =
    List.find (fun c -> c.Type = nodeType) node.Children

let childrenOfType (node: ParseNode) (nodeType: ParseNodeType): List<ParseNode> =
    List.filter (fun c -> c.Type = nodeType) node.Children

let childrenOfTypes (node: ParseNode) (nodeTypes: List<ParseNodeType>): List<ParseNode> =
    List.filter (fun c -> List.contains c.Type nodeTypes) node.Children

let isDone (state: ParseState): bool =
    state.NextTokenIndex >= state.Tokens.Length

let tryPeekToken (state: ParseState): Option<token> =
    if state.NextTokenIndex < state.Tokens.Length then
        Some state.Tokens[state.NextTokenIndex]
    else
        None

let tryPeekTokenIndexAfterTrivia (state: ParseState): Option<int> =
       Seq.indexed state.Tokens
    |> Seq.skip state.NextTokenIndex
    |> Seq.tryFind (fun (i, t) -> not (isTriviaToken t._type))
    |> Option.bind (fun (i, t) -> Some i)

let tryPeekTokenAfterTrivia (state: ParseState): Option<token> =
       Seq.skip state.NextTokenIndex state.Tokens
    |> Seq.tryFind (fun t -> not (isTriviaToken t._type))
    
let currentTextPosition (state: ParseState): text_position =
    if state.NextTokenIndex < state.Tokens.Length then
        state.Tokens[state.NextTokenIndex].position
    else
        text_position(file_name = "", line_index = 0u, column_index = 0u)

let peekTokenAfterTrivia (state: ParseState): Option<token> * ParseState =
    let optionNextToken = tryPeekTokenAfterTrivia state

    match optionNextToken with
    | Some _ ->
        (optionNextToken, state)
    | None ->
        let error = compile_error(
            description = "Unexpectedly reached the end of the tokens.", position = currentTextPosition state
        )

        (
            optionNextToken,
            { state with Errors = state.Errors @ [error] }
        )

let rec tryPeekTokenSequenceIgnoreTrivia (state: ParseState) (tokenSequence: List<token_type>): bool =
    if tokenSequence.IsEmpty then
        true
    else
        let optionTokenIndex = tryPeekTokenIndexAfterTrivia state

        match optionTokenIndex with
        | Some tokenIndex ->
            let token = state.Tokens[tokenIndex]

            if token._type = tokenSequence.Head then
                let state = { state with NextTokenIndex = tokenIndex + 1 }

                tryPeekTokenSequenceIgnoreTrivia state tokenSequence.Tail
            else
                false
        | None -> false

let getUnexpectedTokenError (state: ParseState) (expectedTokenType: token_type) (nextToken: token): compile_error =
    let virtualErrorDescriptionPart =
        if nextToken.was_inserted then
            "lexer-inserted "
        else
            ""

    compile_error(
        description = $"Encountered unexpected {virtualErrorDescriptionPart}token: {nextToken._text}",
        position = currentTextPosition state
    )

let peekExpectedTokenAfterTrivia (state: ParseState) (expectedTokenType: token_type): Option<token> * ParseState =
    let (optionNextToken, state) = peekTokenAfterTrivia state

    match optionNextToken with
    | Some nextToken ->
        if nextToken._type = expectedTokenType then
            (Some nextToken, state)
        else
            let error = getUnexpectedTokenError state expectedTokenType nextToken

            (None, { state with Errors = state.Errors @ [error] })
    | None -> (None, state)

let tryPeekExpectedToken (state: ParseState) (tokenType: token_type): Option<token> =
    if (state.NextTokenIndex < state.Tokens.Length) && (state.Tokens[state.NextTokenIndex]._type = tokenType) then
        Some state.Tokens[state.NextTokenIndex]
    else
        None

let peekToken (state: ParseState): Option<token> * ParseState =
    let optionNextToken = tryPeekToken state

    match optionNextToken with
    | Some nextToken ->
        (optionNextToken, state)
    | None ->
        let error = compile_error(
            description = "Unexpectedly reached the end of the tokens.",
            position = currentTextPosition state
        )

        (
            optionNextToken,
            { state with Errors = List.append state.Errors [error] }
        )

let readToken (state: ParseState): Option<token> * ParseState =
    let (optionNextToken, state) = peekToken state

    match optionNextToken with
    | Some nextToken -> (optionNextToken, { state with NextTokenIndex = state.NextTokenIndex + 1 })
    | None -> (optionNextToken, state)

let readExpectedToken (state: ParseState) (expectedTokenType: token_type): Option<token> * ParseState =
    let (optionNextToken, state) = peekToken state

    match optionNextToken with
    | Some nextToken ->
        if nextToken._type = expectedTokenType then
            (optionNextToken, { state with NextTokenIndex = state.NextTokenIndex + 1 })
        else
            let error = getUnexpectedTokenError state expectedTokenType nextToken

            (None, { state with Errors = state.Errors @ [error] })
    | None -> (optionNextToken, state)

let parseToken (state: ParseState) (tokenType: token_type): Option<ParseNode> * ParseState =
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
                        | token_type.whitespace -> ParseNodeType.Whitespace
                        | token_type.comment -> ParseNodeType.Comment
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
    (tokenType: token_type)
    (accumulator: List<ParseNode>):
    List<ParseNode> * ParseState =
        let optionNextToken = tryPeekToken state

        match optionNextToken with
        | Some nextToken ->
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
                        
                        parseSeparatedByToken state parseFn tokenType accumulator
                    | None -> (accumulator, state)
                | None -> (accumulator, state)
            | None -> (accumulator, state)
        | None -> (accumulator, state)

let rec parseSeparatedByWhitespace
    (state: ParseState)
    (parseFn: (ParseState -> Option<ParseNode> * ParseState))
    (optionTerminatorTokenType: Option<token_type>)
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
    let (optionIdentifier, state) = parseToken state token_type.identifier

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
    let (optionIdentifier, state) = parseToken state token_type.identifier

    match optionIdentifier with
    | Some identifier ->
        let (whitespace1, state) = parseTrivia state
        let (optionColon, state) = parseToken state token_type.colon

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
    let (optionIdentifier, state) = parseToken state token_type.identifier

    match optionIdentifier with
    | Some identifier ->
        let (whitespace1, state) = parseTrivia state
        let (optionColon, state) = parseToken state token_type.colon

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
    let (optionRecordToken, state) = parseToken state token_type.record_keyword

    match optionRecordToken with
    | Some recordToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionLeftCurlyBracket, state) = parseToken state token_type.left_curly_bracket

        match optionLeftCurlyBracket with
        | Some leftCurlyBracket -> 
            let (whitespace2, state) = parseTrivia state
            let (fields, state) = parseSeparatedByWhitespace state parseRecordField (Some token_type.right_curly_bracket) []
            let (whitespace3, state) = parseTrivia state
            let (optionRightCurlyBracket, state) = parseToken state token_type.right_curly_bracket

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
    let (optionIdentifier, state) = parseToken state token_type.identifier

    match optionIdentifier with
    | Some identifier ->
        (
            Some {
                Type = ParseNodeType.UnionCase
                Children = List.concat [ [identifier] ]
                Token = None
            },
            state
        )
    | None -> (None, state)

and parseUnion (state: ParseState): Option<ParseNode> * ParseState =
    let (optionUnionToken, state) = parseToken state token_type.union_keyword

    match optionUnionToken with
    | Some unionToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionLeftCurlyBracket, state) = parseToken state token_type.left_curly_bracket

        match optionLeftCurlyBracket with
        | Some leftCurlyBracket -> 
            let (whitespace2, state) = parseTrivia state
            let (cases, state) = parseSeparatedByWhitespace state parseUnionCase (Some token_type.right_curly_bracket) []
            let (whitespace3, state) = parseTrivia state
            let (optionRightCurlyBracket, state) = parseToken state token_type.right_curly_bracket

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

and parseIfThenElse (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIfToken, state) = parseToken state token_type.if_keyword

    match optionIfToken with
    | Some ifToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionIfExpression, state) = parseExpression state

        match optionIfExpression with
        | Some ifExpression ->
            let (whitespace2, state) = parseTrivia state
            let (optionThenToken, state) = parseToken state token_type.then_keyword

            match optionThenToken with
            | Some thenToken ->
                let (whitespace3, state) = parseTrivia state
                let (optionThenExpression, state) = parseExpression state

                match optionThenExpression with
                | Some thenExpression ->
                    let (whitespace4, state) = parseTrivia state
                    let (optionElseToken, state) = parseToken state token_type.else_keyword

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
    if (tryPeekTokenSequenceIgnoreTrivia state [token_type.identifier; token_type.equals]) then
        parseBinding state
    else
        parseExpression state

and parseBlock (state: ParseState): Option<ParseNode> * ParseState =
    let (optionLeftCurlyBracket, state) = parseToken state token_type.left_curly_bracket

    match optionLeftCurlyBracket with
    | Some leftCurlyBracket ->
        let (whitespace1, state) = parseTrivia state
        let (children, state) = parseSeparatedByWhitespace state parseBindingOrExpression (Some token_type.right_curly_bracket) []
        let (optionRightCurlyBracket, state) = parseToken state token_type.right_curly_bracket

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

and parseFunctionReturnType (state: ParseState): List<ParseNode> * ParseState =
    // read whitespace, then the colon, then more whitespace, then parse an expression
    let (whitespace1, state) = parseTrivia state
    let (optionColon, state) = parseToken state token_type.colon

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

and parseFunction (state: ParseState): Option<ParseNode> * ParseState =
    let (optionFnToken, state) = parseToken state token_type.fn_keyword

    match optionFnToken with
    | Some fnToken ->
        let (whitespace1, state) = parseTrivia state
        let (optionNextToken, state) = peekToken state

        match optionNextToken with
        | Some nextToken ->
            let (typeParametersNodes, state) =
                if nextToken._type = token_type.left_square_bracket then
                    let (optionLessThan, state) = parseToken state token_type.left_square_bracket
                    
                    match optionLessThan with
                    | Some lessThan ->
                        let (whitespace2, state) = parseTrivia state
                        let (typeParameters, state) = parseSeparatedByToken state parseTypeParameter token_type.comma []
                        let (whitespace3, state) = parseTrivia state
                        let (optionGreaterThan, state) = parseToken state token_type.right_square_bracket

                        match optionGreaterThan with
                        | Some greaterThan ->
                            let (trailingWhiteSpace, state) = parseTrivia state

                            ((List.concat [ [lessThan]; whitespace2; typeParameters; whitespace3; [greaterThan]; trailingWhiteSpace ]), state)
                        | None -> ([], state)
                    | None -> ([], state)
                else
                    ([], state)

            let (optionLeftParen, state) = parseToken state token_type.left_paren

            match optionLeftParen with
            | Some leftParen ->
                let (whitespace4, state) = parseTrivia state
                let (parameters, state) = parseSeparatedByToken state parseParameter token_type.comma []
                let (whitespace5, state) = parseTrivia state
                let (optionRightParen, state) = parseToken state token_type.right_paren

                // If next token is ':', then parse it & a return type expression.
                let (optionColonToken, state) = peekExpectedTokenAfterTrivia state token_type.colon
                let (returnTypeNodes, state) =
                    match optionColonToken with
                    | Some _ -> parseFunctionReturnType state
                    | None -> ([], state)

                match optionRightParen with
                | Some rightParen ->
                    let (whitespace6, state) = parseTrivia state
                    let (optionMinus, state) = parseToken state token_type.minus

                    match optionMinus with
                    | Some minus ->
                        let (optionGreaterThan, state) = parseToken state token_type.greater_than

                        match optionGreaterThan with
                        | Some greaterThan ->
                            let (whitespace7, state) = parseTrivia state
                            let (optionResult, state) = parseExpression state

                            match optionResult with
                            | Some result ->
                                (
                                    Some {
                                        Type = ParseNodeType.Function
                                        Children = List.concat [ [fnToken]; whitespace1; typeParametersNodes; [leftParen]; whitespace4; parameters; whitespace5; [rightParen]; returnTypeNodes; whitespace6; [minus]; [greaterThan]; whitespace7; [result] ]
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
            | token_type.left_square_bracket ->
                let (optionLeftSquareBracket, state) = parseToken state token_type.left_square_bracket
                match optionLeftSquareBracket with
                | Some leftSquareBracket ->
                    let (whitespace1, state) = parseTrivia state
                    let (typeArguments, state) = parseSeparatedByToken state parseTypeArgument token_type.comma []
                    let (whitespace2, state) = parseTrivia state
                    let (optionGreaterThan, state) = parseToken state token_type.right_square_bracket
                    match optionGreaterThan with
                    | Some greaterThan ->
                        let (trailingWhiteSpace, state) = parseTrivia state
                    
                        (List.concat [ [leftSquareBracket]; whitespace1; typeArguments; whitespace2; [greaterThan]; trailingWhiteSpace ], state)
                    | None -> ([], state)
                | None -> ([], state)
            | _ -> ([], state)
        | None -> ([], state)

    let (optionLeftParen, state) = parseToken state token_type.left_paren

    match optionLeftParen with
    | Some leftParen ->
        let (whitespace1, state) = parseTrivia state
        let (arguments, state) = parseSeparatedByToken state parseExpression token_type.comma []
        let (whitespace2, state) = parseTrivia state
        let (optionRightParen, state) = parseToken state token_type.right_paren

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
            | token_type.left_square_bracket ->
                let (optionLeftSquareBracket, state) = parseToken state token_type.left_square_bracket
                match optionLeftSquareBracket with
                | Some leftSquareBracket ->
                    let (whitespace1, state) = parseTrivia state
                    let (typeArguments, state) = parseSeparatedByToken state parseTypeArgument token_type.comma []
                    let (whitespace2, state) = parseTrivia state
                    let (optionGreaterThan, state) = parseToken state token_type.right_square_bracket
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

and parseMemberAccess (state: ParseState) (prefixExpression: ParseNode) (rightBindingPower: int): Option<ParseNode> * ParseState =
    let (optionPeriod, state) = parseToken state token_type.period

    match optionPeriod with
    | Some period ->
        let (whitespace1, state) = parseTrivia state
        let (optionIdentifier, state) = parseToken state token_type.identifier

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
        | token_type.fn_keyword ->
            wrapInExpressionNode (parseFunction state)
        | token_type.record_keyword ->
            wrapInExpressionNode (parseRecord state)
        | token_type.union_keyword ->
            wrapInExpressionNode (parseUnion state)
        | token_type.if_keyword ->
            wrapInExpressionNode (parseIfThenElse state)
        | token_type.left_curly_bracket ->
            wrapInExpressionNode (parseBlock state)
        | token_type.identifier ->
            wrapInExpressionNode (parseToken state token_type.identifier)
        | token_type.number_literal ->
            wrapInExpressionNode (parseToken state token_type.number_literal)
        | token_type.character_literal ->
            wrapInExpressionNode (parseToken state token_type.character_literal)
        | _ ->
            let error = compile_error(
                description = $"Encountered unexpected token: {nextToken._text}",
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

and getPostfixLeftBindingPower (tokenType: token_type): Option<int> =
    match tokenType with
    | token_type.left_paren -> Some 12
    | token_type.left_square_bracket -> Some 12
    | _ -> None

and getInfixBindingPowers (tokenType: token_type): Option<int * int> =
    match tokenType with
    | token_type.period -> Some (13, 14)
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
                if nextToken._type = token_type.left_paren then
                    let (whitespace, state) = parseTrivia state
                    let prefixExpression = { prefixExpression with Children = prefixExpression.Children @ whitespace }

                    let (optionFunctionCall, state) = parseFunctionCall state prefixExpression

                    match optionFunctionCall with
                    | Some functionCall ->
                        tryParsePostfixAndInfixExpressions state functionCall minBindingPower
                    | None ->
                        (None, state)
                else if nextToken._type = token_type.left_square_bracket then
                    let (whitespace, state) = parseTrivia state
                    let prefixExpression = { prefixExpression with Children = prefixExpression.Children @ whitespace }

                    // parse type arguments
                    let (optionGenericInstantiation, state) = parseGenericInstantiation state prefixExpression

                    match optionGenericInstantiation with
                    | Some genericInstantiation ->
                        tryParsePostfixAndInfixExpressions state genericInstantiation minBindingPower
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
                    match nextToken._type with
                    | token_type.period ->
                        let (whitespace, state) = parseTrivia state
                        let prefixExpression = { prefixExpression with Children = prefixExpression.Children @ whitespace }

                        let (optionMemberAccess, state) = parseMemberAccess state prefixExpression rightBindingPower

                        match optionMemberAccess with
                        | Some memberAccess ->
                            tryParsePostfixAndInfixExpressions state memberAccess minBindingPower
                        | None -> (None, state)
                    | _ -> failwith "Assert failed"
            | None -> (Some prefixExpression, state)
    | None -> (Some prefixExpression, state)

and parseBinding (state: ParseState): Option<ParseNode> * ParseState =
    let (optionIdentifier, state) = parseToken state token_type.identifier

    match optionIdentifier with
    | Some identifier ->
        let (whitespace1, state) = parseTrivia state
        let (optionEqualsSign, state) = parseToken state token_type.equals
        
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

let parse (tokens: List<token>): ParseOutput =
    let seedState: ParseState = {
        Tokens = tokens
        NextTokenIndex = 0
        Errors = []
    }
    let (compilationUnit, finalState) = parseCompilationUnit seedState

    { CompilationUnit = compilationUnit; Errors = finalState.Errors }
