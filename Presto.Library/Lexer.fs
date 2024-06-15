module Lexer

open System
open CompilerCore

open type PrestoProgram

let autoInsertCurlyBraces = false

let isTriviaToken (tokenType: TokenType): bool =
    (tokenType = TokenType.whitespace) || (tokenType = TokenType.comment)

let listAppend (list: ResizeArray<'a>) (e: 'a): ResizeArray<'a> =
    let newList = ResizeArray<'a> list
    newList.Add(e)
    newList

let listAppendRange (a: ResizeArray<'a>) (b: ResizeArray<'a>): ResizeArray<'a> =
    let newList = ResizeArray<'a> a
    newList.AddRange(b)
    newList

let advance_text_position (position: TextPosition) (readChar: char): TextPosition =
    if readChar <> '\n' then
        TextPosition(file_path = position.file_path, line_index = position.line_index, column_index = position.column_index + 1u)
    else
        TextPosition(file_path = position.file_path, line_index = position.line_index + 1u, column_index = 0u)

let tryPeekExpectedChar (state: TokenizeState) (expectedChar: char): Option<char> =
    if is_not_done state then
        let nextChar = state.text_left[0]

        if nextChar = expectedChar then
            Some nextChar
        else None
    else None

let isNextCharEqualTo (state: TokenizeState) (expectedChar: char): bool =
    match (tryPeekExpectedChar state expectedChar) with
    | Some _ -> true
    | None -> false

let tryPeekChar (state: TokenizeState): Option<char> =
    if is_not_done state then Some state.text_left[0]
    else None

let peekChar (state: TokenizeState): char =
    if is_not_done state then state.text_left[0]
    else failwith "Unexpectedly reached the end of the source code."

let tryLookaheadChar (state: TokenizeState) (numTokensToSkip: int): Option<char> =
    if numTokensToSkip < state.text_left.Length then
        Some state.text_left[numTokensToSkip]
    else
        None

let tryReadChar (state: TokenizeState): Option<char> * TokenizeState = 
    if is_done state then (None, state)
    else
        let nextChar = state.text_left[0]
        (
            Some(nextChar),
            TokenizeState(
                tokens = state.tokens,
                errors = state.errors,
                indentation_stack = state.indentation_stack,
                text_left = state.text_left.Substring(1),
                position = advance_text_position state.position nextChar
            )
        )

let readChar (state: TokenizeState): char * TokenizeState =
    let (maybeNextChar, nextState) = tryReadChar state

    (maybeNextChar.Value, nextState)

let readExpectedChar (state: TokenizeState) (expectedChar: char): Option<char> * TokenizeState =
    let (nextChar, state) = readChar state

    if nextChar = expectedChar then
        (Some nextChar, state)
    else
        let state = TokenizeState(
            tokens = state.tokens,
            errors = listAppend state.errors (CompileError(description = $"Expected '{expectedChar}' but encountered '{nextChar}'", position = state.position)),
            indentation_stack = state.indentation_stack,
            text_left = state.text_left,
            position = state.position
        )

        (None, state)

let rec appendCharsWhile (state: TokenizeState) (predicate: char -> bool) (str: string): string * TokenizeState =
    if is_done state then (str, state)
    else
        let nextChar = peekChar state

        if predicate nextChar then
            let (nextChar, nextState) = readChar state

            appendCharsWhile nextState predicate (str + nextChar.ToString())
        else (str, state)

let takeCharsWhile (state: TokenizeState) (predicate: char -> bool): string * TokenizeState =
    appendCharsWhile state predicate ""

let rec appendCharsWhileWithIndex (state: TokenizeState) (predicate: int -> char -> bool) (str: string) (charIndex: int): string * TokenizeState =
    if is_done state then (str, state)
    else
        let nextChar = peekChar state

        if predicate charIndex nextChar then
            let (nextChar, nextState) = readChar state

            appendCharsWhileWithIndex nextState predicate (str + nextChar.ToString()) (charIndex + 1)
        else (str, state)

let takeCharsWhileWithIndex (state: TokenizeState) (predicate: int -> char -> bool): string * TokenizeState =
    appendCharsWhileWithIndex state predicate "" 0

let readSingleCharToken (state: TokenizeState) (tokenType: TokenType): TokenizeState =
    let startPosition = state.position
    let (nextChar, state) = readChar state
    
    TokenizeState(
        tokens = listAppend state.tokens (Token(_type = tokenType, text = nextChar.ToString(), position = startPosition, was_inserted = false)),
        errors = state.errors,
        indentation_stack = state.indentation_stack,
        text_left = state.text_left,
        position = state.position
    )

let readLineBreak (state: TokenizeState): Option<string> * TokenizeState =
    let optionCarriageReturn = tryPeekExpectedChar state '\r'

    let (lineBreak, state) =
        match optionCarriageReturn with
        | Some carriageReturn ->
            let (carriageReturn, state) = readChar state
            (carriageReturn.ToString(), state)
        | None -> ("", state)

    let optionLineFeed = tryPeekExpectedChar state '\n'

    let (lineBreak, state) =
        match optionLineFeed with
        | Some lineFeed ->
            let (lineFeed, state) = readChar state
            (lineBreak + lineFeed.ToString(), state)
        | None -> (lineBreak, state)

    if lineBreak.Length > 0 then
        (Some lineBreak, state)
    else
        (None, state)

let isValidWhitespace (c: char): bool = (Char.IsWhiteSpace c) && (c <> '\t')

// TODO: make sure this works with blank line with & without line break at end of file

let readWhitespaceTokens (state: TokenizeState): List<Token> * TokenizeState =
    let startPosition = state.position
    let (tokenText, state) = takeCharsWhile state (fun c -> (c <> '\r') && (c <> '\n') && (isValidWhitespace c))
    let (optionLineBreak, state) = readLineBreak state

    let tokenText =
        match optionLineBreak with
        | Some lineBreak -> tokenText + lineBreak
        | None -> tokenText

    let optionWhitespaceToken =
        if tokenText.Length > 0 then
            Some (Token(_type = TokenType.whitespace, text = tokenText, position = startPosition, was_inserted = false))
        else
            None

    let tokens: List<Token> =
        match optionWhitespaceToken with
        | Some whitespaceToken -> [whitespaceToken]
        | None -> []
        
    let isStartOfLine = startPosition.column_index = 0u
    let readEndOfLine = optionLineBreak.IsSome
    let isIndentation = isStartOfLine && not readEndOfLine

    let (tokens, state) =
        if isIndentation then
            let newIndentation =
                match optionWhitespaceToken with
                | Some whitespaceToken -> uint whitespaceToken.text.Length
                | None -> 0u
            let indentationIncreased = newIndentation > (get_current_indentation state)
            let indentationDecreased = newIndentation < (get_current_indentation state)

            if indentationIncreased then
                (
                    if autoInsertCurlyBraces then
                        tokens @ [Token(_type = TokenType.left_curly_bracket, text = "{", position = state.position, was_inserted = true)]
                    else
                        tokens
                    ,
                    TokenizeState(
                        tokens = state.tokens,
                        errors = state.errors,
                        indentation_stack = listAppend state.indentation_stack newIndentation,
                        text_left = state.text_left,
                        position = state.position
                    )
                )
            else if indentationDecreased then
                let (tokens, state) = (
                    if autoInsertCurlyBraces then
                        tokens @ [Token(_type = TokenType.right_curly_bracket, text = "}", position = state.position, was_inserted = true)]
                    else
                        tokens
                    ,
                    TokenizeState(
                        tokens = state.tokens,
                        errors = state.errors,
                        indentation_stack = ResizeArray<uint> (Seq.take (state.indentation_stack.Count - 1) state.indentation_stack),
                        text_left = state.text_left,
                        position = state.position
                    )
                )

                if newIndentation > (get_current_indentation state) then
                    (
                        if autoInsertCurlyBraces then
                            tokens @ [Token(_type = TokenType.left_curly_bracket, text = "{", position = state.position, was_inserted = true)]
                        else
                            tokens
                        ,
                        TokenizeState(
                            tokens = state.tokens,
                            errors = state.errors,
                            indentation_stack = listAppend state.indentation_stack newIndentation,
                            text_left = state.text_left,
                            position = state.position
                        )
                    )
                else
                    (tokens, state)
            else
                (tokens, state)
        else
            (tokens, state)

    (tokens, state)

let rec readWhitespace (state: TokenizeState): TokenizeState =
    let (tokens, state) = readWhitespaceTokens state

    if not tokens.IsEmpty then
        let state = TokenizeState(
            tokens = listAppendRange state.tokens (ResizeArray<Token> tokens),
            errors = state.errors,
            indentation_stack = state.indentation_stack,
            text_left = state.text_left,
            position = state.position
        )

        readWhitespace state
    else
        state

let rec readComment (state: TokenizeState): TokenizeState =
    let startPosition = state.position

    let (_, state) = readExpectedChar state '#'
    let (tokenText, state) = takeCharsWhile state (fun c -> (c <> '\r') && (c <> '\n'))

    TokenizeState(
        tokens = listAppend state.tokens (Token(_type = TokenType.comment, text = "#" + tokenText, position = startPosition, was_inserted = false)),
        errors = state.errors,
        indentation_stack = state.indentation_stack,
        text_left = state.text_left,
        position = state.position
    )

let isIdentifierChar (i: int) (c: char) = (Char.IsLetter c) || (c = '_') || ((i > 0) && Char.IsAsciiDigit c)

let iterateTokenize (state: TokenizeState): TokenizeState =
    if state.text_left.Length = 0 then state
    else
        let nextChar = state.text_left[0]
        let startPosition = state.position
        
        if isValidWhitespace nextChar then
            readWhitespace state
        else if nextChar = '#' then
            readComment state
        else if isIdentifierChar 0 nextChar then
            let (tokenText, nextState) = takeCharsWhileWithIndex state isIdentifierChar
            let tokenType =
                match tokenText with
                | "fn" -> TokenType.fn_keyword
                | "record" -> TokenType.record_keyword
                | "trait" -> TokenType.trait_keyword
                | "union" -> TokenType.union_keyword
                | "if" -> TokenType.if_keyword
                | "then" -> TokenType.then_keyword
                | "else" -> TokenType.else_keyword
                | _ -> TokenType.identifier

            let nextToken = Token(_type = tokenType, text = tokenText, position = startPosition, was_inserted = false)

            TokenizeState(
                tokens = listAppend nextState.tokens nextToken,
                errors = nextState.errors,
                indentation_stack = nextState.indentation_stack,
                text_left = nextState.text_left,
                position = nextState.position
            )
        else if Char.IsDigit nextChar then
            let (tokenText, nextState) = takeCharsWhile state Char.IsDigit

            let (tokenText, nextState) =
                if isNextCharEqualTo nextState '.' then
                    let (_, nextState) = readExpectedChar nextState '.'
                    let (tokenTextAfterDecimal, nextState) = takeCharsWhile nextState Char.IsDigit

                    (tokenText + "." + tokenTextAfterDecimal, nextState)
                else
                    (tokenText, nextState)

            let tokenType = TokenType.number_literal

            let nextToken = Token(_type = tokenType, text = tokenText, position = startPosition, was_inserted = false)

            TokenizeState(
                tokens = listAppend nextState.tokens nextToken,
                errors = nextState.errors,
                indentation_stack = nextState.indentation_stack,
                text_left = nextState.text_left,
                position = nextState.position
            )
        else if nextChar = '=' then
            let optionNextNextChar = tryLookaheadChar state 1

            match optionNextNextChar with
            | Some nextNextChar when nextNextChar = '=' -> 
                TokenizeState(
                    tokens = listAppend state.tokens (Token(_type = TokenType.equality_operator, text = "==", position = startPosition, was_inserted = false)),
                    errors = state.errors,
                    indentation_stack = state.indentation_stack,
                    text_left = state.text_left.Substring(2),
                    position = state.position
                )
            | _ -> readSingleCharToken state TokenType.equals
        else if nextChar = '-' then
            let optionNextNextChar = tryLookaheadChar state 1

            match optionNextNextChar with
            | Some nextNextChar when nextNextChar = '>' -> 
                TokenizeState(
                    tokens = listAppend state.tokens (Token(_type = TokenType.right_arrow, text = "->", position = startPosition, was_inserted = false)),
                    errors = state.errors,
                    indentation_stack = state.indentation_stack,
                    text_left = state.text_left.Substring(2),
                    position = state.position
                )
            | _ -> readSingleCharToken state TokenType.minus
        else if nextChar = '<' then
            readSingleCharToken state TokenType.less_than
        else if nextChar = '>' then
            let optionNextNextChar = tryLookaheadChar state 1

            match optionNextNextChar with
            | Some nextNextChar when nextNextChar = '>' -> 
                TokenizeState(
                    tokens = listAppend state.tokens (Token(_type = TokenType.double_greater_than, text = ">>", position = startPosition, was_inserted = false)),
                    errors = state.errors,
                    indentation_stack = state.indentation_stack,
                    text_left = state.text_left.Substring(2),
                    position = state.position
                )
            | _ -> readSingleCharToken state TokenType.greater_than
        else if nextChar = '(' then
            readSingleCharToken state TokenType.left_paren
        else if nextChar = ')' then
            readSingleCharToken state TokenType.right_paren
        else if nextChar = '{' then
            readSingleCharToken state TokenType.left_curly_bracket
        else if nextChar = '}' then
            readSingleCharToken state TokenType.right_curly_bracket
        else if nextChar = '[' then
            readSingleCharToken state TokenType.left_square_bracket
        else if nextChar = ']' then
            readSingleCharToken state TokenType.right_square_bracket
        else if nextChar = ',' then
            readSingleCharToken state TokenType.comma
        else if nextChar = ':' then
            readSingleCharToken state TokenType.colon
        else if nextChar = '.' then
            readSingleCharToken state TokenType.period
        else if nextChar = '+' then
            readSingleCharToken state TokenType.plus_sign
        else if nextChar = '*' then
            readSingleCharToken state TokenType.asterisk
        else if nextChar = '?' then
            readSingleCharToken state TokenType.question_mark
        else if nextChar = '/' then
            readSingleCharToken state TokenType.forward_slash
        else if nextChar = ''' then
            let (_, state) = readExpectedChar state '\''
            let (charText, state) = takeCharsWhile state (fun x -> x <> '\'')
            let (_, state) = readExpectedChar state '\''

            // TODO: don't allow too many chars, newlines
            let nextToken = Token(_type = TokenType.character_literal, text = "'" + charText + "'", position = startPosition, was_inserted = false)

            TokenizeState(
                tokens = listAppend state.tokens nextToken,
                errors = state.errors,
                indentation_stack = state.indentation_stack,
                text_left = state.text_left,
                position = state.position
            )
        else if nextChar = '"' then
            let (_, state) = readExpectedChar state '"'
            let (charText, state) = takeCharsWhile state (fun x -> x <> '"')
            let (_, state) = readExpectedChar state '"'

            // TODO: how to handle newlines?
            let nextToken = Token(_type = TokenType.string_literal, text = "\"" + charText + "\"", position = startPosition, was_inserted = false)

            TokenizeState(
                tokens = listAppend state.tokens nextToken,
                errors = state.errors,
                indentation_stack = state.indentation_stack,
                text_left = state.text_left,
                position = state.position
            )
        else
            TokenizeState(
                tokens = state.tokens,
                errors = listAppend state.errors (CompileError(description = $"Encountered an unexpected character: '{nextChar}'", position = state.position)),
                indentation_stack = state.indentation_stack,
                text_left = state.text_left[1..],
                position = state.position
            )

let tokenize (fileName: string) (sourceCode: string): TokenizeOutput =
    let seedState = TokenizeState(
        tokens = ResizeArray<Token>(),
        errors = ResizeArray<CompileError>(),
        indentation_stack = ResizeArray<uint>(),
        text_left = sourceCode,
        position = TextPosition(file_path = fileName, line_index = 0u, column_index = 0u)
    )
    let finalState = applyWhile iterateTokenize is_not_done seedState

    let finalState =
        if autoInsertCurlyBraces && finalState.indentation_stack.Count > 0
        then
            let token = Token(_type = TokenType.right_curly_bracket, text = "}", position = finalState.position, was_inserted = true)

            TokenizeState(
                tokens = listAppendRange finalState.tokens (ResizeArray<Token> (List.replicate finalState.indentation_stack.Count token)),
                errors = finalState.errors,
                indentation_stack = ResizeArray<uint>(),
                text_left = finalState.text_left,
                position = finalState.position
            )
        else
            finalState

    TokenizeOutput(tokens = (ResizeArray<Token> finalState.tokens), errors = (ResizeArray<CompileError> finalState.errors))