module Lexer

open System
open CompilerCore

open type PrestoProgram

let isTriviaToken (tokenType: token_type): bool =
    (tokenType = token_type.whitespace) || (tokenType = token_type.comment)

let listAppend (list: ResizeArray<'a>) (e: 'a): ResizeArray<'a> =
    let newList = ResizeArray<'a> list
    newList.Add(e)
    newList

let listAppendRange (a: ResizeArray<'a>) (b: ResizeArray<'a>): ResizeArray<'a> =
    let newList = ResizeArray<'a> a
    newList.AddRange(b)
    newList

let advance_text_position (position: text_position) (readChar: char): text_position =
    if readChar <> '\n' then
        text_position(file_name = position.file_name, line_index = position.line_index, column_index = position.column_index + 1u)
    else
        text_position(file_name = position.file_name, line_index = position.line_index + 1u, column_index = 0u)

let tryPeekExpectedChar (state: tokenize_state) (expectedChar: char): Option<char> =
    if is_not_done state then
        let nextChar = state.text_left[0]

        if nextChar = expectedChar then
            Some nextChar
        else None
    else None

let isNextCharEqualTo (state: tokenize_state) (expectedChar: char): bool =
    match (tryPeekExpectedChar state expectedChar) with
    | Some _ -> true
    | None -> false

let tryPeekChar (state: tokenize_state): Option<char> =
    if is_not_done state then Some state.text_left[0]
    else None

let peekChar (state: tokenize_state): char =
    if is_not_done state then state.text_left[0]
    else failwith "Unexpectedly reached the end of the source code."

let tryLookaheadChar (state: tokenize_state) (numTokensToSkip: int): Option<char> =
    if numTokensToSkip < state.text_left.Length then
        Some state.text_left[numTokensToSkip]
    else
        None

let tryReadChar (state: tokenize_state): Option<char> * tokenize_state = 
    if is_done state then (None, state)
    else
        let nextChar = state.text_left[0]
        (
            Some(nextChar),
            tokenize_state(
                tokens = state.tokens,
                errors = state.errors,
                indentation_stack = state.indentation_stack,
                text_left = state.text_left.Substring(1),
                position = advance_text_position state.position nextChar
            )
        )

let readChar (state: tokenize_state): char * tokenize_state =
    let (maybeNextChar, nextState) = tryReadChar state

    (maybeNextChar.Value, nextState)

let readExpectedChar (state: tokenize_state) (expectedChar: char): Option<char> * tokenize_state =
    let (nextChar, state) = readChar state

    if nextChar = expectedChar then
        (Some nextChar, state)
    else
        let state = tokenize_state(
            tokens = state.tokens,
            errors = listAppend state.errors (compile_error(description = $"Expected '{expectedChar}' but encountered '{nextChar}'", position = state.position)),
            indentation_stack = state.indentation_stack,
            text_left = state.text_left,
            position = state.position
        )

        (None, state)

let rec appendCharsWhile (state: tokenize_state) (predicate: char -> bool) (str: string): string * tokenize_state =
    if is_done state then (str, state)
    else
        let nextChar = peekChar state

        if predicate nextChar then
            let (nextChar, nextState) = readChar state

            appendCharsWhile nextState predicate (str + nextChar.ToString())
        else (str, state)

let takeCharsWhile (state: tokenize_state) (predicate: char -> bool): string * tokenize_state =
    appendCharsWhile state predicate ""

let rec appendCharsWhileWithIndex (state: tokenize_state) (predicate: int -> char -> bool) (str: string) (charIndex: int): string * tokenize_state =
    if is_done state then (str, state)
    else
        let nextChar = peekChar state

        if predicate charIndex nextChar then
            let (nextChar, nextState) = readChar state

            appendCharsWhileWithIndex nextState predicate (str + nextChar.ToString()) (charIndex + 1)
        else (str, state)

let takeCharsWhileWithIndex (state: tokenize_state) (predicate: int -> char -> bool): string * tokenize_state =
    appendCharsWhileWithIndex state predicate "" 0

let readSingleCharToken (state: tokenize_state) (tokenType: token_type): tokenize_state =
    let startPosition = state.position
    let (nextChar, state) = readChar state
    
    tokenize_state(
        tokens = listAppend state.tokens (token(_type = tokenType, _text = nextChar.ToString(), position = startPosition, was_inserted = false)),
        errors = state.errors,
        indentation_stack = state.indentation_stack,
        text_left = state.text_left,
        position = state.position
    )

let readLineBreak (state: tokenize_state): Option<string> * tokenize_state =
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

let readWhitespaceTokens (state: tokenize_state): List<token> * tokenize_state =
    let startPosition = state.position
    let (tokenText, state) = takeCharsWhile state (fun c -> (c <> '\r') && (c <> '\n') && (isValidWhitespace c))
    let (optionLineBreak, state) = readLineBreak state

    let tokenText =
        match optionLineBreak with
        | Some lineBreak -> tokenText + lineBreak
        | None -> tokenText

    let optionWhitespaceToken =
        if tokenText.Length > 0 then
            Some (token(_type = token_type.whitespace, _text = tokenText, position = startPosition, was_inserted = false))
        else
            None

    let tokens: List<token> =
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
                | Some whitespaceToken -> uint whitespaceToken._text.Length
                | None -> 0u
            let indentationIncreased = newIndentation > (get_current_indentation state)
            let indentationDecreased = newIndentation < (get_current_indentation state)

            if indentationIncreased then
                (
                    tokens @ [token(_type = token_type.left_curly_bracket, _text = "{", position = state.position, was_inserted = true)],
                    tokenize_state(
                        tokens = state.tokens,
                        errors = state.errors,
                        indentation_stack = listAppend state.indentation_stack newIndentation,
                        text_left = state.text_left,
                        position = state.position
                    )
                )
            else if indentationDecreased then
                let (tokens, state) = (
                    tokens @ [token(_type = token_type.right_curly_bracket, _text = "}", position = state.position, was_inserted = true)],
                    tokenize_state(
                        tokens = state.tokens,
                        errors = state.errors,
                        indentation_stack = ResizeArray<uint> (Seq.take (state.indentation_stack.Count - 1) state.indentation_stack),
                        text_left = state.text_left,
                        position = state.position
                    )
                )

                if newIndentation > (get_current_indentation state) then
                    (
                        tokens @ [token(_type = token_type.left_curly_bracket, _text = "{", position = state.position, was_inserted = true)],
                        tokenize_state(
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

let rec readWhitespace (state: tokenize_state): tokenize_state =
    let (tokens, state) = readWhitespaceTokens state

    if not tokens.IsEmpty then
        let state = tokenize_state(
            tokens = listAppendRange state.tokens (ResizeArray<token> tokens),
            errors = state.errors,
            indentation_stack = state.indentation_stack,
            text_left = state.text_left,
            position = state.position
        )

        readWhitespace state
    else
        state

let rec readComment (state: tokenize_state): tokenize_state =
    let startPosition = state.position

    let (_, state) = readExpectedChar state '#'
    let (tokenText, state) = takeCharsWhile state (fun c -> (c <> '\r') && (c <> '\n'))

    tokenize_state(
        tokens = listAppend state.tokens (token(_type = token_type.comment, _text = "#" + tokenText, position = startPosition, was_inserted = false)),
        errors = state.errors,
        indentation_stack = state.indentation_stack,
        text_left = state.text_left,
        position = state.position
    )

let isIdentifierChar (i: int) (c: char) = (Char.IsLetter c) || (c = '_') || ((i > 0) && Char.IsAsciiDigit c)

let iterateTokenize (state: tokenize_state): tokenize_state =
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
                | "fn" -> token_type.fn_keyword
                | "record" -> token_type.record_keyword
                | "union" -> token_type.union_keyword
                | "if" -> token_type.if_keyword
                | "then" -> token_type.then_keyword
                | "else" -> token_type.else_keyword
                | _ -> token_type.identifier

            let nextToken = token(_type = tokenType, _text = tokenText, position = startPosition, was_inserted = false)

            tokenize_state(
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

            let tokenType = token_type.number_literal

            let nextToken = token(_type = tokenType, _text = tokenText, position = startPosition, was_inserted = false)

            tokenize_state(
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
                tokenize_state(
                    tokens = listAppend state.tokens (token(_type = token_type.equality_operator, _text = "==", position = startPosition, was_inserted = false)),
                    errors = state.errors,
                    indentation_stack = state.indentation_stack,
                    text_left = state.text_left.Substring(2),
                    position = state.position
                )
            | _ -> readSingleCharToken state token_type.equals
        else if nextChar = '-' then
            readSingleCharToken state token_type.minus
        else if nextChar = '<' then
            readSingleCharToken state token_type.less_than
        else if nextChar = '>' then
            readSingleCharToken state token_type.greater_than
        else if nextChar = '(' then
            readSingleCharToken state token_type.left_paren
        else if nextChar = ')' then
            readSingleCharToken state token_type.right_paren
        else if nextChar = '{' then
            readSingleCharToken state token_type.left_curly_bracket
        else if nextChar = '}' then
            readSingleCharToken state token_type.right_curly_bracket
        else if nextChar = '[' then
            readSingleCharToken state token_type.left_square_bracket
        else if nextChar = ']' then
            readSingleCharToken state token_type.right_square_bracket
        else if nextChar = ',' then
            readSingleCharToken state token_type.comma
        else if nextChar = ':' then
            readSingleCharToken state token_type.colon
        else if nextChar = '.' then
            readSingleCharToken state token_type.period
        else if nextChar = '+' then
            readSingleCharToken state token_type.plus_sign
        else if nextChar = '*' then
            readSingleCharToken state token_type.asterisk
        else if nextChar = '/' then
            readSingleCharToken state token_type.forward_slash
        else if nextChar = ''' then
            let (_, state) = readExpectedChar state '\''
            let (charText, state) = takeCharsWhile state (fun x -> x <> '\'')
            let (_, state) = readExpectedChar state '\''

            // TODO: don't allow too many chars, newlines
            let nextToken = token(_type = token_type.character_literal, _text = "'" + charText + "'", position = startPosition, was_inserted = false)

            tokenize_state(
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
            let nextToken = token(_type = token_type.string_literal, _text = "\"" + charText + "\"", position = startPosition, was_inserted = false)

            tokenize_state(
                tokens = listAppend state.tokens nextToken,
                errors = state.errors,
                indentation_stack = state.indentation_stack,
                text_left = state.text_left,
                position = state.position
            )
        else
            tokenize_state(
                tokens = state.tokens,
                errors = listAppend state.errors (compile_error(description = $"Encountered an unexpected character: '{nextChar}'", position = state.position)),
                indentation_stack = state.indentation_stack,
                text_left = state.text_left[1..],
                position = state.position
            )

let tokenize (fileName: string) (sourceCode: string): tokenize_output =
    let seedState = tokenize_state(
        tokens = ResizeArray<token>(),
        errors = ResizeArray<compile_error>(),
        indentation_stack = ResizeArray<uint>(),
        text_left = sourceCode,
        position = text_position(file_name = fileName, line_index = 0u, column_index = 0u)
    )
    let finalState = applyWhile iterateTokenize is_not_done seedState

    let finalState =
        if finalState.indentation_stack.Count > 0
        then
            let token = token(_type = token_type.right_curly_bracket, _text = "}", position = finalState.position, was_inserted = true)

            tokenize_state(
                tokens = listAppendRange finalState.tokens (ResizeArray<token> (List.replicate finalState.indentation_stack.Count token)),
                errors = finalState.errors,
                indentation_stack = ResizeArray<uint>(),
                text_left = finalState.text_left,
                position = finalState.position
            )
        else
            finalState

    tokenize_output(tokens = (ResizeArray<token> finalState.tokens), errors = (ResizeArray<compile_error> finalState.errors))