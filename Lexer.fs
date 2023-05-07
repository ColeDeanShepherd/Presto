module Lexer

open System
open CompilerCore

type tokenize_state = {
    text_left: string
    position: text_position
    indentation_stack: List<int>
    tokens: List<token>
    errors: List<compile_error>
}

let getCurrentIndentation (state: tokenize_state) = 
    if state.indentation_stack.IsEmpty then
        0
    else state.indentation_stack[state.indentation_stack.Length - 1]

let isDone (state: tokenize_state) = state.text_left.Length = 0
let isNotDone (state: tokenize_state) = not (isDone state)

let advanceTextPosition (position: text_position) (readChar: char): text_position =
    if readChar <> '\n' then
        text_position(line_index = position.line_index, column_index = position.column_index + 1u)
    else
        text_position(line_index = position.line_index + 1u, column_index = 0u)

let tryPeekExpectedChar (state: tokenize_state) (expectedChar: char): Option<char> =
    if isNotDone state then
        let nextChar = state.text_left[0]

        if nextChar = expectedChar then
            Some nextChar
        else None
    else None

let peekChar (state: tokenize_state): char =
    if isNotDone state then state.text_left[0]
    else failwith "Unexpectedly reached the end of the source code."

let tryReadChar (state: tokenize_state): Option<char> * tokenize_state = 
    if isDone state then (None, state)
    else
        let nextChar = state.text_left[0]
        (
            Some(nextChar),
            {
                state with
                    text_left = state.text_left.Substring(1)
                    position = advanceTextPosition state.position nextChar
            }
        )

let readChar (state: tokenize_state): char * tokenize_state =
    let (maybeNextChar, nextState) = tryReadChar state

    (maybeNextChar.Value, nextState)

let rec appendCharsWhile (state: tokenize_state) (predicate: char -> bool) (str: string): string * tokenize_state =
    if isDone state then (str, state)
    else
        let nextChar = peekChar state

        if predicate nextChar then
            let (nextChar, nextState) = readChar state

            appendCharsWhile nextState predicate (str + nextChar.ToString())
        else (str, state)

let takeCharsWhile (state: tokenize_state) (predicate: char -> bool): string * tokenize_state =
    appendCharsWhile state predicate ""

let readSingleCharToken (state: tokenize_state) (tokenType: token_type): tokenize_state =
    let startPosition = state.position
    let (nextChar, state) = readChar state
    
    { state with tokens = state.tokens @ [token(_type = tokenType, _text = nextChar.ToString(), position = startPosition, was_inserted = false)] }

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
                | Some whitespaceToken -> whitespaceToken._text.Length
                | None -> 0
            let indentationIncreased = newIndentation > (getCurrentIndentation state)
            let indentationDecreased = newIndentation < (getCurrentIndentation state)

            if indentationIncreased then
                (
                    tokens @ [token(_type = token_type.left_curly_bracket, _text = "{", position = state.position, was_inserted = true)],
                    { state with indentation_stack = state.indentation_stack @ [newIndentation] }
                )
            else if indentationDecreased then
                let (tokens, state) = (
                    tokens @ [token(_type = token_type.right_curly_bracket, _text = "}", position = state.position, was_inserted = true)],
                    { state with indentation_stack = List.take (state.indentation_stack.Length - 1) state.indentation_stack }
                )

                if newIndentation > (getCurrentIndentation state) then
                    (
                        tokens @ [token(_type = token_type.left_curly_bracket, _text = "{", position = state.position, was_inserted = true)],
                        { state with indentation_stack = state.indentation_stack @ [newIndentation] }
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
        let state = { state with tokens = state.tokens @ tokens }
        readWhitespace state
    else
        state

let isIdentifierChar (c: char) = (Char.IsLetter c) || (c = '_')

let iterateTokenize (state: tokenize_state): tokenize_state =
    if state.text_left.Length = 0 then state
    else
        let nextChar = state.text_left[0]
        let startPosition = state.position
        
        if isValidWhitespace nextChar then
            readWhitespace state
        else if isIdentifierChar nextChar then
            let (tokenText, nextState) = takeCharsWhile state isIdentifierChar
            let tokenType =
                match tokenText with
                | "fn" -> token_type.fn_keyword
                | "record" -> token_type.record_keyword
                | "union" -> token_type.union_keyword
                | "if" -> token_type.if_keyword
                | "then" -> token_type.then_keyword
                | "else" -> token_type.else_keyword
                | _ -> token_type.identifier

            { nextState with tokens = nextState.tokens @ [token(_type = tokenType, _text = tokenText, position = startPosition, was_inserted = false)] }
        else if Char.IsDigit nextChar then
            let (tokenText, nextState) = takeCharsWhile state Char.IsDigit
            let tokenType = token_type.number_literal

            { nextState with tokens = nextState.tokens @ [token(_type = tokenType, _text = tokenText, position = startPosition, was_inserted = false)] }
        else if nextChar = '=' then
            readSingleCharToken state token_type.equals
        else if nextChar = '-' then
            readSingleCharToken state token_type.minus
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
        else if nextChar = ',' then
            readSingleCharToken state token_type.comma
        else if nextChar = ':' then
            readSingleCharToken state token_type.colon
        else if nextChar = '.' then
            readSingleCharToken state token_type.period
        else
            { state with text_left = state.text_left[1..]; errors = state.errors @ [compile_error(description = $"Encountered an unexpected character: '{nextChar}'", position = state.position)] }

let tokenize (sourceCode: string): tokenize_output =
    let seedState: tokenize_state = {
        text_left = sourceCode
        position = text_position(line_index = 0u, column_index = 0u)
        indentation_stack = []
        tokens = []
        errors = []
    }
    let finalState = applyWhile iterateTokenize isNotDone seedState

    let finalState =
        if finalState.indentation_stack.Length > 0
        then
            let token = token(_type = token_type.right_curly_bracket, _text = "}", position = finalState.position, was_inserted = true)

            {
                finalState with
                    indentation_stack = []
                    tokens = finalState.tokens @ (List.replicate finalState.indentation_stack.Length token)
            }
        else
            finalState

    tokenize_output(tokens = (ResizeArray<token> finalState.tokens), errors = (ResizeArray<compile_error> finalState.errors))