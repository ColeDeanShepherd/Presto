module Lexer

open System
open CompilerCore

type token = {
    Type: token_type
    Text: string
    Position: text_position
    WasInserted: bool
}

type TokenizeOutput = {
    Tokens: List<token>
    Errors: List<compile_error>
}

type TokenizeState = {
    TextLeft: string
    Position: text_position
    IndentationStack: List<int>
    Tokens: List<token>
    Errors: List<compile_error>
}

let getCurrentIndentation (state: TokenizeState) = 
    if state.IndentationStack.IsEmpty then
        0
    else state.IndentationStack[state.IndentationStack.Length - 1]

let isDone (state: TokenizeState) = state.TextLeft.Length = 0
let isNotDone (state: TokenizeState) = not (isDone state)

let advanceTextPosition (position: text_position) (readChar: char): text_position =
    if readChar <> '\n' then
        text_position(line_index = position.line_index, column_index = position.column_index + 1u)
    else
        text_position(line_index = position.line_index + 1u, column_index = 0u)

let tryPeekExpectedChar (state: TokenizeState) (expectedChar: char): Option<char> =
    if isNotDone state then
        let nextChar = state.TextLeft[0]

        if nextChar = expectedChar then
            Some nextChar
        else None
    else None

let peekChar (state: TokenizeState): char =
    if isNotDone state then state.TextLeft[0]
    else failwith "Unexpectedly reached the end of the source code."

let tryReadChar (state: TokenizeState): Option<char> * TokenizeState = 
    if isDone state then (None, state)
    else
        let nextChar = state.TextLeft[0]
        (
            Some(nextChar),
            {
                state with
                    TextLeft = state.TextLeft.Substring(1)
                    Position = advanceTextPosition state.Position nextChar
            }
        )

let readChar (state: TokenizeState): char * TokenizeState =
    let (maybeNextChar, nextState) = tryReadChar state

    (maybeNextChar.Value, nextState)

let rec appendCharsWhile (state: TokenizeState) (predicate: char -> bool) (str: string): string * TokenizeState =
    if isDone state then (str, state)
    else
        let nextChar = peekChar state

        if predicate nextChar then
            let (nextChar, nextState) = readChar state

            appendCharsWhile nextState predicate (str + nextChar.ToString())
        else (str, state)

let takeCharsWhile (state: TokenizeState) (predicate: char -> bool): string * TokenizeState =
    appendCharsWhile state predicate ""

let readSingleCharToken (state: TokenizeState) (tokenType: token_type): TokenizeState =
    let startPosition = state.Position
    let (nextChar, state) = readChar state
    
    { state with Tokens = state.Tokens @ [{ Type = tokenType; Text = nextChar.ToString(); Position = startPosition; WasInserted = false }] }

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

let readWhitespaceTokens (state: TokenizeState): List<token> * TokenizeState =
    let startPosition = state.Position
    let (tokenText, state) = takeCharsWhile state (fun c -> (c <> '\r') && (c <> '\n') && (isValidWhitespace c))
    let (optionLineBreak, state) = readLineBreak state

    let tokenText =
        match optionLineBreak with
        | Some lineBreak -> tokenText + lineBreak
        | None -> tokenText

    let optionWhitespaceToken =
        if tokenText.Length > 0 then
            Some { Type = token_type.whitespace; Text = tokenText; Position = startPosition; WasInserted = false }
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
                | Some whitespaceToken -> whitespaceToken.Text.Length
                | None -> 0
            let indentationIncreased = newIndentation > (getCurrentIndentation state)
            let indentationDecreased = newIndentation < (getCurrentIndentation state)

            if indentationIncreased then
                (
                    tokens @ [{ Type = token_type.left_curly_bracket; Text = "{"; Position = state.Position; WasInserted = true }],
                    { state with IndentationStack = state.IndentationStack @ [newIndentation] }
                )
            else if indentationDecreased then
                let (tokens, state) = (
                    tokens @ [{ Type = token_type.right_curly_bracket; Text = "}"; Position = state.Position; WasInserted = true }],
                    { state with IndentationStack = List.take (state.IndentationStack.Length - 1) state.IndentationStack }
                )

                if newIndentation > (getCurrentIndentation state) then
                    (
                        tokens @ [{ Type = token_type.left_curly_bracket; Text = "{"; Position = state.Position; WasInserted = true }],
                        { state with IndentationStack = state.IndentationStack @ [newIndentation] }
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
        let state = { state with Tokens = state.Tokens @ tokens }
        readWhitespace state
    else
        state

let isIdentifierChar (c: char) = (Char.IsLetter c) || (c = '_')

let iterateTokenize (state: TokenizeState): TokenizeState =
    if state.TextLeft.Length = 0 then state
    else
        let nextChar = state.TextLeft[0]
        let startPosition = state.Position
        
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

            { nextState with Tokens = nextState.Tokens @ [{ Type = tokenType; Text = tokenText; Position = startPosition; WasInserted = false }] }
        else if Char.IsDigit nextChar then
            let (tokenText, nextState) = takeCharsWhile state Char.IsDigit
            let tokenType = token_type.number_literal

            { nextState with Tokens = nextState.Tokens @ [{ Type = tokenType; Text = tokenText; Position = startPosition; WasInserted = false }] }
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
            { state with TextLeft = state.TextLeft[1..]; Errors = state.Errors @ [compile_error(description = $"Encountered an unexpected character: '{nextChar}'", position = state.Position)] }

let tokenize (sourceCode: string): TokenizeOutput =
    let seedState: TokenizeState = {
        TextLeft = sourceCode
        Position = text_position(line_index = 0u, column_index = 0u)
        IndentationStack = []
        Tokens = []
        Errors = []
    }
    let finalState = applyWhile iterateTokenize isNotDone seedState

    let finalState =
        if finalState.IndentationStack.Length > 0
        then
            let token = { Type = token_type.right_curly_bracket; Text = "}"; Position = finalState.Position; WasInserted = true }

            {
                finalState with
                    IndentationStack = []
                    Tokens = finalState.Tokens @ (List.replicate finalState.IndentationStack.Length token)
            }
        else
            finalState

    { Tokens = finalState.Tokens; Errors = finalState.Errors }