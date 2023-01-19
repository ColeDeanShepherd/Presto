module Lexer

open System
open CompilerCore

type TokenType =
    | Identifier
    | NumberLiteral
    | FnKeyword
    | RecordKeyword
    | Whitespace
    | Equals
    | Minus
    | GreaterThan
    | LeftParen
    | RightParen
    | Comma
    | Colon
    | Period

type Token = {
    Type: TokenType
    Text: string
    Position: TextPosition
}

type TokenizeOutput = {
    Tokens: List<Token>
    Errors: List<CompileError>
}

type TokenizeState = {
    TextLeft: string
    Position: TextPosition
    Tokens: List<Token>
    Errors: List<CompileError>
}

let isDone (state: TokenizeState) = state.TextLeft.Length = 0
let isNotDone (state: TokenizeState) = not (isDone state)

let advanceTextPosition (position: TextPosition) (readChar: char): TextPosition =
    if readChar <> '\n' then
        {
            LineIndex = position.LineIndex
            ColumnIndex = position.ColumnIndex + 1
        }
    else
        {
            LineIndex = position.LineIndex + 1
            ColumnIndex = 0
        }

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

let readSingleCharToken (state: TokenizeState) (tokenType: TokenType): TokenizeState =
    let startPosition = state.Position
    let (nextChar, nextState) = readChar state
    
    { nextState with Tokens = nextState.Tokens @ [{ Type = tokenType; Text = nextChar.ToString(); Position = startPosition }] }

let iterateTokenize (state: TokenizeState): TokenizeState =
    if state.TextLeft.Length = 0 then state
    else
        let nextChar = state.TextLeft[0]
        let startPosition = state.Position
        
        if Char.IsWhiteSpace nextChar then
            let (tokenText, nextState) = takeCharsWhile state Char.IsWhiteSpace
            { nextState with Tokens = nextState.Tokens @ [{ Type = TokenType.Whitespace; Text = tokenText; Position = startPosition }] }
        else if Char.IsLetter nextChar then
            let (tokenText, nextState) = takeCharsWhile state Char.IsLetter
            let tokenType =
                match tokenText with
                | "fn" -> TokenType.FnKeyword
                | "record" -> TokenType.RecordKeyword
                | _ -> TokenType.Identifier

            { nextState with Tokens = nextState.Tokens @ [{ Type = tokenType; Text = tokenText; Position = startPosition }] }
        else if Char.IsDigit nextChar then
            let (tokenText, nextState) = takeCharsWhile state Char.IsDigit
            let tokenType = TokenType.NumberLiteral

            { nextState with Tokens = nextState.Tokens @ [{ Type = tokenType; Text = tokenText; Position = startPosition }] }
        else if nextChar = '=' then
            readSingleCharToken state TokenType.Equals
        else if nextChar = '-' then
            readSingleCharToken state TokenType.Minus
        else if nextChar = '>' then
            readSingleCharToken state TokenType.GreaterThan
        else if nextChar = '(' then
            readSingleCharToken state TokenType.LeftParen
        else if nextChar = ')' then
            readSingleCharToken state TokenType.RightParen
        else if nextChar = ',' then
            readSingleCharToken state TokenType.Comma
        else if nextChar = ':' then
            readSingleCharToken state TokenType.Colon
        else if nextChar = '.' then
            readSingleCharToken state TokenType.Period
        else
            { state with TextLeft = state.TextLeft[1..]; Errors = state.Errors @ [{ Description = $"Encountered an unexpected character: {nextChar}"; Position = state.Position }] }

let tokenize (sourceCode: string): TokenizeOutput =
    let seedState: TokenizeState = {
        TextLeft = sourceCode
        Position = { ColumnIndex = 0; LineIndex = 0 }
        Tokens = []
        Errors = []
    }
    let finalState = applyWhile iterateTokenize isNotDone seedState

    { Tokens = finalState.Tokens; Errors = finalState.Errors }