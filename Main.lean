def fileName := "BootstrappedCompiler.pst"

structure TextPosition where
  lineIndex: Int
  columnIndex: Int

inductive TokenType where
  | Identifier: TokenType
  | Whitespace: TokenType
  | Equals: TokenType
  | Minus: TokenType
  | GreaterThan: TokenType

structure Token where
  type: TokenType
  text: String
  position: TextPosition

structure TokenizeError where
  description: String
  position: TextPosition

structure TokenizeOutput where
  tokens: List Token
  errors: List TokenizeError

structure TokenizeState where
  textLeft: String
  position: TextPosition
  tokens: List Token
  errors: List TokenizeError

def isDone (state: TokenizeState) := state.textLeft.isEmpty
def isNotDone (state: TokenizeState) := not (isDone state)

partial def applyWhile (fn: (a -> a)) (predicate: (a -> Bool)) (startValue: a): a :=
  if predicate startValue then
    applyWhile fn predicate (fn startValue)
  else
    startValue

def advanceTextPosition (position: TextPosition) (readChar: Char): TextPosition :=
  if readChar != '\n' then
    {
      lineIndex := position.lineIndex,
      columnIndex := position.columnIndex + 1
    }
  else
    {
      lineIndex := position.lineIndex + 1,
      columnIndex := 0
    }

def peekChar (state: TokenizeState): Char :=
  if isNotDone state then state.textLeft.front
  else panic! "Unexpectedly reached the end of the source code."

def readChar? (state: TokenizeState): Prod (Option Char) TokenizeState := 
  if isDone state then (none, state)
  else
    let nextChar := state.textLeft.front
    (
      nextChar,
      {
        state with
        textLeft := state.textLeft.drop 1,
        position := advanceTextPosition state.position nextChar
      }
    )

def readChar (state: TokenizeState): Prod Char TokenizeState :=
  let (nextChar?, nextState) := readChar? state

  (nextChar?.get!, nextState)

def appendCharsWhile (state: TokenizeState) (predicate: Char -> Bool) (str: String): Prod String TokenizeState :=
  if isDone state then (str, state)
  else
    let nextChar := peekChar state

    if predicate nextChar then
      let (nextChar, nextState) := readChar state

      appendCharsWhile nextState predicate (str.append nextChar.toString)
    else (str, state)

def takeCharsWhile (state: TokenizeState) (predicate: Char -> Bool): Prod String TokenizeState :=
  appendCharsWhile state predicate ""

def readSingleCharToken (state: TokenizeState) (tokenType: TokenType): TokenizeState :=
  let startPosition := state.position
  let (nextChar, nextState) := readChar state
  
  { nextState with tokens := nextState.tokens.concat { type := tokenType, text := nextChar.toString, position := startPosition } }

-- Good candidate for refinement type - textLeft isn't empty
def iterateTokenize (state: TokenizeState): TokenizeState :=
  if state.textLeft.isEmpty then state
  else
    let nextChar := state.textLeft.front
    let startPosition := state.position
    
    if nextChar.isAlpha then
      let (tokenText, nextState) := takeCharsWhile state Char.isAlpha
      { nextState with tokens := nextState.tokens.concat { type := TokenType.Identifier, text := tokenText, position := startPosition } }
    else if nextChar.isWhitespace then
      let (tokenText, nextState) := takeCharsWhile state Char.isWhitespace
      { nextState with tokens := nextState.tokens.concat { type := TokenType.Whitespace, text := tokenText, position := startPosition } }
    else if nextChar == '=' then
      readSingleCharToken state TokenType.Equals
    else if nextChar == '-' then
      readSingleCharToken state TokenType.Minus
    else if nextChar == '>' then
      readSingleCharToken state TokenType.GreaterThan
    else
      { state with errors := state.errors.concat { description := s!"Encountered an unexpected character: {nextChar}", position := state.position } }
  
def tokenize (sourceCode: String): TokenizeOutput :=
  let seedState: TokenizeState := {
    textLeft := sourceCode,
    position := { columnIndex := 0, lineIndex := 0 },
    tokens := [],
    errors := []
  }
  let finalState := applyWhile iterateTokenize isNotDone seedState
  { tokens := finalState.tokens, errors := finalState.errors }

def main: IO Unit := do
  let sourceCode <- IO.FS.readFile fileName
  let tokenizeOutput := tokenize sourceCode

  IO.println "Errors:"
  IO.println (tokenizeOutput.errors.map (fun x => x.description))

  IO.println "Tokens:"
  IO.println (tokenizeOutput.tokens.map (fun t => t.text))
