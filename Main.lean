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

structure TokenizeOutput where
  tokens: List Token
  errors: List TokenizeError

structure TokenizeState where
  textLeft: String
  tokens: List Token
  errors: List TokenizeError

def isDone (state: TokenizeState) := state.textLeft.isEmpty
def isNotDone (state: TokenizeState) := not (isDone state)

partial def applyWhile (fn: (a -> a)) (predicate: (a -> Bool)) (startValue: a): a :=
  if predicate startValue then
    applyWhile fn predicate (fn startValue)
  else
    startValue

-- Good candidate for refinement type - textLeft isn't empty
def iterateTokenize (state: TokenizeState): TokenizeState :=
  if state.textLeft.isEmpty then state
  else
    let nextChar := state.textLeft.front
    
    if nextChar.isAlpha then
      state
    else if nextChar.isWhitespace then
    else if nextChar == '=' then
    else if nextChar == '-' then
    else if nextChar == '>' then
    else -- error

def tokenize (sourceCode: String): TokenizeOutput :=
  let seedState: TokenizeState := { textLeft := sourceCode, tokens := [], errors := [] }
  let finalState := applyWhile iterateTokenize isNotDone seedState
  { tokens := finalState.tokens, errors := finalState.errors }

def main: IO Unit := do
  let sourceCode <- IO.FS.readFile fileName
  let tokenizeOutput := tokenize sourceCode
  IO.println sourceCode
