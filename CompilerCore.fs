module CompilerCore

type TextPosition = {
    LineIndex: int
    ColumnIndex: int
}
with
    override this.ToString() = $"Ln {1 + this.LineIndex}, Col {1 + this.ColumnIndex}"
    
type CompileError = {
    Description: string
    Position: TextPosition
}
with
    override this.ToString() = $"{this.Position}: {this.Description}"