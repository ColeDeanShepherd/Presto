module CompilerCore

type TextPosition = {
    LineIndex: int
    ColumnIndex: int
}
with
    override this.ToString() = $"Ln {this.LineIndex}, Col {this.ColumnIndex}"
    
type CompileError = {
    Description: string
    Position: TextPosition
}
with
    override this.ToString() = $"{this.Position}: {this.Description}"