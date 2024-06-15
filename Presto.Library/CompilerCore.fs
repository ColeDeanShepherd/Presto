module CompilerCore

//override this.ToString() = $"Ln {1 + this.LineIndex}, Col {1 + this.ColumnIndex}"
//override this.ToString() = $"{this.Position}: {this.Description}"

let rec applyWhile (fn: ('a -> 'a)) (predicate: ('a -> bool)) (startValue: 'a): 'a =
    if predicate startValue then
        applyWhile fn predicate (fn startValue)
    else
        startValue