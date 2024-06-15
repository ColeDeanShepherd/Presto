using System;
    using System.Collections.Generic;
    
    using Nat = System.UInt32;
    using Real = System.Decimal;
    
    using Presto.Runtime;
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {

public record TextPosition(string file_path, Nat line_index, Nat column_index);
public record CompileError(string description, TextPosition position);

}