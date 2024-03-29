using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;
    
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {

public record text_position(nat line_index, nat column_index);
public record compile_error(string description, text_position position);

}