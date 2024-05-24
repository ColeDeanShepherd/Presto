using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;
    
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {

public static void no_op() {}
public static t identity<t>(t x) {return x;}

}