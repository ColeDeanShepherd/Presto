using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;
    
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {

public static void main(Presto.Runtime.Console console) {print_line(console, "Hello, world!");}

public static void Main(string[] args)
        {
            Presto.Runtime.Console console = new();

            main(console);
        }
        }