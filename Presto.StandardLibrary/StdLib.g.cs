using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;
    using real = System.Double;
    
    using Presto.Runtime;
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {

public static Unit no_op() {return __exec<Unit>(() => {
return Unit.Instance;
});}
public static Unit ignore<t>(t x) {return __exec<Unit>(() => {
return Unit.Instance;
});}
public static t identity<t>(t x) {return x;}
public static bool not_eq<t>(t a, t b) {return not(eq(a, b));}
public static bool is_true(bool x) {return x == true;}
public static bool is_false(bool x) {return x == false;}
public static bool not(bool x) {return ((x) ? (false) : (true));}
public static string concatenate(string a, string b) {return a + b;}

}