using System;
    using System.Collections.Generic;
    
    using Nat = System.UInt32;
    using Real = System.Decimal;
    
    using Presto.Runtime;
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {

public static Unit no_op() {return __exec<Unit>(() => {
return Unit.Instance;
});}
public static Unit ignore<T>(T x) {return __exec<Unit>(() => {
return Unit.Instance;
});}
public static T identity<T>(T x) {return x;}
public static bool not_eq<T>(T a, T b) {return not(eq(a, b));}
public static bool is_true(bool x) {return x == true;}
public static bool is_false(bool x) {return x == false;}
public static bool not(bool x) {return ((x) ? (false) : (true));}
public static string concatenate(string a, string b) {return a + b;}

}