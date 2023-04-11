using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;
    
    public static partial class PrestoProgram {
        static bool eq<T>(T a, T b) => (a != null) ? a.Equals(b) : (b == null);
        static bool not(bool x) => !x;

public enum token_type{identifier, number_literal, fn_keyword, record_keyword, union_keyword, if_keyword, then_keyword, else_keyword, whitespace, equals, minus, greater_than, left_paren, right_paren, left_curly_bracket, right_curly_bracket, comma, colon, period};

}