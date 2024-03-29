using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;
    
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {

public enum token_type{identifier, number_literal, fn_keyword, record_keyword, union_keyword, if_keyword, then_keyword, else_keyword, whitespace, equals, minus, less_than, greater_than, left_paren, right_paren, left_curly_bracket, right_curly_bracket, left_square_bracket, right_square_bracket, character_literal, comma, colon, period};
public record token(token_type _type, string _text, text_position position, bool was_inserted);
public record tokenize_output(List<token> tokens, List<compile_error> errors);
public record tokenize_state(string text_left, text_position position, List<nat> indentation_stack, List<token> tokens, List<compile_error> errors);
public static bool is_done(tokenize_state state) {return eq(length(state.text_left), 0u);}
public static bool is_not_done(tokenize_state state) {return not(is_done(state));}
public static nat get_current_indentation(tokenize_state state) {return last_or_default<nat>(state.indentation_stack, 0u);}

}