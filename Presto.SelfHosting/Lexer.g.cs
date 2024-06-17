using System;
    using System.Collections.Generic;
    
    using Nat = System.UInt32;
    
    using static Presto.Runtime.PrestoProgram;
    
    public static partial class PrestoProgram {

public enum TokenType{identifier, number_literal, fn_keyword, record_keyword, union_keyword, trait_keyword, if_keyword, then_keyword, else_keyword, Self_keyword, whitespace, equals, minus, less_than, greater_than, left_paren, right_paren, left_curly_bracket, right_curly_bracket, left_square_bracket, right_square_bracket, character_literal, comma, colon, period, comment, string_literal, plus_sign, forward_slash, asterisk, equality_operator, right_arrow, question_mark, double_greater_than };
public record Token(TokenType _type, string text, TextPosition position, bool was_inserted);
public record TokenizeOutput(List<Token> tokens, List<CompileError> errors);
public record TokenizeState(string text_left, TextPosition position, List<Nat> indentation_stack, List<Token> tokens, List<CompileError> errors);
public static bool is_done(TokenizeState state) {return eq(length(state.text_left), 0u);}
public static bool is_not_done(TokenizeState state) {return not(is_done(state));}
public static Nat get_current_indentation(TokenizeState state) {return last_or_default<Nat>(state.indentation_stack, 0u);}

}