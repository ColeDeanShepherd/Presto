using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;

public enum token_type{identifier, number_literal, fn_keyword, record_keyword, union_keyword, if_keyword, then_keyword, else_keyword, whitespace, equals, minus, greater_than, left_paren, right_paren, left_curly_bracket, right_curly_bracket, comma, colon, period};
public record token(token_type _type, string _text, text_position position, bool was_inserted);
public record tokenize_output(List<token> tokens, List<compile_error> errors);

