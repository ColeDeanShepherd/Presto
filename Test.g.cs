using System;
    using System.Collections.Generic;
    
    using nat = System.UInt32;

public record text_position(nat line_index, nat column_index);
public record compile_error(string description, text_position position);
static nat fact(nat n) {return ((eq(n, 0)) ? (1) : (product(n, fact(difference(n, 1)))));}

