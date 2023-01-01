grammar Presto;

compilation_unit: binding*;
binding: identifier '=' expression;
expression: function;
function: 'fn' '(' ((parameter ',')* parameter)? ')' '->' expression;
parameter: identifier ':' expression ('[' (expression ',')* expression ']')?;
identifier: [_a-zA-Z][_0-9a-zA-Z]*;