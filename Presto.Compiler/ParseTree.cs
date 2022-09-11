namespace Presto.ParseTree;

/*
grammar Presto;
program: (expression ';')*
expression:
    string_literal
    | identifier
    | call_expression
    | member_access_operator
call_expression: expression '(' (expression ',')* expression ')'
member_access_operator: expression '.' expression
identifier: [_0-9a-zA-Z]+
string_literal: '"' [^"]* '"'
*/

public interface IExpression { }

public record Program(List<IExpression> Expressions);

public record CallExpression(
    IExpression FunctionExpression,
    List<IExpression> Arguments
) : IExpression;

public record MemberAccessOperator(
    IExpression Expression,
    IExpression Member
) : IExpression;

public record Identifier(
    string Text
) : IExpression;

public record StringLiteral(
    string Value
) : IExpression;