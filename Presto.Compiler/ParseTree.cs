namespace Presto.ParseTree;

/*
grammar Presto;
program: (statement ';')*
statement:
    let_statement
    | expression
let_statement: "let" identifier ':' qualified_name '=' expression
expression:
    string_literal
    | identifier
    | call_expression
    | member_access_operator
call_expression: expression '(' (expression ',')* expression ')'
member_access_operator: expression '.' expression
qualified_name: (identifier '.')* identifier
identifier: [_0-9a-zA-Z]+
string_literal: '"' [^"]* '"'
*/

public interface IStatement { }

public interface IExpression : IStatement { }

public record Program(List<IStatement> Statements);

public record LetStatement(
    Identifier VariableName,
    Identifier TypeName,
    IExpression Value
) : IStatement;

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