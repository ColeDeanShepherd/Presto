using System.Security.AccessControl;
using System.Security.Cryptography.X509Certificates;

namespace Presto.ParseTree;

/*
grammar Presto;
program: (statement ';')*
statement:
    let_statement
    | struct_declaration
    | expression
let_statement: "let" identifier ':' qualified_name '=' expression
struct_declaration: "struct" identifier '{' ((field_declaration ',')* field_declaration)? '}'
field_declaration: identifier ':' qualified_name
expression:
      number
    | string_literal
    | identifier
    | call_expression
    | member_access_operator
call_expression: expression '(' ((expression ',')* expression)? ')'
member_access_operator: expression '.' expression
qualified_name: (identifier '.')* identifier
identifier: [_0-9a-zA-Z]+
number: [0-9]+
string_literal: '"' [^"]* '"'
*/

public interface INode { }

public interface IStatement : INode { }

public interface IExpression : IStatement { }

public record Program(List<IStatement> Statements);

public record LetStatement(
    Identifier VariableName,
    QualifiedName TypeName,
    IExpression Value
) : IStatement;

public record StructDefinition(
    Identifier StructName,
    List<FieldDeclaration> FieldDeclarations
) : IStatement;

public record FieldDeclaration(
    Identifier FieldName,
    QualifiedName TypeName
);

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

public record QualifiedName(
    List<Identifier> Identifiers
) : IExpression;

public record NumberLiteral(
    string Text
) : IExpression;

public record StringLiteral(
    string Value
) : IExpression;