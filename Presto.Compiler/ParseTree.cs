using Presto.Compiler;
using System.Text;

namespace Presto.ParseTree;

public record ParseTreeNode(
    IGrammarNode GrammarNode,
    List<ParseTreeNode> Children
);

public static class ParseTreeNodeHelpers
{
    public static string PrintTree(ParseTreeNode node)
    {
        StringBuilder sb = new();

        void Print(ParseTreeNode node, uint indentationLevel)
        {
            PrintIndentation(indentationLevel);
            sb.AppendLine(node.GrammarNode.GetType().Name);

            indentationLevel++;
            foreach (var child in node.Children)
            {
                Print(child, indentationLevel);
            }
        }

        void PrintIndentation(uint indentationLevel)
        {
            sb.Append(' ', (int)(2 * indentationLevel));
        }

        Print(node, 0);

        return sb.ToString();
    }
}

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

public record FunctionDefinition(
    Identifier FunctionName,
    List<ParameterDefinition> ParameterDefinitions,
    List<IStatement> Body
) : IStatement;

public record ParameterDefinition(
    Identifier FieldName,
    QualifiedName TypeName
);

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