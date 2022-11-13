using System.Text;

namespace Presto.ParseTree;

public interface IParseTreeNode { }

public record ParseTreeNode(
    string GrammarRuleName,
    List<IParseTreeNode> Children
) : IParseTreeNode;

public record TerminalParseTreeNode(
    Token token
) : IParseTreeNode;

public static class ParseTreeNodeHelpers
{
    public static string PrintTree(IParseTreeNode node)
    {
        StringBuilder sb = new();

        void Print(IParseTreeNode node, uint indentationLevel)
        {
            PrintIndentation(indentationLevel);

            if (node is ParseTreeNode ptn)
            {
                sb.AppendLine(ptn.GrammarRuleName);

                indentationLevel++;
                foreach (var child in ptn.Children)
                {
                    Print(child, indentationLevel);
                }
            }
            else if (node is TerminalParseTreeNode tptn)
            {
                sb.AppendLine(tptn.token.Text);
            }
            else
            {
                throw new NotImplementedException();
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