using Presto.AST;
using System.Text;

namespace Presto.ParseTree;

public interface IParseTreeNode { }

public record ParseTreeNode(
    List<IParseTreeNode> Children
) : IParseTreeNode;

public record TerminalParseTreeNode(
    Token Token
) : IParseTreeNode;

public record Program : ParseTreeNode
{
    public Program(List<IParseTreeNode> children) : base(children) { }

    public IEnumerable<Statement> Statements => this.GetChildrenOfType<Statement>();
}

public record Statement : ParseTreeNode
{
    public Statement(List<IParseTreeNode> children) : base(children) { }
}

public record LetStatement : ParseTreeNode
{
    public LetStatement(List<IParseTreeNode> children) : base(children) { }

    public TerminalParseTreeNode VariableName => this.GetChildrenOfType(TokenType.Identifier).First();
    public Expression Value => this.GetChildrenOfType<Expression>().First();
    public QualifiedName TypeName => this.GetChildrenOfType<QualifiedName>().First();
}

public record StructDeclaration : ParseTreeNode
{
    public StructDeclaration(List<IParseTreeNode> children) : base(children) { }

    public TerminalParseTreeNode StructName => this.GetChildrenOfType(TokenType.Identifier).First();
    public IEnumerable<FieldDeclaration> FieldDeclarations => this.GetChildrenOfType<FieldDeclaration>();
}

public record FieldDeclaration : ParseTreeNode
{
    public FieldDeclaration(List<IParseTreeNode> children) : base(children) { }

    public TerminalParseTreeNode FieldName => this.GetChildrenOfType(TokenType.Identifier).First();
    public QualifiedName TypeName => this.GetChildrenOfType<QualifiedName>().First();
}

public abstract record Expression : ParseTreeNode
{
    protected Expression(List<IParseTreeNode> Children) : base(Children)
    {
    }
}

public record CallExpression : Expression
{
    public CallExpression(List<IParseTreeNode> children) : base(children) { }

    public Expression FunctionExpression => this.GetChildrenOfType<Expression>().First();
    public IEnumerable<Expression> Arguments => this.GetChildrenOfType<Expression>().Skip(1);
}

public record MemberAccessOperator : Expression
{
    public MemberAccessOperator(List<IParseTreeNode> children) : base(children) { }

    public Expression Expression => this.GetChildrenOfType<Expression>().First();
    public Expression Member => this.GetChildrenOfType<Expression>().Skip(1).First();
}

public record QualifiedName : ParseTreeNode
{
    public QualifiedName(List<IParseTreeNode> children) : base(children) { }

    public IEnumerable<TerminalParseTreeNode> Identifiers => this.GetChildrenOfType(TokenType.Identifier);
}

public record Identifier : Expression
{
    public Identifier(string text) : base(new List<IParseTreeNode>())
    {
        Text = text;
    }

    public readonly string Text;
}

public record NumberLiteral : Expression
{
    public NumberLiteral(string value) : base(new List<IParseTreeNode>())
    {
        Value = value;
    }

    public readonly string Value;
}

public record StringLiteral : Expression
{
    public StringLiteral(string value) : base(new List<IParseTreeNode>())
    {
        Value = value;
    }

    public readonly string Value;
}

public static class ParseTreeNodeHelpers
{
    public static IEnumerable<T> GetChildrenOfType<T>(this IParseTreeNode node)
    {
        if (node is ParseTreeNode ptn)
        {
            foreach (var child in ptn.Children.OfType<T>())
            {
                yield return child;
            }
        }
    }

    public static IEnumerable<TerminalParseTreeNode> GetChildrenOfType(this IParseTreeNode node, TokenType type)
    {
        if (node is ParseTreeNode ptn)
        {
            foreach (var child in ptn.Children.Where(c => c is TerminalParseTreeNode).Cast<TerminalParseTreeNode>())
            {
                if (child.Token.Type == type)
                {
                    yield return child;
                }
            }
        }
    }

    public static string PrintTree(IParseTreeNode node)
    {
        StringBuilder sb = new();

        void Print(IParseTreeNode node, uint indentationLevel)
        {
            PrintIndentation(indentationLevel);

            if (node is ParseTreeNode ptn)
            {
                sb.AppendLine(ptn.GetType().Name);

                indentationLevel++;
                foreach (var child in ptn.Children)
                {
                    Print(child, indentationLevel);
                }
            }
            else if (node is TerminalParseTreeNode tptn)
            {
                sb.AppendLine(tptn.Token.Text);
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
