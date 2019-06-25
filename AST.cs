using System.Collections.Generic;
using System.Text;

namespace Presto
{
    public abstract class AstNodeVisitor
    {
        public abstract void Visit(IntegerLiteral integerLiteral);
        public abstract void Visit(StringLiteral stringLiteral);

        public abstract void Visit(Identifier identifier);

        public abstract void Visit(FunctionCall functionCall);
        public abstract void Visit(MemberAccessOperator memberAccessOperator);

        public abstract void Visit(Block block);
        public abstract void Visit(FunctionDefinition functionDefinition);
    }

    public class PrestoCodeGenerator : AstNodeVisitor
    {
        public string GeneratedCode => stringBuilder.ToString();

        public override void Visit(IntegerLiteral integerLiteral)
        {
            stringBuilder.Append(integerLiteral.Value);
        }

        public override void Visit(StringLiteral stringLiteral)
        {
            stringBuilder.Append('"');
            stringBuilder.Append(stringLiteral.Value);
            stringBuilder.Append('"');
        }

        public override void Visit(Identifier identifier)
        {
            stringBuilder.Append(identifier.Text);
        }

        public override void Visit(FunctionCall functionCall)
        {
            functionCall.FunctionExpression.Accept(this);
            stringBuilder.Append('(');

            for (var i = 0; i < functionCall.Arguments.Count; i++)
            {
                if (i > 0)
                {
                    stringBuilder.Append(", ");
                }

                functionCall.Arguments[i].Accept(this);
            }

            stringBuilder.Append(')');
        }

        public override void Visit(MemberAccessOperator memberAccessOperator)
        {
            memberAccessOperator.MemberContainer.Accept(this);
            stringBuilder.Append('.');
            memberAccessOperator.MemberIdentifier.Accept(this);
        }

        public override void Visit(Block block)
        {
            stringBuilder.Append('{');
            
            foreach (var statement in block.Statements)
            {
                statement.Accept(this);
                stringBuilder.Append(';');
            }

            stringBuilder.Append('}');
        }

        public override void Visit(FunctionDefinition functionDefinition)
        {
            stringBuilder.Append("fn");
            stringBuilder.Append(' ');
            functionDefinition.Name.Accept(this);
            stringBuilder.Append('(');
            stringBuilder.Append(')');
            functionDefinition.Body.Accept(this);
        }

        private StringBuilder stringBuilder = new StringBuilder();
    }

    public interface IAstNode
    {
        void Accept(AstNodeVisitor visitor);
    }
    public interface IExpression : IAstNode
    {
    }
    public interface IStatement : IAstNode
    {
    }

    // All nodes must derive from AstNote.
    #region Literals

    public class IntegerLiteral : IExpression
    {
        public int Value;

        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
    public class StringLiteral : IExpression
    {
        public string Value;

        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }

    #endregion

    public class Identifier : IExpression
    {
        public string Text;

        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }

    public class MemberAccessOperator : IExpression
    {
        public IExpression MemberContainer;
        public Identifier MemberIdentifier;

        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
    public class FunctionCall : IExpression, IStatement
    {
        public IExpression FunctionExpression;
        public List<IExpression> Arguments;

        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }

    public class Block : IStatement
    {
        public List<IStatement> Statements;

        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }

    public class FunctionDefinition : IStatement
    {
        public Identifier Name;
        public Block Body;

        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
}
