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

        private StringBuilder stringBuilder = new StringBuilder();
    }

    public abstract class AstNode
    {
        public abstract void Accept(AstNodeVisitor visitor);
    }
    public abstract class Expression : AstNode
    {
    }

    // All nodes must derive from AstNote.
    #region Literals

    public class IntegerLiteral : Expression
    {
        public int Value;

        public override void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
    public class StringLiteral : Expression
    {
        public string Value;

        public override void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }

    #endregion

    public class Identifier : Expression
    {
        public string Text;

        public override void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }

    public class MemberAccessOperator : Expression
    {
        public Expression MemberContainer;
        public Identifier MemberIdentifier;

        public override void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }

    public class FunctionCall : Expression
    {
        public Expression FunctionExpression;
        public List<Expression> Arguments;

        public override void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
}
