using System.Collections.Generic;

namespace Presto.AST
{
    public interface IAstNode
    {
        void Accept(AstNodeVisitor visitor);
    }
    public interface IDefinition: IAstNode { }
    public interface IExpression : IAstNode { }
    public interface IStatement : IAstNode { }

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

    public class FunctionDefinition : IDefinition
    {
        public Identifier Name;
        public Block Body;

        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }

    public class Program : IDefinition
    {
        public List<IDefinition> Definitions;
        
        public void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
}
