using System.Collections.Generic;

namespace Presto.AST
{
    public interface IAstNode
    {
        TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg);
    }
    public interface IDefinition: IAstNode { }
    public interface IExpression : IAstNode { }
    public interface IStatement : IAstNode { }

    // All nodes must derive from AstNote.
    #region Literals

    public class IntegerLiteral : IExpression
    {
        public int Value;

        public TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }
    }
    public class StringLiteral : IExpression
    {
        public string Value;

        public TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }
    }

    #endregion

    public class Identifier : IExpression
    {
        public string Text;

        public TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }
    }

    public class MemberAccessOperator : IExpression
    {
        public IExpression MemberContainer;
        public Identifier MemberIdentifier;

        public TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }
    }
    public class FunctionCall : IExpression, IStatement
    {
        public IExpression FunctionExpression;
        public List<IExpression> Arguments;

        public TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }
    }

    public class Block : IStatement
    {
        public List<IStatement> Statements;

        public TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }
    }

    public class FunctionDefinition : IDefinition
    {
        public Identifier Name;
        public Block Body;

        public TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }
    }

    public class Program
    {
        public List<IDefinition> Definitions;
        
        public TResult Accept<TArg, TResult>(AstNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }
    }
}
