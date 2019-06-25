using System;
using System.Collections.Generic;

namespace Presto
{
    public abstract class AstNodeVisitor
    {
        public abstract void Visit(IntegerLiteral integerLiteral);
        public abstract void Visit(Identifier identifier);
        public abstract void Visit(FunctionCall node);
    }

    public abstract class AstNode
    {
        public abstract void Accept(AstNodeVisitor visitor);
    }
    public abstract class Expression : AstNode
    {
    }
    public class IntegerLiteral : Expression
    {
        public int Value;

        public override void Accept(AstNodeVisitor visitor)
        {
            visitor.Visit(this);
        }
    }
    public class Identifier : Expression
    {
        public string Text;

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

    class Program
    {
        static void Main(string[] args)
        {
            var ast = new FunctionCall
            {
                FunctionExpression = new Identifier
                {
                    Text = "add"
                },
                Arguments = new List<Expression>
                {
                    new IntegerLiteral
                    {
                        Value = 1
                    },
                    new IntegerLiteral
                    {
                        Value = 2
                    }
                }
            };

            Console.ReadKey();
        }
    }
}
