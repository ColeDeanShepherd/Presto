using System;
using System.Collections.Generic;

namespace Presto
{
    class Program
    {
        static void Main(string[] args)
        {
            var ast = new FunctionCall
            {
                FunctionExpression = new MemberAccessOperator
                {
                    MemberContainer = new MemberAccessOperator
                    {
                        MemberContainer = new MemberAccessOperator
                        {
                            MemberContainer = new Identifier
                            {
                                Text = "std"
                            },
                            MemberIdentifier = new Identifier
                            {
                                Text = "io"
                            }
                        },
                        MemberIdentifier = new Identifier
                        {
                            Text = "stdout"
                        }
                    },
                    MemberIdentifier = new Identifier
                    {
                        Text = "writeLine"
                    }
                },
                Arguments = new List<Expression>
                {
                    new StringLiteral
                    {
                        Value = "Hello, world!"
                    }
                }
            };
            var prestoCodeGenerator = new PrestoCodeGenerator();
            prestoCodeGenerator.Visit(ast);

            Console.WriteLine(prestoCodeGenerator.GeneratedCode);
            Console.ReadKey();
        }
    }
}
