using Presto.AST;
using System;
using System.Collections.Generic;

namespace Presto
{
    class Program
    {
        static void Main(string[] args)
        {
            var printHelloWorld = new FunctionCall
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
                Arguments = new List<IExpression>
                {
                    new StringLiteral
                    {
                        Value = "Hello, world!"
                    }
                }
            };
            var ast = new AST.Program
            {
                Definitions = new List<IDefinition>
                {
                    new FunctionDefinition
                    {
                        Name = new Identifier
                        {
                            Text = "main"
                        },
                        Body = new Block
                        {
                            Statements = new List<IStatement>
                            {
                                printHelloWorld
                            }
                        }
                    }
                }
            };

            var codeGenerator = new CCodeGenerator();
            codeGenerator.Visit(ast);

            Console.WriteLine(codeGenerator.GeneratedCode);
            Console.ReadKey();
        }
    }
}
