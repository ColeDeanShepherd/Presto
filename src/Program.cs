using Presto.ASG;
using Presto.CodeGeneration;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Presto.Cli
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var program = CreateFibonacciProgram();

            // semantic analysis
            var semanticAnalysisErrors = SemanticAnalysis.Validate(program);

            if (semanticAnalysisErrors.Any())
            {
                Console.WriteLine($"Semantic analysis failed with {semanticAnalysisErrors.Count} errors.");

                foreach (var error in semanticAnalysisErrors)
                {
                    Console.WriteLine(error);
                }

                return;
            }

            // code gen
            Console.WriteLine("Generating C# program...");
            var (generatedCode, codeGenErrors) = CSharpCodeGenerator.GenerateCode(program);

            if (codeGenErrors.Any())
            {
                Console.WriteLine($"Code generation failed with {codeGenErrors.Count} errors.");

                foreach (var error in codeGenErrors)
                {
                    Console.WriteLine(error);
                }

                return;
            }

            // C# compilation
            Console.WriteLine("Compiling C# program...");
            CSharpCompiler.CompileCSharpProgram(generatedCode);

            // run executable
            Console.WriteLine("Starting compiled program...");
            var process = Process.Start("bin/PrestoProgram.exe");
            process.WaitForExit();
        }

        public static ASG.Program CreateFibonacciProgram()
        {
            var program = new ASG.Program();

            var fibFunction = new Function
            {
                Name = "Fib",
                Parameters = new List<Variable>
                {
                    new Variable
                    {
                        Name = "n",
                        Type = BuiltInTypes.Int32
                    }
                },
                ReturnType = BuiltInTypes.Int32
            };
            fibFunction.Body = new List<IStatement>
            {
                new IfStatement
                {
                    Condition = new EqualityOperator
                    {
                        Left = new VariableExpression
                        {
                            Variable = fibFunction.Parameters[0]
                        },
                        Right = new IntegerLiteral
                        {
                            Value = 0
                        }
                    },
                    Body = new List<IStatement>
                    {
                        new ReturnStatement
                        {
                            Value = new IntegerLiteral
                            {
                                Value = 0
                            }
                        }
                    },
                },
                new IfStatement
                {
                    Condition = new EqualityOperator
                    {
                        Left = new VariableExpression
                        {
                            Variable = fibFunction.Parameters[0]
                        },
                        Right = new IntegerLiteral
                        {
                            Value = 1
                        }
                    },
                    Body = new List<IStatement>
                    {
                        new ReturnStatement
                        {
                            Value = new IntegerLiteral
                            {
                                Value = 1
                            }
                        }
                    },
                },
                new ReturnStatement
                {
                    Value = new AdditionOperator
                    {
                        Left = new FunctionCall
                        {
                            Function = fibFunction,
                            Arguments = new List<IExpression>
                            {
                                new SubtractionOperator
                                {
                                    Left = new VariableExpression
                                    {
                                        Variable = fibFunction.Parameters[0]
                                    },
                                    Right = new IntegerLiteral
                                    {
                                        Value = 1
                                    }
                                }
                            }
                        },
                        Right = new FunctionCall
                        {
                            Function = fibFunction,
                            Arguments = new List<IExpression>
                            {
                                new SubtractionOperator
                                {
                                    Left = new VariableExpression
                                    {
                                        Variable = fibFunction.Parameters[0]
                                    },
                                    Right = new IntegerLiteral
                                    {
                                        Value = 2
                                    }
                                }
                            }
                        }
                    }
                }
            };
            program.Functions.Add(fibFunction);

            var fibVariable = new Variable
            {
                Name = "fib",
                Type = BuiltInTypes.Int32
            };

            var mainFunction = new Function
            {
                Name = "Main",
                Parameters = new List<Variable>(),
                ReturnType = BuiltInTypes.Unit,
                Body = new List<IStatement>
                {
                    new VariableDeclaration
                    {
                        Variable = fibVariable,
                        InitialValue = new FunctionCall
                        {
                            Function = fibFunction,
                            Arguments = new List<IExpression>
                            {
                                new IntegerLiteral { Value = 8 }
                            }
                        }
                    },
                    new FunctionCall
                    {
                        Function = BuiltInFunctions.WriteLineToConsole,
                        Arguments = new List<IExpression>
                        {
                            new StringLiteral { Value = "Hello, world!" }
                        }
                    },
                    new FunctionCall
                    {
                        Function = BuiltInFunctions.WriteLineToConsole,
                        Arguments = new List<IExpression>
                        {
                            new FunctionCall
                            {
                                Function = BuiltInFunctions.Int32ToString,
                                Arguments = new List<IExpression>
                                {
                                    new VariableExpression
                                    {
                                        Variable = fibVariable
                                    }
                                }
                            }
                        }
                    }
                }
            };
            program.Functions.Add(mainFunction);

            return program;
        }
    }
}
