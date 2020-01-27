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
            var program = CreateProgram();

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

        public static ASG.Program CreateProgram()
        {
            var program = new ASG.Program();
            var mainFunction = new Function
            {
                Name = "Main",
                Parameters = new List<Parameter>(),
                ReturnType = BuiltInTypes.Unit,
                Body = new List<IStatement>
                {
                    new FunctionCall
                    {
                        Function = BuiltInFunctions.WriteLineToConsole,
                        Arguments = new List<IExpression>
                        {
                            new StringLiteral { Value = "Hello, world!" }
                        }
                    }
                }
            };
            program.Functions.Add(mainFunction);

            return program;
        }
    }
}
