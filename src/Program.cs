using Presto.ASG;
using Presto.CodeGeneration;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Presto
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var program = CreateProgram();

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

            Console.WriteLine("Compiling C# program...");
            CSharpCompiler.CompileCSharpProgram(generatedCode);

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
