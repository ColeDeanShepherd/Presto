using Presto.ASG;
using Presto.CodeGeneration;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Presto
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var program = CreateProgram();
            var (generatedCode, codeGenErrors) = CSharpCodeGenerator.GenerateCode(program);

            foreach (var error in codeGenErrors)
            {
                Console.WriteLine(error);
            }

            if (codeGenErrors.Any())
            {
                Console.WriteLine($"Compilation failed with {codeGenErrors.Count} errors.");
                return;
            }

            Console.WriteLine(generatedCode);

            CSharpCompiler.CompileCSharpProgram(generatedCode);
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
