using Presto.ASG;
using Presto.CodeGenerators;
using System;

namespace Presto
{
    class Program
    {
        static void Main(string[] args)
        {
            var program = ASG.Program.CreateWithStdLib();

            var mainFn = new Function("main");
            mainFn.SetParent(program.GlobalNamespace);

            var mainFnBody = new Block();
            mainFnBody.ParentNode = mainFn;
            mainFn.Body = mainFnBody;

            var printCall = new FunctionCall();
            printCall.Function = program.GlobalNamespace.FindDeclaration(new[] { "std", "io", "stdout", "writeLine" }) as Function;
            printCall.ParentNode = mainFnBody;
            mainFnBody.Statements.Add(printCall);

            var printCallArg = new StringLiteral();
            printCallArg.Value = "Hello, world!";
            printCallArg.ParentNode = printCall;
            printCall.Arguments.Add(printCallArg);

            var prestoGenerator = new PrestoCodeGenerator();
            prestoGenerator.Visit(program);
            Console.WriteLine(prestoGenerator.GeneratedCode);

            Console.WriteLine();
            Console.WriteLine("==========");
            Console.WriteLine();

            var cGenerator = new CCodeGenerator();
            cGenerator.Visit(program);
            Console.WriteLine(cGenerator.GeneratedCode);

            Console.ReadKey();
        }
    }
}
