using Presto.ASG;
using Presto.CodeGenerators;
using System;

namespace Presto
{
    class Program
    {
        static void Main(string[] args)
        {
            var sourceCode = "fn main(){std.io.stdout.writeLine(\"Hello, world!\");}";
            (var tokens, var lexerErrors) = (new Lexer.Lexer()).Tokenize(sourceCode);
            (var programAst, var parserErrors) = (new Parser.Parser()).Parse(tokens);
            (var program, var asgBuilderErrors) = (new AsgBuilder()).BuildAsg(programAst);

            var prestoGenerator = new PrestoCodeGenerator();
            prestoGenerator.Visit(program, Unit.Instance);
            Console.WriteLine(prestoGenerator.GeneratedCode);

            Console.WriteLine();
            Console.WriteLine("==========");
            Console.WriteLine();

            var cGenerator = new CCodeGenerator();
            cGenerator.Visit(program, Unit.Instance);
            Console.WriteLine(cGenerator.GeneratedCode);

            Console.ReadKey();
        }
    }
}
