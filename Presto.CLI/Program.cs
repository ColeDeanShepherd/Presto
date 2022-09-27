﻿namespace Presto.CLI;

class Program
{
    static async Task Main(string[] args)
    {
        string sourceCode = "Console.WriteLine(\"Hello, world!\");";

        // Tokenize.
        Lexer lexer = new Lexer(sourceCode);
        (List<Token> tokens, List<ILexerError> tokenizeErrors) = lexer.Tokenize();

        if (tokenizeErrors.Any())
        {
            foreach (ILexerError error in tokenizeErrors)
            {
                Console.WriteLine($"ERROR {error.TextRange}: {error.GetDescription()}");
            }

            return;
        }

        // Parse.
        Parser parser = new(tokens);
        (ParseTree.Program parseTree, List<IParserError> parseErrors) = parser.ParseProgram();

        if (parseErrors.Any())
        {
            foreach (IParserError error in parseErrors)
            {
                Console.WriteLine($"ERROR {error.TextRange}: {error.GetDescription()}");
            }

            return;
        }

        // Translate parse tree to AST.
        ASTBuilder builder = new();
        AST.Program program = builder.BuildAST(parseTree);

        // Generate code.
        CodeGenerator codeGenerator = new();
        string generatedCode = codeGenerator.GenerateCode(program);

        // Run the code!.
        CSharpCodeRunner codeRunner = new();
        await codeRunner.RunCode(generatedCode);
    }
}