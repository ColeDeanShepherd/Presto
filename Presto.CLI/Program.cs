namespace Presto.CLI;

class Program
{
    static async Task Main(string[] args)
    {
        string sourceCode = "Console.WriteLine(\"Hello, world!\");";

        // Create tokens.
        Lexer lexer = new Lexer(sourceCode);
        (List<Token> tokens, List<ILexerError> errors) = lexer.Tokenize();

        if (errors.Any())
        {
            foreach (ILexerError error in errors)
            {
                Console.WriteLine($"ERROR {error.TextRange}: {error.GetDescription()}");
            }

            return;
        }

        // Create parse tree.
        Parser parser = new(tokens);
        ParseTree.Program parseTree = parser.ParseProgram();

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
