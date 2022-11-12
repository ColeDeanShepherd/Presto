using Presto.Compiler;

namespace Presto.CLI;

class Program
{
    static async Task Main(string[] args)
    {
        var grammar = PrestoGrammarConstants.Grammar;
        var p1 = GrammarHelpers.PrintGrammar(grammar);
        var p2 = GrammarHelpers.ResolveReferences(grammar);



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
        GrammarParser parser = new(PrestoGrammarConstants.Grammar, tokens);
        (ParseTree.Program? parseTree, List<IParserError> parseErrors) = parser.Parse();

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
        (AST.Program program, List<IASTBuilderError> buildAstErrors) = builder.BuildAST(parseTree);

        if (buildAstErrors.Any())
        {
            foreach (IASTBuilderError error in buildAstErrors)
            {
                Console.WriteLine($"ERROR: {error.GetDescription()}");
            }

            return;
        }

        // Generate code.
        CodeGenerator codeGenerator = new();
        string generatedCode = codeGenerator.GenerateCode(program);

        // Run the code!.
        CSharpCodeRunner codeRunner = new();
        await codeRunner.RunCode(generatedCode);
    }
}
