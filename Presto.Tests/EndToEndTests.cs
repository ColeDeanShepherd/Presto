using System.Collections.Generic;
using Xunit;

namespace Presto.Tests;

public class EndToEndTests
{
    #region Tests

    [Fact]
    public void Empty()
    {
        const string sourceCode = "";
        const string expectedGeneratedCode = "";

        AssertCodeGenerated(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void HelloWorld()
    {
        const string sourceCode = "Console.WriteLine(\"Hello, world!\");";
        const string expectedGeneratedCode = "Console.WriteLine(\"Hello, world!\");";

        AssertCodeGenerated(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void HelloWorldWithVariable()
    {
        const string sourceCode = "let msg = \"Hello, world!\";Console.WriteLine(msg);";
        const string expectedGeneratedCode = "string msg = \"Hello, world!\";Console.WriteLine(msg);";

        AssertCodeGenerated(sourceCode, expectedGeneratedCode);
    }

    #endregion Tests

    #region Helper Methods

    private void AssertCodeGenerated(string sourceCode, string expectedGeneratedCode)
    {
        #region Act

        // Create tokens.
        Lexer lexer = new Lexer(sourceCode);
        (List<Token> tokens, List<ILexerError> tokenizeErrors) = lexer.Tokenize();
        Assert.Empty(tokenizeErrors);

        // Create parse tree.
        Parser parser = new(tokens);
        (ParseTree.Program parseTree, List<IParserError> parseErrors) = parser.ParseProgram();
        Assert.Empty(parseErrors);

        // Translate parse tree to AST.
        ASTBuilder builder = new();
        AST.Program program = builder.BuildAST(parseTree);

        // Generate code.
        CodeGenerator codeGenerator = new();
        string generatedCode = codeGenerator.GenerateCode(program);

        #endregion Act

        #region Assert

        Assert.Equal(expectedGeneratedCode, generatedCode);

        #endregion Assert
    }

    #endregion Helper Methods
}