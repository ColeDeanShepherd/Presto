using DeepEqual.Syntax;
using Presto.AST;
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
    public void Literals()
    {
        const string sourceCode = "\"\";2;";
        const string expectedGeneratedCode = "\"\";2;";

        AssertCodeGenerated(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void UnresolvedIdentifier()
    {
        const string sourceCode = "Asdf;";

        AssertCompileFailed(
            sourceCode,
            expectedAstBuilderErrors: new List<IASTBuilderError>
            {
                new UnresolvedNameError("Asdf")
            });
    }

    [Fact]
    public void FunctionCall()
    {
        const string sourceCode = "Console.WriteLine(\"Hello, world!\");";
        const string expectedGeneratedCode = "Console.WriteLine(\"Hello, world!\");";

        AssertCodeGenerated(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void FunctionCall_InvalidNumArguments()
    {
        const string sourceCode = "Console.WriteLine();";

        AssertCompileFailed(
            sourceCode,
            expectedAstBuilderErrors: new List<IASTBuilderError>
            {
                new InvalidArgumentCountError("WriteLine", 1, 0)
            });
    }

    [Fact]
    public void FunctionCall_ArgumentWrongType()
    {
        const string sourceCode = "Console.WriteLine(1);";

        AssertCompileFailed(
            sourceCode,
            expectedAstBuilderErrors: new List<IASTBuilderError>
            {
                new InvalidTypeError(ExpectedType: Types.StringType.Name, EncounteredType: Types.Int32Type.Name)
            });
    }

    [Fact]
    public void VariableDeclaration()
    {
        const string sourceCode = "let msg: string = \"Hello, world!\";";
        const string expectedGeneratedCode = "string msg = \"Hello, world!\";";

        AssertCodeGenerated(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void VariableDeclaration_WrongValueType()
    {
        const string sourceCode = "let msg: string = 1;";

        AssertCompileFailed(
            sourceCode,
            expectedAstBuilderErrors: new List<IASTBuilderError>
            {
                new InvalidTypeError(ExpectedType: Types.StringType.Name, EncounteredType: Types.Int32Type.Name)
            });
    }

    [Fact]
    public void HelloWorldWithVariable()
    {
        const string sourceCode = "let msg: string = \"Hello, world!\"; Console.WriteLine(msg);";
        const string expectedGeneratedCode = "string msg = \"Hello, world!\";Console.WriteLine(msg);";

        AssertCodeGenerated(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void Struct()
    {
        const string sourceCode = "struct ToDo { description: string, isComplete: bool };";
        const string expectedGeneratedCode = "class ToDo { public string description; public bool isComplete;  };";

        AssertCodeGenerated(sourceCode, expectedGeneratedCode);
    }

    #endregion Tests

    #region Helper Methods

    private void AssertCodeGenerated(string sourceCode, string expectedGeneratedCode)
    {
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
        (AST.Program program, List<IASTBuilderError> buildAstErrors) = builder.BuildAST(parseTree);
        Assert.Empty(buildAstErrors);

        // Generate code.
        CodeGenerator codeGenerator = new();
        string generatedCode = codeGenerator.GenerateCode(program);

        Assert.Equal(expectedGeneratedCode, generatedCode);
    }

    private void AssertCompileFailed(
        string sourceCode,
        List<ILexerError>? expectedLexerErrors = null,
        List<IParserError>? expectedParserErrors = null,
        List<IASTBuilderError>? expectedAstBuilderErrors = null)
    {
        // Create tokens.
        Lexer lexer = new Lexer(sourceCode);
        (List<Token> tokens, List<ILexerError> tokenizeErrors) = lexer.Tokenize();
        (expectedLexerErrors ?? new List<ILexerError>()).WithDeepEqual(tokenizeErrors).Assert();

        // Create parse tree.
        Parser parser = new(tokens);
        (ParseTree.Program parseTree, List<IParserError> parseErrors) = parser.ParseProgram();
        (expectedParserErrors ?? new List<IParserError>()).WithDeepEqual(parseErrors).Assert();

        // Translate parse tree to AST.
        ASTBuilder builder = new();
        (AST.Program program, List<IASTBuilderError> buildAstErrors) = builder.BuildAST(parseTree);
        (expectedAstBuilderErrors ?? new List<IASTBuilderError>()).WithDeepEqual(buildAstErrors).Assert();

        // Generate code.
        CodeGenerator codeGenerator = new();
        string generatedCode = codeGenerator.GenerateCode(program);
    }

    #endregion Helper Methods
}