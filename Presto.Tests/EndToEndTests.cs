using DeepEqual.Syntax;
using Presto.AST;
using Presto.Compiler;
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

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void SingleLineComment()
    {
        const string sourceCode = "# This is a comment.";
        const string expectedGeneratedCode = "";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void Literals()
    {
        const string sourceCode = "\"\"2";
        const string expectedGeneratedCode = "\"\";2;";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void UnresolvedIdentifier()
    {
        const string sourceCode = "Asdf";

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
        const string sourceCode = "Console.WriteLine(\"Hello, world!\")";
        const string expectedGeneratedCode = "Console.WriteLine(\"Hello, world!\");";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void FunctionCall_InvalidNumArguments()
    {
        const string sourceCode = "Console.WriteLine()";

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
        const string sourceCode = "Console.WriteLine(1)";

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
        const string sourceCode = "let msg: String = \"Hello, world!\"";
        const string expectedGeneratedCode = "string msg = \"Hello, world!\";";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void VariableDeclaration_WrongValueType()
    {
        const string sourceCode = "let msg: String = 1";

        AssertCompileFailed(
            sourceCode,
            expectedAstBuilderErrors: new List<IASTBuilderError>
            {
                new InvalidTypeError(ExpectedType: Types.StringType.Name, EncounteredType: Types.Int32Type.Name)
            });
    }

    [Fact]
    public void VariableDeclaration_DuplicateName()
    {
        const string sourceCode = "let msg: String = \"\" let msg: String = \"\"";

        AssertCompileFailed(
            sourceCode,
            expectedAstBuilderErrors: new List<IASTBuilderError>
            {
                new DuplicateNameError("msg")
            });
    }

    [Fact]
    public void HelloWorldWithVariable()
    {
        const string sourceCode = "let msg: String = \"Hello, world!\" Console.WriteLine(msg)";
        const string expectedGeneratedCode = "string msg = \"Hello, world!\";Console.WriteLine(msg);";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void EmptyStruct()
    {
        const string sourceCode = "struct ToDo { }";
        const string expectedGeneratedCode = "class ToDo { }";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void Struct()
    {
        const string sourceCode = "struct ToDo { description: String, isComplete: Bool }";
        const string expectedGeneratedCode = "class ToDo { public string description; public bool isComplete; }";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    [Fact]
    public void Struct_DuplicateFieldName()
    {
        const string sourceCode = "struct ToDo { description: String, description: bool }";

        AssertCompileFailed(
            sourceCode,
            expectedAstBuilderErrors: new List<IASTBuilderError>
            {
                new DuplicateNameError("description")
            });
    }

    [Fact]
    public void DuplicateStructNames()
    {
        const string sourceCode = "struct ToDo { } struct ToDo { }";

        AssertCompileFailed(
            sourceCode,
            expectedAstBuilderErrors: new List<IASTBuilderError>
            {
                new DuplicateNameError("ToDo")
            });
    }

    [Fact]
    public void EmptyFunction()
    {
        const string sourceCode = "fn x() {}";
        const string expectedGeneratedCode = "void x() { }";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    // duplicate

    [Fact]
    public void FibonacciNumberFunction()
    {
        const string sourceCode = "fib(n) = case n of 0 -> 0 1 -> 1 n -> fib(n - 1) + fib(n - 2)";
        const string expectedGeneratedCode = "void x() { }";

        AssertCompileSucceeded(sourceCode, expectedGeneratedCode);
    }

    #endregion Tests

    #region Helper Methods

    private void AssertCompileSucceeded(string sourceCode, string? expectedGeneratedCode = null)
    {
        // Create tokens.
        Lexer lexer = new Lexer(PrestoGrammarConstants.LexerGrammar, sourceCode);
        (List<Token> tokens, List<ILexerError> tokenizeErrors) = lexer.Tokenize();
        Assert.Empty(tokenizeErrors);

        // Create parse tree.
        Parser parser = new(PrestoGrammarConstants.Grammar, tokens);
        (ParseTree.Program? parseTree, List<IParserError> parseErrors) = parser.Parse();
        Assert.Empty(parseErrors);

        // Translate parse tree to AST.
        ASTBuilder builder = new();
        (AST.Program program, List<IASTBuilderError> buildAstErrors) = builder.BuildAST(parseTree!);
        Assert.Empty(buildAstErrors);

        // Generate code.
        CodeGenerator codeGenerator = new();
        string generatedCode = codeGenerator.GenerateCode(program);

        if (expectedGeneratedCode != null)
        {
            Assert.Equal(expectedGeneratedCode, generatedCode);
        }
    }

    private void AssertCompileFailed(
        string sourceCode,
        List<ILexerError>? expectedLexerErrors = null,
        List<IParserError>? expectedParserErrors = null,
        List<IASTBuilderError>? expectedAstBuilderErrors = null)
    {
        // Create tokens.
        Lexer lexer = new Lexer(PrestoGrammarConstants.LexerGrammar, sourceCode);
        (List<Token> tokens, List<ILexerError> tokenizeErrors) = lexer.Tokenize();
        (expectedLexerErrors ?? new List<ILexerError>()).WithDeepEqual(tokenizeErrors).Assert();

        // Create parse tree.
        Parser parser = new(PrestoGrammarConstants.Grammar, tokens);
        (ParseTree.Program? parseTree, List<IParserError> parseErrors) = parser.Parse();
        (expectedParserErrors ?? new List<IParserError>()).WithDeepEqual(parseErrors).Assert();

        // Translate parse tree to AST.
        ASTBuilder builder = new();
        (AST.Program program, List<IASTBuilderError> buildAstErrors) = builder.BuildAST(parseTree!);
        (expectedAstBuilderErrors ?? new List<IASTBuilderError>()).WithDeepEqual(buildAstErrors).Assert();

        // Generate code.
        CodeGenerator codeGenerator = new();
        string generatedCode = codeGenerator.GenerateCode(program);
    }

    #endregion Helper Methods
}