
using Presto.ParseTree;

namespace Presto.CLI;

class Program
{
    static async Task Main(string[] args)
    {
        // Create parse tree.
        ParseTree.Program parseTree = new(
            Expressions: new List<IExpression>
            {
                new CallExpression(
                    FunctionExpression: new MemberAccessOperator(
                        Expression: new Identifier("Console"),
                        Member: new Identifier("WriteLine")),
                    Arguments: new List<IExpression>
                    {
                        new StringLiteral("Hello, world!")
                    })
            });

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
