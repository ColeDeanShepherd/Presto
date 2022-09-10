using Presto.AST;

namespace Presto.CLI;

class Program
{
    static void Main(string[] args)
    {
        // Create program node.
        Namespace globalNamespace = new(
            "",
            Declarations: new List<IDeclaration>(),
            ParentNamespace: null);

        AST.Program program = new(
            globalNamespace,
            Expressions: new List<IExpression>());

        // Create "Console" namespace.
        Namespace consoleNamespace = new(
            "Console",
            Declarations: new List<IDeclaration>(),
            ParentNamespace: globalNamespace);

        // Create "WriteLine" function.
        Function writeLineFunction = new(
            "WriteLine",
            ParentNamespace: consoleNamespace);

        // Add "WriteLine" function to "Console" namespace.
        consoleNamespace.Declarations.Add(writeLineFunction);

        // Initialize the program node.
        program.Expressions.Add(
            new FunctionCall(
                writeLineFunction,
                Arguments: new List<IExpression>
                {
                    new StringLiteral("Hello, World!")
                }));

        // Generate code.
        CodeGenerator codeGenerator = new();
        string generatedCode = codeGenerator.GenerateCode(program);

        // Output generated code to console.
        Console.WriteLine(generatedCode);
    }
}
