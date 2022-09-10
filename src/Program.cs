using Presto.AST;

class Program
{
    static void Main(string[] args)
    {
        // Create program node.
        Presto.AST.Program program = new(
            Expressions: new List<IExpression>());

        // Create "Console" namespace.
        Namespace consoleNamespace = new(
            "Console",
            Declarations: new List<IDeclaration>());

        // Create "WriteLine" function.
        Function writeLineFunction = new("WriteLine");

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

        // Output to console.
        Console.WriteLine("Not there yet!");
    }
}
