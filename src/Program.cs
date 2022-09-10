using Presto.AST;

class Program
{
    static void Main(string[] args)
    {
        // Create "Console" namespace.
        Namespace consoleNamespace = new(
            "Console",
            Declarations: new List<IDeclaration>());

        // Create "WriteLine" function.
        Function writeLineFunction = new("WriteLine");

        // Add "WriteLine" function to "Console" namespace.
        consoleNamespace.Declarations.Add(writeLineFunction);

        // Create program AST.
        List<IExpression> program = new()
        {
            new FunctionCall(
                writeLineFunction,
                Arguments: new List<IExpression>
                {
                    new StringLiteral("Hello, World!")
                })
        };

        // Output to console.
        Console.WriteLine("Not there yet!");
    }
}
