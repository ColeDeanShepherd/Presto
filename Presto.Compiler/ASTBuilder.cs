using Presto.Compiler.AST;
using Presto.Compiler.ParseTree;

namespace Presto.Compiler.AST;

public interface IASTBuilderError
{
    public string GetDescription();
}

public record NoMainError() : IASTBuilderError
{
    public string GetDescription() => "No \"main\" function is defined.";
}

public record UnexpectedNodeError(
    ParseTree.IParseTreeNode Node
) : IASTBuilderError
{
    public string GetDescription() => $"Unexpected node: {Node.GetType()}.";
};

public record UnresolvedNameError(
    string Name
) : IASTBuilderError
{
    public string GetDescription()
    {
        return $"Unresolved name: {Name}.";
    }
}

public record InvalidArgumentCountError(
    string FunctionName,
    int NumParameters,
    int NumArgumentsProvided) : IASTBuilderError
{
    public string GetDescription()
    {
        return $"Function {FunctionName} has {NumParameters} parameters, but was called with {NumArgumentsProvided} arguments.";
    }
}

public record InvalidTypeError(
    string ExpectedType,
    string EncounteredType
) : IASTBuilderError
{
    public string GetDescription()
    {
        return $"Expected type {ExpectedType} but encountered type {EncounteredType}.";
    }
}

public record DuplicateNameError(
    string Name
) : IASTBuilderError
{
    public string GetDescription() => $"Duplicate name: \"{Name}\"";
}

public class ASTBuilder
{
    public ASTBuilder()
    {
        // Create program node.
        Namespace globalNamespace = new(
            "",
            Declarations: new List<IDeclaration>(),
            ParentNamespace: null);

        program = new AST.Program(
            globalNamespace,
            Statements: new List<IStatement>());

        scope = globalNamespace;
        errors = new List<IASTBuilderError>();
    }

    public (AST.Program, List<IASTBuilderError>) BuildAST(ParseTree.Program parseTree, bool isLibrary)
    {
        // Create "Console" namespace.
        Namespace consoleNamespace = new(
            "Console",
            Declarations: new List<IDeclaration>(),
            ParentNamespace: program.GlobalNamespace);

        program.GlobalNamespace.Declarations.Add(consoleNamespace);

        // Create "WriteLine" function.
        Function writeLineFunction = new(
            Name: "WriteLine",
            ParentNamespace: consoleNamespace,
            ParameterDeclarations: new List<ParameterDeclaration>
            {
                new ParameterDeclaration("value", Types.StringType)
            });

        // Add "WriteLine" function to "Console" namespace.
        consoleNamespace.Declarations.Add(writeLineFunction);

        program.Statements.AddRange(
            parseTree.Statements
                .Map(Visit)
                .Where(x => x != null)
                .Cast<IStatement>()
                .ToList()
        );

        // Check for the existence of a "main" function.
        if (!isLibrary && !DoesMainFunctionExist(program))
        {
            errors.Add(new NoMainError());
        }

        return (program, errors);
    }

    public bool DoesMainFunctionExist(AST.Program program) =>
        program.Statements
            .Any(s => (s is Function func) && func.Name == "main");

    public IStatement? Visit(ParseTree.Statement statement)
    {
        var child = statement.Children.First();

        if (child is ParseTree.LetStatement letStatement)
        {
            return Visit(letStatement);
        }
        else if (child is ParseTree.FunctionDefinition functionDefinition)
        {
            return Visit(functionDefinition);
        }
        else if (child is ParseTree.StructDeclaration structDeclaration)
        {
            return Visit(structDeclaration);
        }
        else if (child is ParseTree.Expression expr)
        {
            return Visit(expr);
        }
        else
        {
            errors.Add(new UnexpectedNodeError(statement));
            return null;
        }
    }

    public AST.LetStatement? Visit(ParseTree.LetStatement letStatement)
    {
        IType? variableType = ResolveType(letStatement.TypeName);
        if (variableType == null)
        {
            return null;
        }

        IExpression? value = Visit(letStatement.Value);
        if (value == null)
        {
            return null;
        }

        IType valueType = ResolveType(value);
        if (valueType != variableType)
        {
            errors.Add(new InvalidTypeError(ExpectedType: variableType.Name, EncounteredType: valueType.Name));
            return null;
        }

        AST.LetStatement result = new(
            letStatement.VariableName.Token.Text,
            variableType,
            value);

        if (!AddToScope(result))
        {
            return null;
        }

        return result;
    }

    public Function? Visit(ParseTree.FunctionDefinition functionDefinition)
    {
        List<ParameterDeclaration> parameterDeclarations = new();

        foreach (var paramDecl in functionDefinition.ParameterDeclarations)
        {
            if (parameterDeclarations.Any(x => x.Name == paramDecl.FieldName.Token.Text))
            {
                errors.Add(new DuplicateNameError(paramDecl.FieldName.Token.Text));
                continue;
            }

            IType fieldType = ResolveType(paramDecl.TypeName);
            if (fieldType == null)
            {
                return null;
            }

            parameterDeclarations.Add(new ParameterDeclaration(paramDecl.FieldName.Token.Text, fieldType));
        }

        Function result = new(
            functionDefinition.FunctionName.Token.Text,
            scope,
            parameterDeclarations);

        if (!AddToScope(result))
        {
            return null;
        }

        return result;
    }

    public StructDefinition? Visit(ParseTree.StructDeclaration structDefinition)
    {
        List<FieldDefinition> fieldDefinitions = new();

        foreach (var fieldDeclaration in structDefinition.FieldDeclarations)
        {
            if (fieldDefinitions.Any(x => x.FieldName == fieldDeclaration.FieldName.Token.Text))
            {
                errors.Add(new DuplicateNameError(fieldDeclaration.FieldName.Token.Text));
                continue;
            }

            IType fieldType = ResolveType(fieldDeclaration.TypeName);
            if (fieldType == null)
            {
                return null;
            }

            fieldDefinitions.Add(new FieldDefinition(fieldDeclaration.FieldName.Token.Text, fieldType));
        }

        StructDefinition result = new(
            structDefinition.StructName.Token.Text,
            fieldDefinitions);

        if (!AddToScope(result))
        {
            return null;
        }

        return result;
    }

    public IExpression? Visit(ParseTree.Expression expression)
    {
        if (expression is ParseTree.CallExpression callExpr)
        {
            return Visit(callExpr);
        }
        else if (expression is ParseTree.MemberAccessOperator memberAccessOperator)
        {
            return Visit(memberAccessOperator);
        }
        else if (expression is ParseTree.NumberLiteral number)
        {
            return Visit(number);
        }
        else if (expression is ParseTree.StringLiteral stringLiteral)
        {
            return Visit(stringLiteral);
        }
        else if (expression is ParseTree.Identifier identifier)
        {
            IDeclaration? declaration = Visit(identifier);
            if (declaration == null)
            {
                return null;
            }

            if (declaration is IDeclarationExpression)
            {
                return (IDeclarationExpression)declaration;
            }
            else if (declaration is AST.LetStatement letStatement)
            {
                return new VariableReference(letStatement);
            }
            else
            {
                throw new NotImplementedException($"Unknown declaration expression type {expression.GetType().Name}");
            }
        }

        errors.Add(new UnexpectedNodeError(expression));
        return null;
    }

    public FunctionCall? Visit(ParseTree.CallExpression callExpression)
    {
        Function? function = ResolveFunction(callExpression.FunctionExpression);
        if (function == null)
        {
            return null;
        }

        List<IExpression> arguments = callExpression.Arguments
            .Map(arg => Visit(arg))
            .Where(x => x != null)
            .Cast<IExpression>()
            .ToList();

        if (arguments.Count != function.ParameterDeclarations.Count)
        {
            errors.Add(new InvalidArgumentCountError(function.Name, function.ParameterDeclarations.Count, arguments.Count));
            return null;
        }

        for (int i = 0; i < function.ParameterDeclarations.Count; i++)
        {
            ParameterDeclaration parameterDeclaration = function.ParameterDeclarations[i];
            IExpression argument = arguments[i];
            IType argumentType = ResolveType(argument);

            if (parameterDeclaration.Type != argumentType)
            {
                errors.Add(new InvalidTypeError(parameterDeclaration.Type.Name, argumentType.Name));
            }
        }

        return new FunctionCall(
            function,
            arguments);
    }

    public IDeclarationExpression Visit(ParseTree.MemberAccessOperator memberAccessOperator)
    {
        IDeclaration accessedDeclaration = GetDeclarationFromExpression(memberAccessOperator.Expression);

        if (accessedDeclaration is Namespace)
        {
            scope = (Namespace)accessedDeclaration;
            IDeclaration declaration = GetDeclarationFromExpression(memberAccessOperator.Member);
            scope = scope.ParentNamespace!;

            if (declaration is IDeclarationExpression)
            {
                return (IDeclarationExpression)declaration;
            }
            else
            {
                throw new Exception();
            }
        }
        else
        {
            throw new Exception();
        }
    }

    public AST.NumberLiteral Visit(ParseTree.NumberLiteral numberLiteral)
    {
        return new AST.NumberLiteral(numberLiteral.Value);
    }

    public AST.StringLiteral Visit(ParseTree.StringLiteral stringLiteral)
    {
        return new AST.StringLiteral(stringLiteral.Value);
    }

    public IDeclaration? Visit(ParseTree.Identifier identifier)
    {
        Namespace? nullableScope = scope;

        do
        {
            IDeclaration? declaration = nullableScope.Declarations
                .FirstOrDefault(decl => GetNameFromDeclaration(decl) == identifier.Text);

            if (declaration != null)
            {
                return declaration;
            }

            nullableScope = nullableScope.ParentNamespace;
        } while (nullableScope != null);

        errors.Add(new UnresolvedNameError(identifier.Text));
        return null;
    }

    public Function ResolveFunction(ParseTree.Expression expression)
    {
        if (expression is ParseTree.MemberAccessOperator memberAccessOperator)
        {
            IDeclaration declaration = Visit(memberAccessOperator);

            if (declaration is Function)
            {
                return (Function)declaration;
            }
            else
            {
                // TODO: improve
                throw new Exception();
            }
        }
        else
        {
            throw new NotImplementedException($"Unknown function expression type {expression.GetType().Name}");
        }
    }

    public IType ResolveType(IExpression expression)
    {
        if (expression is AST.NumberLiteral number)
        {
            return Types.Int32Type;
        }
        else if (expression is AST.StringLiteral stringLiteral)
        {
            return Types.StringType;
        }
        else if (expression is VariableReference variableReference)
        {
            return variableReference.LetStatement.Type;
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public IType ResolveType(ParseTree.QualifiedName qualifiedName)
    {
        if (qualifiedName.Identifiers.Count() == 1)
        {
            return ResolveIdentifierType(qualifiedName.Identifiers.First());
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public IType ResolveIdentifierType(ParseTree.TerminalParseTreeNode identifier)
    {
        if (identifier.Token.Text == "String")
        {
            return Types.StringType;
        }
        else if (identifier.Token.Text == "Bool")
        {
            return Types.BoolType;
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public string GetNameFromDeclaration(IDeclaration declaration)
    {
        if (declaration is Namespace ns)
        {
            return ns.Name;
        }
        else if (declaration is Function function)
        {
            return function.Name;
        }
        else if (declaration is AST.LetStatement letStatement)
        {
            return letStatement.VariableName;
        }
        else if (declaration is StructDefinition structDefinition)
        {
            return structDefinition.StructName;
        }
        else
        {
            throw new NotImplementedException($"Unknown declaration type {declaration.GetType().Name}");
        }
    }

    public IDeclaration? GetDeclarationFromExpression(ParseTree.Expression expression)
    {
        if (expression is ParseTree.MemberAccessOperator memberAccessOperator)
        {
            return Visit(memberAccessOperator);
        }
        else if (expression is ParseTree.Identifier)
        {
            return Visit((ParseTree.Identifier)expression);
        }
        else
        {
            throw new NotImplementedException($"Unknown declaration expression type {expression.GetType().Name}");
        }
    }

    public bool AddToScope(IDeclaration declaration)
    {
        string declarationName = GetNameFromDeclaration(declaration);
        if (scope.Declarations.Any(x => GetNameFromDeclaration(x) == declarationName))
        {
            errors.Add(new DuplicateNameError(declarationName));
            return false;
        }

        scope.Declarations.Add(declaration);
        return true;
    }

    private AST.Program program;
    private Namespace scope;
    private List<IASTBuilderError> errors;
}
