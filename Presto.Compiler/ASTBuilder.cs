using Presto.AST;

namespace Presto;

public class ASTBuilder
{
    public ASTBuilder()
    {
        // Create program node.
        Namespace globalNamespace = new(
            "",
            Declarations: new List<IDeclaration>(),
            ParentNamespace: null);

        program = new Program(
            globalNamespace,
            Statements: new List<IStatement>());

        scope = globalNamespace;
    }

    public AST.Program BuildAST(ParseTree.Program parseTree)
    {
        // Create "Console" namespace.
        Namespace consoleNamespace = new(
            "Console",
            Declarations: new List<IDeclaration>(),
            ParentNamespace: program.GlobalNamespace);

        program.GlobalNamespace.Declarations.Add(consoleNamespace);

        // Create "WriteLine" function.
        Function writeLineFunction = new(
            "WriteLine",
            ParentNamespace: consoleNamespace);

        // Add "WriteLine" function to "Console" namespace.
        consoleNamespace.Declarations.Add(writeLineFunction);

        program.Statements.AddRange(
            parseTree.Statements
                .Select(Visit)
                .ToList()
        );

        return program;
    }

    public IStatement Visit(ParseTree.IStatement statement)
    {
        if (statement is ParseTree.LetStatement)
        {
            return Visit((ParseTree.LetStatement)statement);
        }
        if (statement is ParseTree.IExpression)
        {
            return Visit((ParseTree.IExpression)statement);
        }
        else
        {
            throw new NotImplementedException($"Unknown statement type {statement.GetType().Name}");
        }
    }

    public LetStatement Visit(ParseTree.LetStatement letStatement)
    {
        LetStatement result = new(
            letStatement.VariableName.Text,
            ResolveType(letStatement.TypeName),
            Visit(letStatement.Value));

        scope.Declarations.Add(result);

        return result;
    }

    public IExpression Visit(ParseTree.IExpression expression)
    {
        if (expression is ParseTree.CallExpression)
        {
            return Visit((ParseTree.CallExpression)expression);
        }
        else if (expression is ParseTree.MemberAccessOperator)
        {
            return Visit((ParseTree.MemberAccessOperator)expression);
        }
        else if (expression is ParseTree.Identifier)
        {
            IDeclaration declaration = Visit((ParseTree.Identifier)expression);

            if (declaration is IDeclarationExpression)
            {
                return (IDeclarationExpression)declaration;
            }
            else if (declaration is LetStatement letStatement)
            {
                return new VariableReference(letStatement);
            }
            else
            {
                throw new Exception();
            }
        }
        else if (expression is ParseTree.StringLiteral)
        {
            return Visit((ParseTree.StringLiteral)expression);
        }
        else
        {
            throw new NotImplementedException($"Unknown expression type {expression.GetType().Name}");
        }
    }

    public FunctionCall Visit(ParseTree.CallExpression callExpression)
    {
        return new FunctionCall(
            ResolveFunction(callExpression.FunctionExpression),
            callExpression.Arguments
                .Select(arg => Visit(arg))
                .ToList());
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

    public IDeclaration Visit(ParseTree.Identifier identifier)
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

        throw new Exception();
    }

    public StringLiteral Visit(ParseTree.StringLiteral stringLiteral)
    {
        return new StringLiteral(stringLiteral.Value);
    }

    public Function ResolveFunction(ParseTree.IExpression expression)
    {
        if (expression is ParseTree.MemberAccessOperator)
        {
            IDeclaration declaration = Visit((ParseTree.MemberAccessOperator)expression);

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

    public IType ResolveType(ParseTree.Identifier identifier)
    {
        if (identifier.Text == "string")
        {
            return Types.StringType;
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public string GetNameFromDeclaration(IDeclaration declaration)
    {
        if (declaration is Namespace)
        {
            return ((Namespace)declaration).Name;
        }
        else if (declaration is Function)
        {
            return ((Function)declaration).Name;
        }
        else if (declaration is LetStatement letStatement)
        {
            return letStatement.VariableName;
        }
        else
        {
            throw new NotImplementedException($"Unknown declaration type {declaration.GetType().Name}");
        }
    }

    public IDeclaration GetDeclarationFromExpression(ParseTree.IExpression expression)
    {
        if (expression is ParseTree.MemberAccessOperator)
        {
            return Visit((ParseTree.MemberAccessOperator)expression);
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

    private Program program;
    private Namespace scope;
}
