﻿using Presto.AST;

namespace Presto;

public interface IASTBuilderError
{
    public string GetDescription();
}

public record UnexpectedNodeError(
    ParseTree.INode Node
) : IASTBuilderError
{
    public string GetDescription()
    {
        return $"Unexpected node: {Node.GetType()}.";
    }
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

        program = new Program(
            globalNamespace,
            Statements: new List<IStatement>());

        scope = globalNamespace;
        errors = new List<IASTBuilderError>();
    }

    public (AST.Program, List<IASTBuilderError>) BuildAST(ParseTree.Program parseTree)
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
                .Select(Visit)
                .Where(x => x != null)
                .Cast<IStatement>()
                .ToList()
        );

        return (program, errors);
    }

    public IStatement? Visit(ParseTree.IStatement statement)
    {
        if (statement is ParseTree.LetStatement letStatement)
        {
            return Visit(letStatement);
        }
        else if (statement is ParseTree.StructDefinition structDeclaration)
        {
            return Visit(structDeclaration);
        }
        else if (statement is ParseTree.IExpression)
        {
            return Visit((ParseTree.IExpression)statement);
        }
        else
        {
            errors.Add(new UnexpectedNodeError(statement));
            return null;
        }
    }

    public LetStatement? Visit(ParseTree.LetStatement letStatement)
    {
        if (scope.Declarations.Any(x => GetNameFromDeclaration(x) == letStatement.VariableName.Text))
        {
            errors.Add(new DuplicateNameError(letStatement.VariableName.Text));
            return null;
        }

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

        LetStatement result = new(
            letStatement.VariableName.Text,
            variableType,
            value);

        scope.Declarations.Add(result);

        return result;
    }

    public StructDefinition? Visit(ParseTree.StructDefinition structDefinition)
    {
        List<FieldDefinition> fieldDefinitions = new();

        foreach (var fieldDeclaration in structDefinition.FieldDeclarations)
        {
            if (fieldDefinitions.Any(x => x.FieldName == fieldDeclaration.FieldName.Text))
            {
                errors.Add(new DuplicateNameError(fieldDeclaration.FieldName.Text));
                continue;
            }

            IType fieldType = ResolveType(fieldDeclaration.TypeName);
            if (fieldType == null)
            {
                return null;
            }

            fieldDefinitions.Add(new FieldDefinition(fieldDeclaration.FieldName.Text, fieldType));
        }



        StructDefinition result = new(
            structDefinition.StructName.Text,
            fieldDefinitions);

        scope.Declarations.Add(result);

        return result;
    }

    public IExpression? Visit(ParseTree.IExpression expression)
    {
        if (expression is ParseTree.CallExpression)
        {
            return Visit((ParseTree.CallExpression)expression);
        }
        else if (expression is ParseTree.MemberAccessOperator)
        {
            return Visit((ParseTree.MemberAccessOperator)expression);
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
            else if (declaration is LetStatement letStatement)
            {
                return new VariableReference(letStatement);
            }
            else
            {
                throw new NotImplementedException($"Unknown declaration expression type {expression.GetType().Name}");
            }
        }
        else
        {
            errors.Add(new UnexpectedNodeError(expression));
            return null;
        }
    }

    public FunctionCall? Visit(ParseTree.CallExpression callExpression)
    {
        Function? function = ResolveFunction(callExpression.FunctionExpression);
        if (function == null)
        {
            return null;
        }

        List<IExpression> arguments = callExpression.Arguments
            .Select(arg => Visit(arg))
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

    public NumberLiteral Visit(ParseTree.NumberLiteral stringLiteral)
    {
        return new NumberLiteral(stringLiteral.Text);
    }

    public StringLiteral Visit(ParseTree.StringLiteral stringLiteral)
    {
        return new StringLiteral(stringLiteral.Value);
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

    public IType ResolveType(IExpression expression)
    {
        if (expression is NumberLiteral number)
        {
            return Types.Int32Type;
        }
        else if (expression is StringLiteral stringLiteral)
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
        if (qualifiedName.Identifiers.Count == 1)
        {
            return ResolveType(qualifiedName.Identifiers[0]);
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public IType ResolveType(ParseTree.Identifier identifier)
    {
        if (identifier.Text == "string")
        {
            return Types.StringType;
        }
        else if (identifier.Text == "bool")
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
    private List<IASTBuilderError> errors;
}
