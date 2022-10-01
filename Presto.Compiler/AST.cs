namespace Presto.AST;

public interface IDeclaration { }

public interface IDeclarationExpression : IDeclaration, IExpression { }

public record Program(
    Namespace GlobalNamespace,
    List<IStatement> Statements
);

public record Namespace(
    string Name,
    List<IDeclaration> Declarations,
    Namespace? ParentNamespace
) : IDeclaration
{
    public bool IsGlobal => Name == "";
}

public record Function(
    string Name,
    Namespace ParentNamespace,
    List<ParameterDeclaration> ParameterDeclarations
) : IDeclarationExpression;

public record ParameterDeclaration(
    string Name,
    IType Type
) : IDeclaration;

public interface IStatement { }

public interface IType
{
    string Name { get; }
}

public static class Types
{
    public static readonly IntegerType Int32Type = new();
    public static readonly StringType StringType = new();
    public static readonly BoolType BoolType = new();
}

public record IntegerType() : IType
{
    public string Name => "int";
}

public record StringType() : IType
{
    public string Name => "string";
}

public record BoolType() : IType
{
    public string Name => "bool";
}

public record LetStatement(
    string VariableName,
    IType Type,
    IExpression Value
) : IDeclaration, IStatement;

public record StructDefinition(
    string StructName,
    List<FieldDefinition> FieldDefinitions
) : IDeclaration, IStatement;

public record FieldDefinition(
    string FieldName,
    IType FieldType
) : IDeclaration;

public interface IExpression : IStatement { }

public record FunctionCall(
    Function Function,
    List<IExpression> Arguments
) : IExpression;

public record StringLiteral(
    string Value
) : IExpression;

public record NumberLiteral(
    string ValueAsString
) : IExpression;

public record VariableReference(
    LetStatement LetStatement
) : IExpression;

public static class ASTUtil
{
    public static IEnumerable<Namespace> EnumerateNamespaceHierarchy(Namespace ns)
    {
        return EnumerateNamespaceAndAncestors(ns).Reverse();
    }

    public static IEnumerable<Namespace> EnumerateNamespaceAndAncestors(Namespace ns)
    {
        Namespace? nullableNamespace = ns;

        do
        {
            yield return nullableNamespace;
            nullableNamespace = nullableNamespace.ParentNamespace;
        } while (nullableNamespace != null);
    }
}