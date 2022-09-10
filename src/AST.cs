namespace Presto.AST;

public interface IDeclaration { }

public interface IDeclarationExpression : IDeclaration, IExpression { }

public record Program(
    Namespace GlobalNamespace,
    List<IExpression> Expressions
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
    Namespace ParentNamespace
) : IDeclarationExpression;

public interface IExpression { }

public record FunctionCall(
    Function Function,
    List<IExpression> Arguments
) : IExpression;

public record StringLiteral(
    string Value
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