namespace Presto.AST
{
    public interface IDeclaration { }

    public record Namespace(
        string Name,
        List<IDeclaration> Declarations
    ) : IDeclaration;

    public record Function(
        string Name
    ) : IDeclaration;

    public interface IExpression { }

    public record FunctionCall(
        Function Function,
        List<IExpression> Arguments
    ) : IExpression;

    public record StringLiteral(
        string Value
    ) : IExpression;
}
