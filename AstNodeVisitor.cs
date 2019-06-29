namespace Presto.AST
{
    public abstract class AstNodeVisitor<TArg, TResult>
    {
        public abstract TResult Visit(Program program, TArg arg);

        public abstract TResult Visit(IntegerLiteral integerLiteral, TArg arg);
        public abstract TResult Visit(StringLiteral stringLiteral, TArg arg);

        public abstract TResult Visit(Identifier identifier, TArg arg);

        public abstract TResult Visit(FunctionCall functionCall, TArg arg);
        public abstract TResult Visit(MemberAccessOperator memberAccessOperator, TArg arg);

        public abstract TResult Visit(Block block, TArg arg);
        public abstract TResult Visit(FunctionDefinition functionDefinition, TArg arg);
    }
}
