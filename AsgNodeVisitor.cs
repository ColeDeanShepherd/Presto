namespace Presto.ASG
{
    public abstract class AsgNodeVisitor<TArg, TResult>
    {
        public abstract TResult Visit(Program program, TArg arg);
        public abstract TResult Visit(Namespace @namespace, TArg arg);
        public abstract TResult Visit(Function function, TArg arg);
        public abstract TResult Visit(Block block, TArg arg);
        public abstract TResult Visit(FunctionCall functionCall, TArg arg);
        public abstract TResult Visit(IntegerLiteral integerLiteral, TArg arg);
        public abstract TResult Visit(StringLiteral stringLiteral, TArg arg);
    }
}
