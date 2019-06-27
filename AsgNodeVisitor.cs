namespace Presto.ASG
{
    public abstract class AsgNodeVisitor
    {
        public abstract void Visit(Program program);
        public abstract void Visit(Namespace @namespace);
        public abstract void Visit(Function function);
        public abstract void Visit(Block block);
        public abstract void Visit(FunctionCall functionCall);
        public abstract void Visit(StringLiteral stringLiteral);
    }
}
