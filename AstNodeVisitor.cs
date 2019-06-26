namespace Presto.AST
{
    public abstract class AstNodeVisitor
    {
        public abstract void Visit(Program program);

        public abstract void Visit(IntegerLiteral integerLiteral);
        public abstract void Visit(StringLiteral stringLiteral);

        public abstract void Visit(Identifier identifier);

        public abstract void Visit(FunctionCall functionCall);
        public abstract void Visit(MemberAccessOperator memberAccessOperator);

        public abstract void Visit(Block block);
        public abstract void Visit(FunctionDefinition functionDefinition);
    }
}
