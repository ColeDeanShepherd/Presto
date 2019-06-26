using Presto.AST;
using System.Text;

namespace Presto
{
    public class PrestoCodeGenerator : AstNodeVisitor
    {
        public string GeneratedCode => stringBuilder.ToString();

        public override void Visit(AST.Program program)
        {
            foreach (var definition in program.Definitions)
            {
                definition.Accept(this);
            }
        }

        public override void Visit(IntegerLiteral integerLiteral)
        {
            stringBuilder.Append(integerLiteral.Value);
        }

        public override void Visit(StringLiteral stringLiteral)
        {
            stringBuilder.Append('"');
            stringBuilder.Append(stringLiteral.Value);
            stringBuilder.Append('"');
        }

        public override void Visit(Identifier identifier)
        {
            stringBuilder.Append(identifier.Text);
        }

        public override void Visit(FunctionCall functionCall)
        {
            functionCall.FunctionExpression.Accept(this);
            stringBuilder.Append('(');

            for (var i = 0; i < functionCall.Arguments.Count; i++)
            {
                if (i > 0)
                {
                    stringBuilder.Append(", ");
                }

                functionCall.Arguments[i].Accept(this);
            }

            stringBuilder.Append(')');
        }

        public override void Visit(MemberAccessOperator memberAccessOperator)
        {
            memberAccessOperator.MemberContainer.Accept(this);
            stringBuilder.Append('.');
            memberAccessOperator.MemberIdentifier.Accept(this);
        }

        public override void Visit(Block block)
        {
            stringBuilder.Append('{');

            foreach (var statement in block.Statements)
            {
                statement.Accept(this);
                stringBuilder.Append(';');
            }

            stringBuilder.Append('}');
        }

        public override void Visit(FunctionDefinition functionDefinition)
        {
            stringBuilder.Append("fn");
            stringBuilder.Append(' ');
            functionDefinition.Name.Accept(this);
            stringBuilder.Append('(');
            stringBuilder.Append(')');
            functionDefinition.Body.Accept(this);
        }

        private StringBuilder stringBuilder = new StringBuilder();
    }
}
