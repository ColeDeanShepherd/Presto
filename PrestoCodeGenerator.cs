using Presto.ASG;
using System.Text;

namespace Presto.CodeGenerators
{
    public class PrestoCodeGenerator : AsgNodeVisitor
    {
        public string GeneratedCode => stringBuilder.ToString();

        public override void Visit(ASG.Program program)
        {
            program.GlobalNamespace.Accept(this);
        }

        public override void Visit(Namespace @namespace)
        {
            foreach (var function in @namespace.Functions)
            {
                function.Accept(this);
            }

            foreach (var childNamespace in @namespace.Namespaces)
            {
                childNamespace.Accept(this);
            }
        }

        public override void Visit(Function function)
        {
            // Skip externally-defined functions for now.
            if (function.Body == null) { return; }

            stringBuilder.Append("fn");
            stringBuilder.Append(' ');
            stringBuilder.Append(function.Name);
            stringBuilder.Append('(');
            stringBuilder.Append(')');
            function.Body.Accept(this);
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

        public override void Visit(FunctionCall functionCall)
        {
            stringBuilder.Append(functionCall.Function.GetQualifiedName());
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

        public override void Visit(StringLiteral stringLiteral)
        {
            stringBuilder.Append('"');
            stringBuilder.Append(stringLiteral.Value);
            stringBuilder.Append('"');
        }

        
        private StringBuilder stringBuilder = new StringBuilder();
    }
}
