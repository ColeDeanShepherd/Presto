using Presto.ASG;
using System.Text;

namespace Presto.CodeGenerators
{
    public class PrestoCodeGenerator : AsgNodeVisitor<Unit, Unit>
    {
        public string GeneratedCode => stringBuilder.ToString();

        public override Unit Visit(ASG.Program program, Unit unit)
        {
            program.GlobalNamespace.Accept(this, unit);
            return unit;
        }

        public override Unit Visit(Namespace @namespace, Unit unit)
        {
            foreach (var function in @namespace.Functions)
            {
                function.Accept(this, unit);
            }

            foreach (var childNamespace in @namespace.Namespaces)
            {
                childNamespace.Accept(this, unit);
            }

            return unit;
        }

        public override Unit Visit(Function function, Unit unit)
        {
            // Skip externally-defined functions for now.
            if (function.Body == null) { return unit; }

            stringBuilder.Append("fn");
            stringBuilder.Append(' ');
            stringBuilder.Append(function.Name);
            stringBuilder.Append('(');
            stringBuilder.Append(')');
            function.Body.Accept(this, unit);

            return unit;
        }

        public override Unit Visit(Block block, Unit unit)
        {
            stringBuilder.Append('{');

            foreach (var statement in block.Statements)
            {
                statement.Accept(this, unit);
                stringBuilder.Append(';');
            }

            stringBuilder.Append('}');

            return unit;
        }

        public override Unit Visit(FunctionCall functionCall, Unit unit)
        {
            stringBuilder.Append(functionCall.Function.GetQualifiedName());
            stringBuilder.Append('(');

            for (var i = 0; i < functionCall.Arguments.Count; i++)
            {
                if (i > 0)
                {
                    stringBuilder.Append(", ");
                }

                functionCall.Arguments[i].Accept(this, unit);
            }

            stringBuilder.Append(')');

            return unit;
        }

        public override Unit Visit(IntegerLiteral integerLiteral, Unit unit)
        {
            stringBuilder.Append(integerLiteral.Value);
            return unit;
        }

        public override Unit Visit(StringLiteral stringLiteral, Unit unit)
        {
            stringBuilder.Append('"');
            stringBuilder.Append(stringLiteral.Value);
            stringBuilder.Append('"');

            return unit;
        }

        private StringBuilder stringBuilder = new StringBuilder();
    }
}
