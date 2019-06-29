using Presto.ASG;
using System.Text;

namespace Presto.CodeGenerators
{
    public class CCodeGenerator : AsgNodeVisitor<Unit, Unit>
    {
        #region Constants

        public const int SPACES_PER_INDENT = 4;
        public const string IDENTIFIER_PREFIX = "pst_";

        #endregion

        public string GeneratedCode => stringBuilder.ToString();

        public override Unit Visit(ASG.Program program, Unit unit)
        {
            stringBuilder.Append("#include <stdio.h>");
            StartNewLine();
            StartNewLine();

            stringBuilder.Append("void pst_std_io_stdout_writeLine(char* str) {");
            StartNewLine(deltaIndentationLevel: 1);
            stringBuilder.Append("printf(\"%s\", str);");
            StartNewLine(deltaIndentationLevel: -1);
            stringBuilder.Append("}");
            StartNewLine();
            StartNewLine();

            program.GlobalNamespace.Accept(this, unit);

            StartNewLine();

            stringBuilder.Append("int main() {");
            StartNewLine(deltaIndentationLevel: 1);

            stringBuilder.Append("pst_main();");
            StartNewLine();

            stringBuilder.Append("return 0;");
            StartNewLine(deltaIndentationLevel: -1);

            stringBuilder.Append("}");
            StartNewLine();

            return Unit.Instance;
        }

        public override Unit Visit(Namespace @namespace, Unit unit)
        {
            foreach (var function in @namespace.Functions)
            {
                function.Accept(this, unit);
                StartNewLine();
            }

            foreach (var childNamespace in @namespace.Namespaces)
            {
                childNamespace.Accept(this, unit);
            }

            return Unit.Instance;
        }

        public override Unit Visit(Function function, Unit unit)
        {
            // Skip externally-defined functions for now.
            if (function.Body == null) { return Unit.Instance; }

            stringBuilder.Append("void");
            stringBuilder.Append(' ');
            stringBuilder.Append(IDENTIFIER_PREFIX);
            stringBuilder.Append(function.Name);
            stringBuilder.Append('(');
            stringBuilder.Append(')');
            function.Body.Accept(this, unit);

            return Unit.Instance;
        }

        public override Unit Visit(Block block, Unit unit)
        {
            stringBuilder.Append('{');
            StartNewLine(deltaIndentationLevel: 1);

            foreach (var statement in block.Statements)
            {
                statement.Accept(this, unit);
                stringBuilder.Append(';');
                StartNewLine();
            }

            StartNewLine(deltaIndentationLevel: -1);
            stringBuilder.Append('}');

            return Unit.Instance;
        }

        public override Unit Visit(FunctionCall functionCall, Unit unit)
        {
            stringBuilder.Append(IDENTIFIER_PREFIX);
            stringBuilder.Append(string.Join('_', functionCall.Function.GetQualifiedNameParts()));
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

            return Unit.Instance;
        }

        public override Unit Visit(IntegerLiteral integerLiteral, Unit arg)
        {
            stringBuilder.Append(integerLiteral.Value);
            return Unit.Instance;
        }

        public override Unit Visit(StringLiteral stringLiteral, Unit unit)
        {
            stringBuilder.Append('"');
            stringBuilder.Append(stringLiteral.Value);
            stringBuilder.Append('"');

            return Unit.Instance;
        }

        private StringBuilder stringBuilder = new StringBuilder();
        private int indentationLevel = 0;

        #region Helper Methods

        private void StartNewLine(int deltaIndentationLevel = 0)
        {
            // TODO: preconditions

            indentationLevel += deltaIndentationLevel;

            stringBuilder.Append("\n");
            stringBuilder.Append(' ', SPACES_PER_INDENT * indentationLevel);
        }

        #endregion
    }
}
