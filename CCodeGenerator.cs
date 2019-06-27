using Presto.ASG;
using System.Text;

namespace Presto.CodeGenerators
{
    public class CCodeGenerator : AsgNodeVisitor
    {
        #region Constants

        public const int SPACES_PER_INDENT = 4;
        public const string IDENTIFIER_PREFIX = "pst_";

        #endregion

        public string GeneratedCode => stringBuilder.ToString();

        public override void Visit(ASG.Program program)
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

            program.GlobalNamespace.Accept(this);

            StartNewLine();

            stringBuilder.Append("int main() {");
            StartNewLine(deltaIndentationLevel: 1);

            stringBuilder.Append("pst_main();");
            StartNewLine();

            stringBuilder.Append("return 0;");
            StartNewLine(deltaIndentationLevel: -1);

            stringBuilder.Append("}");
            StartNewLine();



            
        }

        public override void Visit(Namespace @namespace)
        {
            foreach (var function in @namespace.Functions)
            {
                function.Accept(this);
                StartNewLine();
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

            stringBuilder.Append("void");
            stringBuilder.Append(' ');
            stringBuilder.Append(IDENTIFIER_PREFIX);
            stringBuilder.Append(function.Name);
            stringBuilder.Append('(');
            stringBuilder.Append(')');
            function.Body.Accept(this);
        }

        public override void Visit(Block block)
        {
            stringBuilder.Append('{');
            StartNewLine(deltaIndentationLevel: 1);

            foreach (var statement in block.Statements)
            {
                statement.Accept(this);
                stringBuilder.Append(';');
                StartNewLine();
            }

            StartNewLine(deltaIndentationLevel: -1);
            stringBuilder.Append('}');
        }

        public override void Visit(FunctionCall functionCall)
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
