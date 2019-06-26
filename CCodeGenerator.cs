using Presto.AST;
using System.Text;

namespace Presto
{
    public class CCodeGenerator : AstNodeVisitor
    {
        #region Constants

        public const int SPACES_PER_INDENT = 4;
        public const string IDENTIFIER_PREFIX = "pst_";

        #endregion

        public string GeneratedCode => stringBuilder.ToString();

        public override void Visit(AST.Program program)
        {
            stringBuilder.Append("#include <stdio.h>");
            StartNewLine();
            StartNewLine();

            foreach (var definition in program.Definitions)
            {
                definition.Accept(this);
                StartNewLine();
            }

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
            stringBuilder.Append(IDENTIFIER_PREFIX);
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

        public override void Visit(FunctionDefinition functionDefinition)
        {
            stringBuilder.Append("void");
            stringBuilder.Append(' ');
            functionDefinition.Name.Accept(this);
            stringBuilder.Append('(');
            stringBuilder.Append(')');
            stringBuilder.Append(' ');
            functionDefinition.Body.Accept(this);
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
