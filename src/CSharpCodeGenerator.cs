using Presto.ASG;
using System;
using System.Collections.Generic;
using System.Text;

namespace Presto.CodeGeneration
{
    public static class CSharpCodeGenerator
    {
        public class Context
        {
            public StringBuilder StringBuilder;
            public int IndentCount;
            public List<Error> Errors;
        }

        public class Error
        {
            public ErrorType Type;
            public string Description;

            public override string ToString()
            {
                return Description;
            }
        }

        public enum ErrorType
        {
        }

        public const string Header =
@"using System;

public static class Program
{
    public static void WriteLineToConsole(string line)
    {
        Console.WriteLine(line);
    }
";

        public const string Footer = "}";
        public const string IndentationString = "    ";
        public const string NameDisambiguationPrefix = "pst_";
        public static readonly HashSet<string> ReservedNames = new HashSet<string>();

        public static (string, List<Error>) GenerateCode(ASG.Program program)
        {
            var context = new Context
            {
                StringBuilder = new StringBuilder(),
                IndentCount = 0,
                Errors = new List<Error>()
            };

            Write(context, Header);
            StartNewLine(context, deltaIndentCount: 1);

            {
                int i = 0;

                foreach (var function in program.Functions)
                {
                    if (function.Body == null) { continue; }

                    if (i > 0)
                    {
                        StartNewLine(context);
                    }

                    WriteFunction(context, function);

                    i++;
                }
            }

            StartNewLine(context, deltaIndentCount: -1);
            Write(context, Footer);

            return (context.StringBuilder.ToString(), context.Errors);
        }

        public static void WriteTypeName(Context context, IType type)
        {
            WriteName(context, type.Name);
        }

        #region Declarations

        public static void WriteFunction(Context context, Function function)
        {
            Write(context, "public static ");
            WriteTypeName(context, function.ReturnType);
            Write(context, ' ');
            WriteName(context, function.Name);
            WriteParameterTuple(context, function.Parameters);
            StartNewLine(context);
            WriteBlock(context, function.Body);
        }

        public static void WriteParameterTuple(Context context, List<Variable> parameters)
        {
            WriteTuple(context, parameters, WriteParameter);
        }

        public static void WriteParameter(Context context, Variable parameter)
        {
            WriteTypeName(context, parameter.Type);
            Write(context, ' ');
            WriteName(context, parameter.Name);
        }

        public static void WriteBlock(Context context, List<IStatement> body)
        {
            Write(context, '{');
            StartNewLine(context, deltaIndentCount: 1);

            for (int i = 0; i < body.Count; i++)
            {
                if (i > 0)
                {
                    StartNewLine(context);
                }

                WriteStatement(context, body[i]);
            }

            StartNewLine(context, deltaIndentCount: -1);
            Write(context, '}');
        }

        #endregion

        #region Statements/Expressions

        public static void WriteStatement(Context context, IStatement statement)
        {
            if (statement is IExpression)
            {
                WriteExpression(context, (IExpression)statement);
            }
            else if (statement is ReturnStatement)
            {
                WriteReturnStatement(context, (ReturnStatement)statement);
            }
            else if (statement is IfStatement)
            {
                WriteIfStatement(context, (IfStatement)statement);
            }
            else
            {
                throw new NotImplementedException($"Unknown statement type: {statement.GetType().Name}");
            }

            Write(context, ';');
        }

        public static void WriteReturnStatement(Context context, ReturnStatement returnStatement)
        {
            Write(context, "return ");
            WriteExpression(context, returnStatement.Value);
        }

        public static void WriteIfStatement(Context context, IfStatement ifStatement)
        {
            Write(context, "if (");
            WriteExpression(context, ifStatement.Condition);
            Write(context, ')');
            StartNewLine(context);
            WriteBlock(context, ifStatement.Body);
        }

        public static void WriteExpression(Context context, IExpression expression)
        {
            if (expression is IntegerLiteral)
            {
                WriteIntegerLiteral(context, (IntegerLiteral)expression);
            }
            else if (expression is StringLiteral)
            {
                WriteStringLiteral(context, (StringLiteral)expression);
            }
            else if (expression is VariableExpression)
            {
                WriteVariableExpression(context, (VariableExpression)expression);
            }
            else if (expression is FunctionCall)
            {
                WriteFunctionCall(context, (FunctionCall)expression);
            }
            else if (expression is EqualityOperator)
            {
                WriteEqualityOperator(context, (EqualityOperator)expression);
            }
            else if (expression is AdditionOperator)
            {
                WriteAdditionOperator(context, (AdditionOperator)expression);
            }
            else if (expression is SubtractionOperator)
            {
                WriteSubtractionOperator(context, (SubtractionOperator)expression);
            }
            else
            {
                throw new NotImplementedException($"Unknown expression type: {expression.GetType().Name}");
            }
        }

        public static void WriteExpressionTuple(Context context, List<IExpression> expressions)
        {
            WriteTuple(context, expressions, WriteExpression);
        }

        public static void WriteIntegerLiteral(Context context, IntegerLiteral integerLiteral)
        {
            Write(context, integerLiteral.Value.ToString());
        }

        public static void WriteStringLiteral(Context context, StringLiteral stringLiteral)
        {
            Write(context, '"');
            Write(context, stringLiteral.Value);
            Write(context, '"');
        }

        public static void WriteVariableExpression(Context context, VariableExpression variableExpression)
        {
            WriteName(context, variableExpression.Variable.Name);
        }

        public static void WriteFunctionCall(Context context, FunctionCall functionCall)
        {
            WriteFunctionExpression(context, functionCall.Function);
            WriteExpressionTuple(context, functionCall.Arguments);
        }

        public static void WriteFunctionExpression(Context context, Function function)
        {
            WriteName(context, function.Name);
        }

        public static void WriteEqualityOperator(Context context, EqualityOperator equalityOperator)
        {
            Write(context, '(');
            WriteExpression(context, equalityOperator.Left);
            Write(context, " == ");
            WriteExpression(context, equalityOperator.Right);
            Write(context, ')');
        }


        public static void WriteAdditionOperator(Context context, AdditionOperator additionOperator)
        {
            Write(context, '(');
            WriteExpression(context, additionOperator.Left);
            Write(context, " + ");
            WriteExpression(context, additionOperator.Right);
            Write(context, ')');
        }

        public static void WriteSubtractionOperator(Context context, SubtractionOperator subtractionOperator)
        {
            Write(context, '(');
            WriteExpression(context, subtractionOperator.Left);
            Write(context, " - ");
            WriteExpression(context, subtractionOperator.Right);
            Write(context, ')');
        }

        #endregion

        #region Other Helper Methods

        public static void Write(Context context, char c)
        {
            context.StringBuilder.Append(c);
        }
        public static void Write(Context context, string str)
        {
            context.StringBuilder.Append(str);
        }
        public static void StartNewLine(Context context, int deltaIndentCount = 0)
        {
            var newIndentCount = context.IndentCount + deltaIndentCount;
            Dbc.Precondition(newIndentCount >= 0);

            context.IndentCount = newIndentCount;

            Write(context, '\n');
            WriteIndentation(context);
        }
        public static void WriteIndentation(Context context)
        {
            for (int i = 0; i < context.IndentCount; i++)
            {
                context.StringBuilder.Append(IndentationString);
            }
        }

        public static void WriteName(Context context, string name)
        {
            if (ReservedNames.Contains(name))
            {
                Write(context, NameDisambiguationPrefix);
            }

            Write(context, name);
        }

        public static void WriteTuple<T>(Context context, List<T> tuple, Action<Context, T> writeElement)
        {
            Write(context, '(');

            for (int i = 0; i < tuple.Count; i++)
            {
                if (i > 0)
                {
                    Write(context, ", ");
                }

                writeElement(context, tuple[i]);
            }

            Write(context, ')');
        }

        #endregion
    }
}
