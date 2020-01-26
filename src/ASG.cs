using System.Collections.Generic;

namespace Presto.ASG
{
    #region Declarations

    public class Program
    {
        public Program()
        {
            Functions = new List<Function>(BuiltInFunctions.All);
        }

        public List<Function> Functions;
    }

    public static class BuiltInFunctions
    {
        public static readonly Function WriteLineToConsole = new Function
        {
            Name = "WriteLineToConsole",
            Parameters = new List<Parameter>
            {
                new Parameter
                {
                    Name = "line",
                    Type = BuiltInTypes.String
                }
            },
            ReturnType = BuiltInTypes.Unit
        };

        public static readonly List<Function> All = new List<Function>
        {
            WriteLineToConsole
        };
    }

    public class Function
    {
        public string Name;
        public List<Parameter> Parameters;
        public IType ReturnType;
        public List<IStatement> Body;
    }

    public class Parameter
    {
        public string Name;
        public IType Type;
    }

    #endregion

    #region Types

    public interface IType { }

    public static class BuiltInTypes
    {
        public static readonly UnitType Unit = new UnitType();
        public static readonly StringType String = new StringType();
    }

    public class UnitType : IType { }

    public class StringType : IType { }

    #endregion

    #region Statements & Expressions

    public interface IStatement { }
    public interface IExpression : IStatement { }

    public class FunctionCall : IStatement, IExpression
    {
        public Function Function;
        public List<IExpression> Arguments;
    }

    public class StringLiteral : IExpression
    {
        public string Value;
    }

    #endregion
}
