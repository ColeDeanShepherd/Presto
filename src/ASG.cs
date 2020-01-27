using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

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

    public interface IType : IEquatable<IType>
    {
        public string Name { get; }
    }

    public static class BuiltInTypes
    {
        public static readonly UnitType Unit = new UnitType();
        public static readonly StringType String = new StringType();
    }

    public class UnitType : IType
    {
        public string Name => "void";

        public override bool Equals(object obj)
        {
            return this.Equals(obj as IType);
        }
        public bool Equals([AllowNull] IType type)
        {
            if (type == null) { return false; }
            if (!(type is UnitType)) { return false; }
            return true;
        }

        public override int GetHashCode() => GetType().GetHashCode();
    }

    public class StringType : IType
    {
        public string Name => "String";

        public override bool Equals(object obj)
        {
            return this.Equals(obj as IType);
        }
        public bool Equals([AllowNull] IType type)
        {
            if (type == null) { return false; }
            if (!(type is StringType)) { return false; }
            return true;
        }

        public override int GetHashCode() => GetType().GetHashCode();
    }

    public class FunctionType : IType
    {
        public string Name => $"({string.Join(", ", ParameterTypes.Select(t => t.Name))}) => {ReturnType.Name}";
        public readonly IType ReturnType;
        public readonly IReadOnlyList<IType> ParameterTypes;

        public FunctionType(IType returnType, IReadOnlyList<IType> parameterTypes)
        {
            ReturnType = returnType;
            ParameterTypes = parameterTypes;
        }
        public FunctionType(Function function)
            : this(function.ReturnType, function.Parameters.Select(p => p.Type).ToList().AsReadOnly()) { }

        public override bool Equals(object obj)
        {
            return this.Equals(obj as IType);
        }
        public bool Equals([AllowNull] IType type)
        {
            if (type == null) { return false; }
            if (!(type is FunctionType)) { return false; }

            var other = (FunctionType)type;
            return ReturnType.Equals(other.ReturnType) &&
                ParameterTypes.SequenceEqual(other.ParameterTypes);
        }

        public override int GetHashCode()
        {
            int hashCode = 486187739;

            hashCode = (hashCode * 16777619) ^ ReturnType.GetHashCode();
            hashCode = (hashCode * 16777619) ^ ParameterTypes.GetSequenceHashCode();

            return hashCode;
        }
    }

    #endregion

    #region Statements & Expressions

    public interface IStatement { }
    public interface IExpression : IStatement
    {
        public IType Type { get; }
    }

    public class UnitLiteral : IExpression
    {
        public IType Type => BuiltInTypes.Unit;
    }

    public class StringLiteral : IExpression
    {
        public string Value;

        public IType Type => BuiltInTypes.String;
    }

    public class FunctionCall : IStatement, IExpression
    {
        public Function Function;
        public List<IExpression> Arguments;

        public IType Type => new FunctionType(Function);
    }

    #endregion
}
