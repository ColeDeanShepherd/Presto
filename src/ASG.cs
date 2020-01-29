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
            Parameters = new List<Variable>
            {
                new Variable
                {
                    Name = "line",
                    Type = BuiltInTypes.String
                }
            },
            ReturnType = BuiltInTypes.Unit
        };
        public static readonly Function Int32ToString = new Function
        {
            Name = "ToString",
            Parameters = new List<Variable>
            {
                new Variable
                {
                    Name = "x",
                    Type = BuiltInTypes.Int32
                }
            },
            ReturnType = BuiltInTypes.String
        };
        public static readonly Function Length = new Function
        {
            Name = "Length",
            Parameters = new List<Variable>
            {
                new Variable
                {
                    Name = "list",
                    Type = new ListType(BuiltInTypes.Int32)
                }
            },
            ReturnType = BuiltInTypes.Int32
        };

        public static readonly List<Function> All = new List<Function>
        {
            WriteLineToConsole,
            Int32ToString,
            Length
        };
    }

    public class Function
    {
        public string Name;
        public List<Variable> Parameters;
        public IType ReturnType;
        public List<IStatement> Body;
    }

    public class Variable
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
        public static readonly IntegerType Int32 = new IntegerType();
        public static readonly BooleanType Bool = new BooleanType();
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

    public class BooleanType : IType
    {
        public string Name => "bool";

        public override bool Equals(object obj)
        {
            return this.Equals(obj as IType);
        }
        public bool Equals([AllowNull] IType type)
        {
            if (type == null) { return false; }
            if (!(type is BooleanType)) { return false; }
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

    public class IntegerType : IType
    {
        public string Name => "int";

        public override bool Equals(object obj)
        {
            return this.Equals(obj as IType);
        }
        public bool Equals([AllowNull] IType type)
        {
            if (type == null) { return false; }
            if (!(type is IntegerType)) { return false; }
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

    public class ListType : IType
    {
        public string Name => $"List<{ElementType.Name}>";
        public readonly IType ElementType;

        public ListType(IType elementType)
        {
            ElementType = elementType;
        }

        public override bool Equals(object obj)
        {
            return this.Equals(obj as IType);
        }
        public bool Equals([AllowNull] IType type)
        {
            if (type == null) { return false; }
            if (!(type is ListType)) { return false; }

            var other = (ListType)type;
            return ElementType.Equals(other.ElementType);
        }

        public override int GetHashCode()
        {
            int hashCode = GetType().GetHashCode();
            hashCode = (hashCode * 16777619) ^ ElementType.GetHashCode();
            return hashCode;
        }
    }

    #endregion

    #region Statements & Expressions

    public interface IStatement { }

    public class ReturnStatement : IStatement
    {
        public IExpression Value;
    }

    public class IfStatement : IStatement
    {
        public IExpression Condition;
        public List<IStatement> Body;
    }

    public class DoWhileStatement : IStatement
    {
        public List<IStatement> Body;
        public IExpression Condition;
    }

    public class ForLoopStatement : IStatement
    {
        public IStatement PreStatement;
        public IExpression Condition;
        public IStatement PostIterationStatement;
        public List<IStatement> Body;
    }

    public class VariableDeclaration : IStatement
    {
        public Variable Variable;
        public IExpression InitialValue;
    }

    public class VariableAssignment : IStatement
    {
        public Variable Variable;
        public IExpression Value;
    }

    public interface IExpression : IStatement
    {
        public IType Type { get; }
    }

    public class UnitLiteral : IExpression
    {
        public IType Type => BuiltInTypes.Unit;
    }

    public class IntegerLiteral : IExpression
    {
        public int Value;

        public IType Type => BuiltInTypes.Int32;
    }

    public class BooleanLiteral : IExpression
    {
        public bool Value;

        public IType Type => BuiltInTypes.Bool;
    }

    public class StringLiteral : IExpression
    {
        public string Value;

        public IType Type => BuiltInTypes.String;
    }

    public class EqualityOperator : IExpression
    {
        public IExpression Left;
        public IExpression Right;

        public IType Type
        {
            get
            {
                // TODO: move to type checker
                Dbc.Precondition(Left.Type.Equals(Right.Type));

                return BuiltInTypes.Bool;
            }
        }
    }

    public class LessThanOperator : IExpression
    {
        public IExpression Left;
        public IExpression Right;

        public IType Type
        {
            get
            {
                // TODO: move to type checker
                Dbc.Precondition(Left.Type.Equals(Right.Type));

                return BuiltInTypes.Bool;
            }
        }
    }

    public class LessThanOrEqualToOperator : IExpression
    {
        public IExpression Left;
        public IExpression Right;

        public IType Type
        {
            get
            {
                // TODO: move to type checker
                Dbc.Precondition(Left.Type.Equals(Right.Type));

                return BuiltInTypes.Bool;
            }
        }
    }

    public class AdditionOperator : IExpression
    {
        public IExpression Left;
        public IExpression Right;

        public IType Type
        {
            get
            {
                // TODO: move to type checker
                Dbc.Precondition(Left.Type.Equals(Right.Type));

                return Left.Type;
            }
        }
    }

    public class SubtractionOperator : IExpression
    {
        public IExpression Left;
        public IExpression Right;

        public IType Type
        {
            get
            {
                // TODO: move to type checker
                Dbc.Precondition(Left.Type.Equals(Right.Type));

                return Left.Type;
            }
        }
    }

    public class VariableExpression : IExpression
    {
        public Variable Variable;

        public IType Type => Variable.Type;
    }

    public class FunctionCall : IStatement, IExpression
    {
        public Function Function;
        public List<IExpression> Arguments;

        public IType Type => Function.ReturnType;
    }

    // ensure variable references have valid names
    // ensure if statements have boolean expressions
    // ensure functions return values if not unit return type
    // ensure return values match return type
    // ensure binary operators operate on same type
    // ensure var. decl. have unique names (in scope)
    // ensure function parameters have diff names
    // ensure function calls refer to valid functions

    #endregion
}
