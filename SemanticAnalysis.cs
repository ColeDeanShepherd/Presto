using Presto.ASG;
using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace Presto
{
    public static class SemanticAnalysis
    {
        public class Context
        {
            public ASG.Program Program;
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
            NoValidEntryPoint,
            DuplicateFunctionDeclaration,
            InvalidFunctionCall
        }

        public static List<Error> Validate(ASG.Program program)
        {
            var context = new Context
            {
                Program = program,
                Errors = new List<Error>()
            };

            ValidateEntryPoint(context, program);
            ValidateFunctions(context, program);
            ValidateStatements(context, program);

            return context.Errors;
        }

        public static void ValidateEntryPoint(Context context, ASG.Program program)
        {
            var mainFunction = program.Functions
                .Where(f =>
                {
                    if (f.Name != "Main") { return false; }
                    if (f.ReturnType != BuiltInTypes.Unit) { return false; }
                    if (f.Parameters.Any()) { return false; }
                    if (f.Body == null) { return false; }

                    return true;
                })
                .FirstOrDefault();
            if (mainFunction == null)
            {
                context.Errors.Add(new Error
                {
                    Type = ErrorType.NoValidEntryPoint,
                    Description = "No valid \"Main\" function found."
                });
                return;
            }
        }

        public class FunctionOverload : IEquatable<FunctionOverload>
        {
            public readonly string Name;
            public readonly IReadOnlyList<IType> ParameterTypes;

            public FunctionOverload(string name, IReadOnlyList<IType> parameterTypes)
            {
                Name = name;
                ParameterTypes = parameterTypes;
            }
            public FunctionOverload(Function function)
                : this(function.Name, function.Parameters.Select(p => p.Type).ToList().AsReadOnly()) { }

            public override bool Equals(object obj)
            {
                return this.Equals(obj as FunctionOverload);
            }
            public bool Equals([AllowNull] FunctionOverload other)
            {
                if (other == null) { return false; }

                return Name.Equals(other.Name) &&
                    ParameterTypes.SequenceEqual(other.ParameterTypes);
            }

            public override int GetHashCode()
            {
                int hashCode = 486187739;

                hashCode = (hashCode * 16777619) ^ Name.GetHashCode();
                hashCode = (hashCode * 16777619) ^ ParameterTypes.GetSequenceHashCode();

                return hashCode;
            }
        }

        public static void ValidateFunctions(Context context, ASG.Program program)
        {
            var functionsByOverload = new Dictionary<FunctionOverload, Function>();

            foreach (var function in program.Functions)
            {
                var functionOverload = new FunctionOverload(function);

                if (!functionsByOverload.ContainsKey(functionOverload))
                {
                    functionsByOverload[functionOverload] = function;
                }
                else
                {
                    context.Errors.Add(new Error
                    {
                        Type = ErrorType.DuplicateFunctionDeclaration,
                        Description = $"Another function with the same name ({function.Name}) and parameter types was already defined."
                    });
                }
            }
        }

        public static void ValidateStatements(Context context, ASG.Program program)
        {
            foreach (var functionBody in program.Functions.Select(f => f.Body))
            {
                if (functionBody == null) { continue; }
                
                foreach (var statement in functionBody)
                {
                    if (statement is FunctionCall)
                    {
                        var functionCall = (FunctionCall)statement;

                        // Ensure # of function arguments matches function
                        if (functionCall.Arguments.Count != functionCall.Function.Parameters.Count)
                        {
                            context.Errors.Add(new Error
                            {
                                Type = ErrorType.InvalidFunctionCall,
                                Description = "Function call does not have the correct # of args."
                            });
                            continue;
                        }

                        // Ensure function argument types match parameter types
                        if (!functionCall.Arguments.Select(a => a.Type)
                            .SequenceEqual(functionCall.Function.Parameters.Select(p => p.Type)))
                        {
                            context.Errors.Add(new Error
                            {
                                Type = ErrorType.InvalidFunctionCall,
                                Description = "Function call has args with incorrect types."
                            });
                            continue;
                        }
                    }
                }
            }
        }
    }
}
