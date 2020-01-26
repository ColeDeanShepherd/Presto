using Presto.ASG;
using System.Collections.Generic;

namespace Presto
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var program = new ASG.Program();

            var mainFunction = new Function
            {
                Name = "Main",
                Parameters = new List<Parameter>(),
                ReturnType = BuiltInTypes.Unit,
                Body = new List<IStatement>
                {
                    new FunctionCall
                    {
                        Function = BuiltInFunctions.WriteLineToConsole,
                        Arguments = new List<IExpression>
                        {
                            new StringLiteral { Value = "Hello, world!" }
                        }
                    }
                }
            };
            program.Functions.Add(mainFunction);
        }
    }
}
