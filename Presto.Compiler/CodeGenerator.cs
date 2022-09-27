using Presto.AST;
using System.Linq.Expressions;
using System.Text;

namespace Presto;

public class CodeGenerator
{
    public string GenerateCode(Program program)
    {
        foreach (IStatement statement in program.Statements)
        {
            GenerateCode(statement);
            GenerateCode(';');
        }

        return codeBuilder.ToString();
    }

    public void GenerateCode(IStatement statement)
    {
        if (statement is LetStatement letStatement)
        {
            GenerateCode(letStatement);
        }
        else if (statement is IExpression expression)
        {
            GenerateCode(expression);
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public void GenerateCode(LetStatement letStatement)
    {
        GenerateCode(letStatement.Type);
        GenerateCode(' ');
        GenerateCode(letStatement.VariableName);
        GenerateCode(" = ");
        GenerateCode(letStatement.Value);
    }

    public void GenerateCode(IType type)
    {
        if (type is StringType)
        {
            GenerateCode("string");
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public void GenerateCode(IExpression expression)
    {
        if (expression is FunctionCall)
        {
            GenerateCode((FunctionCall)expression);
        }
        else if (expression is StringLiteral)
        {
            GenerateCode((StringLiteral)expression);
        }
        else
        {
            throw new NotImplementedException($"Unknown expression type {expression.GetType().Name}");
        }
    }

    public void GenerateCode(FunctionCall functionCall)
    {
        GenerateFullyQualifiedName(functionCall.Function.Name, functionCall.Function.ParentNamespace);
        GenerateCode('(');

        bool isFirstArg = true;
        foreach (IExpression arg in functionCall.Arguments)
        {
            if (!isFirstArg)
            {
                GenerateCode(", ");
            }

            GenerateCode(arg);

            isFirstArg = false;
        }

        GenerateCode(')');
    }

    public void GenerateFullyQualifiedName(string name, Namespace parentNamespace)
    {
        foreach (Namespace ns in ASTUtil.EnumerateNamespaceHierarchy(parentNamespace))
        {
            if (!ns.IsGlobal)
            {
                GenerateCode(ns.Name);
                GenerateCode('.');
            }
        }

        GenerateCode(name);
    }

    public void GenerateCode(StringLiteral stringLiteral)
    {
        GenerateCode('"');
        GenerateCode(stringLiteral.Value);
        GenerateCode('"');
    }

    public void GenerateCode(string s)
    {
        codeBuilder.Append(s);
    }

    public void GenerateCode(char c)
    {
        codeBuilder.Append(c);
    }

    private StringBuilder codeBuilder = new();
}