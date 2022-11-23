using Presto.AST;
using System.Text;

namespace Presto;

public class CodeGenerator
{
    public string GenerateCode(Program program)
    {
        foreach (IStatement statement in program.Statements)
        {
            GenerateCode(statement);

            if (DoesStatementRequireSemicolonDelimiter(statement))
            {
                GenerateCode(';');
            }
        }

        return codeBuilder.ToString();
    }

    public void GenerateCode(IStatement statement)
    {
        if (statement is LetStatement letStatement)
        {
            GenerateCode(letStatement);
        }
        else if (statement is Function function)
        {
            GenerateCode(function);
        }
        else if (statement is StructDefinition structDefinition)
        {
            GenerateCode(structDefinition);
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

    public void GenerateCode(Function function)
    {
        // TODO: return type
        GenerateCode("void");
        GenerateCode(' ');
        GenerateCode(function.Name);
        GenerateCode('(');
        GenerateStringSeparated(function.ParameterDeclarations, x => GenerateCode(x), ", ");
        GenerateCode(')');
        GenerateCode(' ');
        GenerateCode('{');
        GenerateCode(' ');
        GenerateCode('}');
    }

    public void GenerateCode(ParameterDeclaration paramDecl)
    {
        GenerateCode(paramDecl.Type);
        GenerateCode(' ');
        GenerateCode(paramDecl.Name);
    }

    public void GenerateCode(StructDefinition structDefinition)
    {
        GenerateCode("class ");
        GenerateCode(structDefinition.StructName);
        GenerateCode(" { ");
        GenerateStringDelimited(structDefinition.FieldDefinitions, x => GenerateCode(x), "; ");
        GenerateCode('}');
    }

    public void GenerateCode(FieldDefinition fieldDefinition)
    {
        GenerateCode("public ");
        GenerateCode(fieldDefinition.FieldType);
        GenerateCode(' ');
        GenerateCode(fieldDefinition.FieldName);
    }

    public void GenerateCode(IType type)
    {
        if (type is StringType)
        {
            GenerateCode("string");
        }
        else if (type is BoolType)
        {
            GenerateCode("bool");
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public void GenerateCode(IExpression expression)
    {
        if (expression is NumberLiteral number)
        {
            GenerateCode(number);
        }
        else if (expression is StringLiteral)
        {
            GenerateCode((StringLiteral)expression);
        }
        else if (expression is VariableReference variableReference)
        {
            GenerateCode(variableReference);
        }
        else if (expression is FunctionCall)
        {
            GenerateCode((FunctionCall)expression);
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
        GenerateStringSeparated(functionCall.Arguments, arg => GenerateCode(arg), ", ");
        GenerateCode(')');
    }

    public void GenerateStringSeparated<TNode>(List<TNode> nodes, Action<TNode> generateNodeCode, string separator)
    {
        bool isFirstNode = true;
        foreach (TNode node in nodes)
        {
            if (!isFirstNode)
            {
                GenerateCode(separator);
            }
            else
            {
                isFirstNode = false;
            }

            generateNodeCode(node);
        }
    }

    public void GenerateStringDelimitedAndSeparated<TNode>(List<TNode> nodes, Action<TNode> generateNodeCode, string delimiter, string separator)
    {
        bool isFirstNode = true;
        foreach (TNode node in nodes)
        {
            if (!isFirstNode)
            {
                GenerateCode(separator);
            }
            else
            {
                isFirstNode = false;
            }

            generateNodeCode(node);
            GenerateCode(delimiter);
        }
    }

    public void GenerateStringDelimited<TNode>(List<TNode> nodes, Action<TNode> generateNodeCode, string delimiter)
    {
        foreach (TNode node in nodes)
        {
            generateNodeCode(node);
            GenerateCode(delimiter);
        }
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

    public void GenerateCode(NumberLiteral number)
    {
        GenerateCode(number.ValueAsString);
    }

    public void GenerateCode(StringLiteral stringLiteral) =>
        GenerateCode(stringLiteral.Value);

    public void GenerateCode(VariableReference variableReference)
    {
        GenerateCode(variableReference.LetStatement.VariableName);
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

    private bool DoesStatementRequireSemicolonDelimiter(IStatement statement)
    {
        if (statement is StructDefinition)
        {
            return false;
        }
        else
        {
            return true;
        }
    }
}