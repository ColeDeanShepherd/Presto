using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Presto.ASG
{
    public abstract class Type { }
    public class IntegerType : Type
    {
        public readonly bool IsUnsigned;
        public readonly uint NumBytes;

        public IntegerType(bool isUnsigned, uint numBytes)
        {
            IsUnsigned = isUnsigned;
            NumBytes = numBytes;
        }
    }
    public class StringType : Type { }
    public class FunctionType : Type
    {
        public readonly IReadOnlyCollection<Type> ParameterTypes;
        public readonly Function Function; // optional

        public FunctionType(IReadOnlyCollection<Type> parameterTypes, Function function = null)
        {
            ParameterTypes = parameterTypes;
            Function = function;
        }
    }

    public interface INode
    {
        INode ParentNode { get; }
        IEnumerable<INode> ChildNodes { get; }

        TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg);
    }
    public interface IDeclaration : INode
    {
        string Name { get; }
        Type Type { get; }
    }
    public interface IStatement : INode
    {
        new INode ParentNode { get; set; }
    }
    public interface IExpression : INode
    {
        new INode ParentNode { get; set; }
        Type Type { get; set; }
    }
    public interface IScope : INode { }

    public static class INodeHelpers
    {
        public static T FindThisOrAncestorOfType<T>(this INode node) where T : class
        {
            var thisAsT = node as T;
            return (thisAsT != null)
                ? thisAsT
                : FindAncestorOfType<T>(node);
        }
        public static T FindAncestorOfType<T>(this INode node) where T : class
        {
            if (node.ParentNode == null) { return null; }

            var parentAsT = node.ParentNode as T;
            return (parentAsT != null)
                ? parentAsT
                : FindAncestorOfType<T>(node.ParentNode);
        }
        public static IScope GetScope(this INode node)
        {
            return node.FindThisOrAncestorOfType<IScope>();
        }

        public static List<string> GetQualifiedNameParts(this IDeclaration declaration)
        {
            var qualifiedNameParts = new List<string> { declaration.Name };
            IDeclaration parentDeclaration = declaration;

            while (((parentDeclaration = parentDeclaration.FindAncestorOfType<IDeclaration>()) != null) &&
                (parentDeclaration.Name != null))
            {
                qualifiedNameParts.Insert(0, parentDeclaration.Name);
            }

            return qualifiedNameParts;
        }
        public static string GetQualifiedName(this IDeclaration declaration)
        {
            return string.Join('.', GetQualifiedNameParts(declaration));
        }

        public static IDeclaration FindDeclaration(this IScope scope, string name)
        {
            if (string.IsNullOrEmpty(name)) { throw new ArgumentOutOfRangeException(nameof(name)); }

            // Try to find a child declaration with the name.
            var childDeclaration = scope.ChildNodes
                .Select(child => child as IDeclaration)
                .FirstOrDefault(declaration => declaration?.Name == name);
            if (childDeclaration != null) { return childDeclaration; }

            // Try to find an ancestor declaration with the name.
            var parentScope = FindAncestorOfType<IScope>(scope);
            return (parentScope != null)
                ? parentScope.FindDeclaration(name)
                : null;
        }
        public static IDeclaration FindDeclaration(this IScope scope, IEnumerable<string> nameParts)
        {
            if (!nameParts.Any()) { throw new ArgumentOutOfRangeException(nameof(nameParts)); }

            var declaration = scope.FindDeclaration(nameParts.First());
            if (declaration == null) { return null; }

            var remainingNameParts = nameParts.Skip(1);
            if (!remainingNameParts.Any()) { return declaration; }

            var nextScope = FindThisOrAncestorOfType<IScope>(declaration);
            return nextScope.FindDeclaration(remainingNameParts);
        }
    }

    public class Program : INode
    {
        #region Constants

        public readonly IntegerType Int32Type = new IntegerType(isUnsigned: false, numBytes: 4);
        public readonly StringType StringType = new StringType();

        #endregion

        #region INode

        public INode ParentNode => null;
        public IEnumerable<INode> ChildNodes => IEnumerableExtensions.AsEnumerable<INode>(GlobalNamespace);

        public TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }

        #endregion

        public Namespace GlobalNamespace;
        public List<Type> Types;

        public Program()
        {
            Types = new List<Type>();
            Types.Add(Int32Type);
            Types.Add(StringType);
        }
    }

    public class Namespace : IDeclaration, IScope
    {
        #region INode

        public INode ParentNode => Parent;
        public IEnumerable<INode> ChildNodes =>
            IEnumerableExtensions.Concat(Namespaces.Cast<INode>(), Functions.Cast<INode>());

        public TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }

        #endregion

        #region IDeclaration

        public string Name { get; set; }
        public Type Type => null;

        #endregion

        public Namespace Parent;

        public List<Namespace> Namespaces;
        public List<Function> Functions;

        public Namespace(string name)
        {
            Name = name;
            Namespaces = new List<Namespace>();
            Functions = new List<Function>();
        }
        public void SetParent(Namespace value)
        {
            // remove from old parent
            if (Parent != null)
            {
                Parent.Namespaces.Remove(this);
            }

            // add to new parent
            Parent = value;

            if (Parent != null)
            {
                Parent.Namespaces.Add(this);
            }
        }
    }

    public class Function : IDeclaration, IScope
    {
        #region INode

        public INode ParentNode => Parent;
        public IEnumerable<INode> ChildNodes => IEnumerableExtensions.AsEnumerable<INode>(Body);

        public TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }

        #endregion

        #region IDeclaration

        public string Name { get; set; }

        #endregion

        public Namespace Parent;
        public List<FunctionParameter> Parameters;
        // TODO: return type
        public Block Body; // nullable
        public Type Type { get; set; }

        public Function(string name)
        {
            Name = name;
            Parameters = new List<FunctionParameter>();
        }
        public void SetParent(Namespace value)
        {
            // remove from old parent
            if (Parent != null)
            {
                Parent.Functions.Remove(this);
            }

            // add to new parent
            Parent = value;

            if (Parent != null)
            {
                Parent.Functions.Add(this);
            }
        }
    }

    public class FunctionParameter : IDeclaration
    {
        #region INode

        public INode ParentNode => Parent;
        public IEnumerable<INode> ChildNodes => Enumerable.Empty<INode>();

        public TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }

        #endregion

        #region IDeclaration

        public string Name { get; set; }

        #endregion

        public Function Parent;
        public Type Type { get; set; }

        public FunctionParameter(string name)
        {
            Name = name;
        }
        public void SetParent(Function value)
        {
            // remove from old parent
            if (Parent != null)
            {
                Parent.Parameters.Remove(this);
            }

            // add to new parent
            Parent = value;

            if (Parent != null)
            {
                Parent.Parameters.Add(this);
            }
        }
    }

    public class Block : INode, IScope
    {
        #region INode

        public INode ParentNode { get; set; }
        public IEnumerable<INode> ChildNodes => Statements.Cast<INode>();

        public TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }

        #endregion

        public List<IStatement> Statements;

        public Block()
        {
            Statements = new List<IStatement>();
        }
    }

    public class FunctionCall : IStatement, IExpression
    {
        #region INode

        public INode ParentNode { get; set; }
        public IEnumerable<INode> ChildNodes => Arguments.Cast<INode>();

        public TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }

        #endregion

        public Function Function;
        public List<IExpression> Arguments;
        public Type Type { get; set; }

        public FunctionCall()
        {
            Arguments = new List<IExpression>();
        }
    }

    public class IntegerLiteral : IExpression
    {
        #region INode

        public INode ParentNode { get; set; }
        public IEnumerable<INode> ChildNodes => Enumerable.Empty<INode>();

        public TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }

        #endregion

        public int Value;
        public Type Type { get; set; }
    }

    public class StringLiteral : IExpression
    {
        #region INode

        public INode ParentNode { get; set; }
        public IEnumerable<INode> ChildNodes => Enumerable.Empty<INode>();

        public TResult Accept<TArg, TResult>(AsgNodeVisitor<TArg, TResult> visitor, TArg arg)
        {
            return visitor.Visit(this, arg);
        }

        #endregion

        public string Value;
        public Type Type { get; set; }
    }

    public class AsgBuilderDeclarationsPass : AST.AstNodeVisitor<INode, INode>
    {
        public AsgBuilderDeclarationsPass(List<string> errors)
        {
            this.errors = errors;
        }

        #region AstNodeVisitor<INode>

        public override INode Visit(AST.Program astProgram, INode parent)
        {
            Debug.Assert(astProgram != null);

            program = CreateProgramWithStdLib();

            foreach (var astDefinition in astProgram.Definitions)
            {
                var node = astDefinition.Accept(this, program.GlobalNamespace);

                if (node is Function)
                {
                    var function = node as Function;
                    function.SetParent(program.GlobalNamespace);
                }
                else
                {
                    errors.Add($"Unexpected AST node type: {node.GetType().Name}");
                }
            }

            return program;
        }

        public override INode Visit(AST.FunctionDefinition astFunctionDefinition, INode parent)
        {
            Debug.Assert(astFunctionDefinition != null);

            var function = new Function(astFunctionDefinition.Name.Text);
            return function;

            var node = astFunctionDefinition.Body.Accept(this, function);
            var body = node as Block;
            if (body == null)
            {
                errors.Add($"Unexpected AST node type: {node.GetType().Name}");
            }

            body.ParentNode = function;
            function.Body = body;

            return function;
        }

        public override INode Visit(AST.Block astBlock, INode parent)
        {
            Debug.Assert(astBlock != null);

            var block = new Block
            {
                Statements = new List<IStatement>()
            };

            foreach (var astStatement in astBlock.Statements)
            {
                var node = astStatement.Accept(this, block);
                var statement = node as IStatement;
                if (node != null)
                {
                    statement.ParentNode = block;
                    block.Statements.Add(statement);
                }
                else
                {
                    errors.Add($"Unexpected AST node type: {node.GetType().Name}");
                }
            }

            return block;
        }

        public override INode Visit(AST.FunctionCall astFunctionCall, INode parent)
        {
            Debug.Assert(astFunctionCall != null);

            var functionCall = new FunctionCall();
            functionCall.Function = program.GlobalNamespace.FindDeclaration(new[] { "std", "io", "stdout", "writeLine" }) as Function;

            return functionCall;
        }

        public override INode Visit(AST.MemberAccessOperator astMemberAccessOperator, INode parent)
        {
            return null;

            /*Debug.Assert(astMemberAccessOperator != null);

            var memberContainerNode = astMemberAccessOperator.MemberContainer.Accept(this, parent);

            var memberDeclarationNode = astMemberAccessOperator.MemberIdentifier.Accept(this, parent);
            var memberDeclaration = memberDeclarationNode as IDeclaration;
            if (memberDeclaration == null)
            {
                errors.Add($"Unexpected AST node type: {memberDeclarationNode.GetType().Name}");
            }*/
        }

        public override INode Visit(AST.Identifier identifier, INode parent)
        {
            Debug.Assert(identifier != null);

            var scope = parent.FindThisOrAncestorOfType<IScope>();
            return scope.FindDeclaration(identifier.Text);
        }

        public override INode Visit(AST.IntegerLiteral astIntegerLiteral, INode parent)
        {
            var integerLiteral = new IntegerLiteral();
            integerLiteral.Value = astIntegerLiteral.Value;
            return integerLiteral;
        }

        public override INode Visit(AST.StringLiteral astStringLiteral, INode parent)
        {
            var stringLiteral = new StringLiteral();
            stringLiteral.Value = astStringLiteral.Value;
            return stringLiteral;
        }
        
        #endregion

        private Program program;
        private List<string> errors;

        #region Helper Methods

        private Program CreateProgramWithStdLib()
        {
            var program = new ASG.Program();

            var globalNamespace = new Namespace(name: null);
            program.GlobalNamespace = globalNamespace;

            var stdNamespace = new Namespace("std");
            stdNamespace.SetParent(globalNamespace);

            var stdIoNamespace = new Namespace("io");
            stdIoNamespace.SetParent(stdNamespace);

            var stdIoStdOutNamespace = new Namespace("stdout");
            stdIoStdOutNamespace.SetParent(stdIoNamespace);

            var writeLineFunction = new Function("writeLine");
            var writeLineStrParameter = new FunctionParameter("str");
            writeLineStrParameter.Type = program.StringType;
            writeLineFunction.Parameters.Add(writeLineStrParameter);
            writeLineFunction.Type = new FunctionType(new List<Type> { program.StringType }, writeLineFunction);
            writeLineFunction.SetParent(stdIoStdOutNamespace);

            return program;
        }

        #endregion
    }

    public class AsgBuilderCreateDeclarationTypesPass : AST.AstNodeVisitor<INode, INode>
    {
        public AsgBuilderCreateDeclarationTypesPass(Program program, List<string> errors)
        {
            this.program = program;
            this.errors = errors;
        }

        #region AstNodeVisitor<INode>

        public override INode Visit(AST.Program astProgram, INode parent)
        {
            Debug.Assert(astProgram != null);

            foreach (var astDefinition in astProgram.Definitions)
            {
                var node = astDefinition.Accept(this, program.GlobalNamespace);
            }

            return program;
        }

        public override INode Visit(AST.FunctionDefinition astFunctionDefinition, INode parent)
        {
            Debug.Assert(astFunctionDefinition != null);

            var scope = parent.GetScope();
            var function = scope.FindDeclaration(astFunctionDefinition.Name.Text) as Function;
            var parameterTypes = new List<Type>(); // TODO: implement
            function.Type = new FunctionType(parameterTypes, function);
            return function;
        }

        public override INode Visit(AST.Block astBlock, INode parent)
        {
            Debug.Assert(astBlock != null);
            return null;
        }

        public override INode Visit(AST.FunctionCall astFunctionCall, INode parent)
        {
            Debug.Assert(astFunctionCall != null);
            return null;
        }

        public override INode Visit(AST.MemberAccessOperator astMemberAccessOperator, INode parent)
        {
            Debug.Assert(astMemberAccessOperator != null);
            return null;
        }

        public override INode Visit(AST.Identifier identifier, INode parent)
        {
            Debug.Assert(identifier != null);
            return null;
        }

        public override INode Visit(AST.IntegerLiteral astIntegerLiteral, INode parent)
        {
            Debug.Assert(astIntegerLiteral != null);
            return null;
        }

        public override INode Visit(AST.StringLiteral astStringLiteral, INode parent)
        {
            Debug.Assert(astStringLiteral != null);
            return null;
        }

        #endregion

        private Program program;
        private List<string> errors;
    }

    public class AsgBuilderFunctionBodiesPass : AST.AstNodeVisitor<INode, INode>
    {
        public AsgBuilderFunctionBodiesPass(Program program, List<string> errors)
        {
            this.program = program;
            this.errors = errors;
        }

        #region AstNodeVisitor<INode>

        public override INode Visit(AST.Program astProgram, INode parent)
        {
            Debug.Assert(astProgram != null);

            foreach (var astDefinition in astProgram.Definitions)
            {
                astDefinition.Accept(this, program.GlobalNamespace);
            }

            return program;
        }

        public override INode Visit(AST.FunctionDefinition astFunctionDefinition, INode parent)
        {
            Debug.Assert(astFunctionDefinition != null);

            var scope = parent.FindThisOrAncestorOfType<IScope>();
            var function = scope.FindDeclaration(astFunctionDefinition.Name.Text) as Function;

            var body = astFunctionDefinition.Body.Accept(this, function) as Block;
            body.ParentNode = function;
            function.Body = body;

            return function;
        }

        public override INode Visit(AST.Block astBlock, INode parent)
        {
            Debug.Assert(astBlock != null);

            var block = new Block
            {
                Statements = new List<IStatement>(),
                ParentNode = parent
            };

            foreach (var astStatement in astBlock.Statements)
            {
                var statement = astStatement.Accept(this, block) as IStatement;
                statement.ParentNode = block;
                block.Statements.Add(statement);
            }

            return block;
        }

        public override INode Visit(AST.FunctionCall astFunctionCall, INode parent)
        {
            Debug.Assert(astFunctionCall != null);

            var functionCall = new FunctionCall();
            functionCall.ParentNode = parent;
            var functionExpressionNode = astFunctionCall.FunctionExpression.Accept(this, functionCall);

            if (functionExpressionNode is Function)
            {
                var function = functionExpressionNode as Function;
                functionCall.Function = function;
                functionCall.Type = function.Type;
            }
            else if (functionExpressionNode is IExpression)
            {
                var functionExpression = functionExpressionNode as IExpression;
                var functionType = functionExpression.Type as FunctionType;
                functionCall.Function = functionType.Function;
                functionCall.Type = functionType;
            }
            else
            {
                throw new Exception($"Unknown node type: {functionExpressionNode.GetType().Name}");
            }

            return functionCall;
        }

        public override INode Visit(AST.MemberAccessOperator astMemberAccessOperator, INode parent)
        {
            Debug.Assert(astMemberAccessOperator != null);

            var memberContainerNode = astMemberAccessOperator.MemberContainer.Accept(this, parent);
            Type type; // TODO: use instead of declaration
            IDeclaration declaration = null;

            if (memberContainerNode is IDeclaration)
            {
                type = (memberContainerNode as IDeclaration).Type;
                declaration = memberContainerNode as IDeclaration;
            }
            else if (memberContainerNode is IExpression)
            {
                type = (memberContainerNode as IExpression).Type;
            }
            else
            {
                throw new Exception($"Unknown node type: {memberContainerNode.GetType().Name}");
            }

            var memberDeclarationNode = astMemberAccessOperator.MemberIdentifier.Accept(this, declaration);
            var memberDeclaration = memberDeclarationNode as IDeclaration;
            if (memberDeclaration == null)
            {
                errors.Add($"Unexpected AST node type: {memberDeclarationNode.GetType().Name}");
            }

            return memberDeclaration;
        }

        public override INode Visit(AST.Identifier identifier, INode parent)
        {
            Debug.Assert(identifier != null);

            var scope = parent.FindThisOrAncestorOfType<IScope>();
            return scope.FindDeclaration(identifier.Text);
        }

        public override INode Visit(AST.IntegerLiteral astIntegerLiteral, INode parent)
        {
            var integerLiteral = new IntegerLiteral();
            integerLiteral.Value = astIntegerLiteral.Value;
            return integerLiteral;
        }

        public override INode Visit(AST.StringLiteral astStringLiteral, INode parent)
        {
            var stringLiteral = new StringLiteral();
            stringLiteral.Value = astStringLiteral.Value;
            return stringLiteral;
        }

        #endregion

        private Program program;
        private List<string> errors;
    }

    public class AsgBuilder
    {
        public (Program, List<string>) BuildAsg(AST.Program astProgram)
        {
            // preconditions
            Debug.Assert(astProgram != null);

            // body
            errors = new List<string>();

            var initialPass = new AsgBuilderDeclarationsPass(errors);

            var node = astProgram.Accept(initialPass, null);
            var program = node as Program;
            if (program == null)
            {
                errors.Add($"Unexpected AST node type: {node.GetType().Name}");
            }

            var createTypesPass = new AsgBuilderCreateDeclarationTypesPass(program, errors);
            astProgram.Accept(createTypesPass, null);

            var functionBodiesPass = new AsgBuilderFunctionBodiesPass(program, errors);
            astProgram.Accept(functionBodiesPass, null);

            return (program, errors);
        }

        private List<string> errors;
    }
}
