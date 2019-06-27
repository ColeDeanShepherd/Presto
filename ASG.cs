using System;
using System.Collections.Generic;
using System.Linq;

namespace Presto.ASG
{
    public interface INode
    {
        INode ParentNode { get; }
        IEnumerable<INode> ChildNodes { get; }

        void Accept(AsgNodeVisitor visitor);
    }
    public interface IDeclaration : INode
    {
        string Name { get; }
    }
    public interface IStatement : INode { }
    public interface IExpression : INode { }
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
        public static Program CreateWithStdLib()
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
            writeLineFunction.SetParent(stdIoStdOutNamespace);

            return program;
        }

        #region INode

        public INode ParentNode => null;
        public IEnumerable<INode> ChildNodes => IEnumerableExtensions.AsEnumerable<INode>(GlobalNamespace);

        public void Accept(AsgNodeVisitor visitor)
        {
            visitor.Visit(this);
        }

        #endregion

        public Namespace GlobalNamespace;
    }

    public class Namespace : IDeclaration, IScope
    {
        #region INode

        public INode ParentNode => Parent;
        public IEnumerable<INode> ChildNodes =>
            IEnumerableExtensions.Concat(Namespaces.Cast<INode>(), Functions.Cast<INode>());

        public void Accept(AsgNodeVisitor visitor)
        {
            visitor.Visit(this);
        }

        #endregion

        #region IDeclaration

        public string Name { get; set; }

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

        public void Accept(AsgNodeVisitor visitor)
        {
            visitor.Visit(this);
        }

        #endregion

        #region IDeclaration

        public string Name { get; set; }

        #endregion

        public Namespace Parent;

        // TODO: parameters
        // TODO: return type
        public Block Body; // nullable

        public Function(string name)
        {
            Name = name;
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

    public class Block : INode, IScope
    {
        #region INode

        public INode ParentNode { get; set; }
        public IEnumerable<INode> ChildNodes => Statements.Cast<INode>();

        public void Accept(AsgNodeVisitor visitor)
        {
            visitor.Visit(this);
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

        public void Accept(AsgNodeVisitor visitor)
        {
            visitor.Visit(this);
        }

        #endregion

        public Function Function;
        public List<IExpression> Arguments;

        public FunctionCall()
        {
            Arguments = new List<IExpression>();
        }
    }

    public class StringLiteral : IExpression
    {
        #region INode

        public INode ParentNode { get; set; }
        public IEnumerable<INode> ChildNodes => Enumerable.Empty<INode>();

        public void Accept(AsgNodeVisitor visitor)
        {
            visitor.Visit(this);
        }

        #endregion

        public string Value;
    }
}
