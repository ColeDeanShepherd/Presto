using System.Text;

namespace Presto.Compiler;

public interface IGrammarNode { }

public record GrammarRule(
    string Name,
    List<IGrammarNode> Nodes
) : IGrammarNode
{ }

public record TokenGrammarNode(
    TokenType TokenType
) : IGrammarNode;

public record OneOfGrammarNode(
    List<IGrammarNode> Nodes
) : IGrammarNode;

public record OptionalGrammarNode(
    IGrammarNode Node
) : IGrammarNode;

public record ZeroOrMoreGrammarNode(
    IGrammarNode Node
) : IGrammarNode;

public record OneOrMoreGrammarNode(
    IGrammarNode Node
) : IGrammarNode;

public record TokenSeparatedGrammarNode(
    IGrammarNode Node,
    TokenType TokenType,
    bool OneOrMore
) : IGrammarNode;

public record GroupGrammarNode(
    List<IGrammarNode> Nodes
) : IGrammarNode;

public record ExpressionGrammarNode(
    IGrammarNode PrefixExpressionNode,
    Dictionary<TokenType, (int, IGrammarNode)> PostfixOperatorLeftBindingPowers,
    Dictionary<TokenType, (int, int, IGrammarNode)> InfixOperatorBindingPowers
) : IGrammarNode;

public record GrammarRuleReference(
    string Name
) : IGrammarNode;

public static class GrammarBuilder
{
    public static GrammarRule Rule(string Name, params IGrammarNode[] Nodes) => new GrammarRule(Name, Nodes.ToList());
    public static TokenGrammarNode Token(TokenType TokenType) => new TokenGrammarNode(TokenType);
    public static OneOfGrammarNode OneOf(params IGrammarNode[] Nodes) => new OneOfGrammarNode(Nodes.ToList());
    public static OptionalGrammarNode OneOf(IGrammarNode Node) => new OptionalGrammarNode(Node);
    public static ZeroOrMoreGrammarNode ZeroOrMore(this IGrammarNode Node) => new ZeroOrMoreGrammarNode(Node);
    public static OneOrMoreGrammarNode OneOrMore(this IGrammarNode Node) => new OneOrMoreGrammarNode(Node);
    public static TokenSeparatedGrammarNode TokenSeparated(IGrammarNode Node, TokenType TokenType, bool OneOrMore = false) => new TokenSeparatedGrammarNode(Node, TokenType, OneOrMore);
    public static GroupGrammarNode Group(params IGrammarNode[] Nodes) => new GroupGrammarNode(Nodes.ToList());
    public static GrammarRuleReference RuleRef(string Name) => new GrammarRuleReference(Name);
}

public static class GrammarHelpers
{
    public static List<GrammarRule> ResolveReferences(List<GrammarRule> grammar)
    {
        var rulesByName = grammar.ToDictionary(r => r.Name);

        return grammar
            .Map(rule => ResolveReferences(rulesByName, rule))
            .Cast<GrammarRule>()
            .ToList();
    }

    public static IGrammarNode ResolveReferences(Dictionary<string, GrammarRule> rulesByName, IGrammarNode node)
    {
        if (node is GrammarRule rule)
        {
            return new GrammarRule(
                rule.Name,
                rule.Nodes.Map(n => ResolveReferences(rulesByName, n)).ToList());
        }
        else if (node is TokenGrammarNode token)
        {
            return node;
        }
        else if (node is OneOfGrammarNode oneOf)
        {
            return new OneOfGrammarNode(oneOf.Nodes.Map(n => ResolveReferences(rulesByName, n)).ToList());
        }
        else if (node is OptionalGrammarNode optional)
        {
            return new OptionalGrammarNode(ResolveReferences(rulesByName, optional.Node));
        }
        else if (node is ZeroOrMoreGrammarNode zeroOrMore)
        {
            return new ZeroOrMoreGrammarNode(ResolveReferences(rulesByName, zeroOrMore.Node));
        }
        else if (node is OneOrMoreGrammarNode oneOrMore)
        {
            return new OneOrMoreGrammarNode(ResolveReferences(rulesByName, oneOrMore.Node));
        }
        else if (node is TokenSeparatedGrammarNode tokenSeparated)
        {
            return new TokenSeparatedGrammarNode(ResolveReferences(rulesByName, tokenSeparated.Node), tokenSeparated.TokenType, tokenSeparated.OneOrMore);
        }
        else if (node is GroupGrammarNode group)
        {
            return new GroupGrammarNode(group.Nodes.Map(n => ResolveReferences(rulesByName, n)).ToList());
        }
        else if (node is GrammarRuleReference reference)
        {
            return ResolveReferences(rulesByName, rulesByName[reference.Name]);
        }
        else if (node is ExpressionGrammarNode expr)
        {
            return new ExpressionGrammarNode(
                ResolveReferences(rulesByName, expr.PrefixExpressionNode),
                expr.PostfixOperatorLeftBindingPowers
                    .ToDictionary(
                        kvp => kvp.Key,
                        kvp => (kvp.Value.Item1, ResolveReferences(rulesByName, kvp.Value.Item2))),
                expr.InfixOperatorBindingPowers
                    .ToDictionary(
                        kvp => kvp.Key,
                        kvp => (kvp.Value.Item1, kvp.Value.Item2, ResolveReferences(rulesByName, kvp.Value.Item3))));
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public static string PrintGrammar(List<GrammarRule> grammar)
    {
        var stringBuilder = new StringBuilder();

        string Print(IGrammarNode node)
        {
            if (node is GrammarRule rule)
            {
                return rule.Name;
            }
            else if (node is TokenGrammarNode token)
            {
                return token.TokenType.ToString();
            }
            else if (node is OneOfGrammarNode oneOf)
            {
                return string.Join(" | ", oneOf.Nodes.Select(Print));
            }
            else if (node is OptionalGrammarNode optional)
            {
                return Print(optional.Node) + '?';
            }
            else if (node is ZeroOrMoreGrammarNode zeroOrMore)
            {
                return Print(zeroOrMore.Node) + '*';
            }
            else if (node is OneOrMoreGrammarNode oneOrMore)
            {
                return Print(oneOrMore.Node) + '+';
            }
            else if (node is TokenSeparatedGrammarNode tokenSeparated)
            {
                var result = $"({Print(tokenSeparated.Node)} {tokenSeparated.TokenType})* {Print(tokenSeparated.Node)}";
                return tokenSeparated.OneOrMore
                    ? result
                    : $"({result})?";
            }
            else if (node is GroupGrammarNode group)
            {
                return $"({string.Join(' ', group.Nodes.Select(Print))})";
            }
            else if (node is GrammarRuleReference reference)
            {
                return $"Ref({reference.Name})";
            }
            else if (node is ExpressionGrammarNode expr)
            {
                return "Expression";
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        foreach (var rule in grammar)
        {
            stringBuilder.AppendLine($"{rule.Name}: {string.Join(' ', rule.Nodes.Select(Print))}");
        }

        return stringBuilder.ToString();
    }

    public static HashSet<TokenType> GetFirstTokenTypes(List<GrammarRule> grammar, GrammarRule rule) =>
        GetFirstTokenTypes(grammar, rule.Nodes.First());

    public static HashSet<TokenType> GetFirstTokenTypes(List<GrammarRule> grammar, IGrammarNode node)
    {
        if (node is GrammarRule rule)
        {
            return GetFirstTokenTypes(grammar, rule.Nodes.First());
        }
        else if (node is TokenGrammarNode token)
        {
            return new HashSet<TokenType>(new[] { token.TokenType });
        }
        else if (node is OneOfGrammarNode oneOf)
        {
            return oneOf.Nodes
                .Select(n => GetFirstTokenTypes(grammar, n))
                .SelectMany(x => x)
                .ToHashSet();
        }
        else if (node is OptionalGrammarNode optional)
        {
            return GetFirstTokenTypes(grammar, optional.Node);
        }
        else if (node is ZeroOrMoreGrammarNode zeroOrMore)
        {
            return GetFirstTokenTypes(grammar, zeroOrMore.Node);
        }
        else if (node is OneOrMoreGrammarNode oneOrMore)
        {
            return GetFirstTokenTypes(grammar, oneOrMore.Node);
        }
        else if (node is TokenSeparatedGrammarNode tokenSeparated)
        {
            return GetFirstTokenTypes(grammar, tokenSeparated.Node);
        }
        else if (node is GroupGrammarNode group)
        {
            return GetFirstTokenTypes(grammar, group.Nodes.First());
        }
        else if (node is GrammarRuleReference reference)
        {
            var resolvedReference = grammar.First(r => r.Name == reference.Name);
            return GetFirstTokenTypes(grammar, resolvedReference);
        }
        else if (node is ExpressionGrammarNode expr)
        {
            return GetFirstTokenTypes(grammar, expr.PrefixExpressionNode);
        }
        else
        {
            throw new NotImplementedException();
        }
    }
}