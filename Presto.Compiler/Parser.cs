using Presto.Compiler;
using Presto.ParseTree;
using System.Xml.Linq;

namespace Presto;

public interface IParserError
{
    public TextRange TextRange { get; }

    public string GetDescription();
}

public record UnexpectedTokenError(
    TextRange TextRange,
    TokenType encounteredTokenType,
    TokenType? expectedTokenType = null
) : IParserError
{
    public string GetDescription() =>
        (expectedTokenType == null)
            ? $"Unexpected token: {encounteredTokenType}."
            : $"Expected token {expectedTokenType} but encountered {encounteredTokenType}.";
};

public record EndOfTokensError(
    TextRange TextRange
) : IParserError
{
    public string GetDescription() => "Unexpectedly reached the end of the tokens.";
};

public class GrammarParser
{
    public GrammarParser(List<GrammarRule> grammar, List<Token> tokens)
    {
        this.grammar = grammar;
        expressionGrammarNode =
            grammar
                .Where(r => (r.Nodes.Count == 1) && (r.Nodes.First() is ExpressionGrammarNode))
                .Select(r => r.Nodes.First())
                .Cast<ExpressionGrammarNode>()
                .First();
        this.tokens = tokens;
        nextTokenIndex = 0;
        minExprBindingPowerStack = new Stack<int>();
        errors = new List<IParserError>();
    }

    public (ParseTreeNode?, List<IParserError>) Parse()
    {
        var rootRule = grammar.First();

        return (ParseNode(rootRule)?.Single(), errors);
    }

    private List<ParseTreeNode>? ParseNode(IGrammarNode node)
    {
        if (node is GrammarRule rule)
        {
            List<ParseTreeNode> children = new();

            foreach (var n in rule.Nodes)
            {
                var newChildren = ParseNode(n);

                if (newChildren == null)
                {
                    return null;
                }

                children.AddRange(newChildren);
            }

            return new List<ParseTreeNode> { new ParseTreeNode(node, children) };
        }
        else if (node is TokenGrammarNode tokenNode)
        {
            var token = ReadExpectedToken(tokenNode.TokenType);
            return (token != null)
                ? new List<ParseTreeNode> { new ParseTreeNode(node, new List<ParseTreeNode>()) }
                : null;
        }
        else if (node is OneOfGrammarNode oneOf)
        {
            foreach (var childNode in oneOf.Nodes)
            {
                var firstTokenTypes = GrammarHelpers.GetFirstTokenTypes(grammar, childNode);

                var token = PeekToken();
                if (token == null)
                {
                    return null;
                }

                if (firstTokenTypes.Contains(token.Type))
                {
                    return ParseNode(childNode);
                }
            }

            return null;
        }
        else if (node is OptionalGrammarNode optional)
        {
            if (IsDoneReading)
            {
                return null;
            }

            var firstTokenTypes = GrammarHelpers.GetFirstTokenTypes(grammar, optional.Node);

            var token = PeekToken();
            if (token == null)
            {
                return null;
            }

            return firstTokenTypes.Contains(token.Type)
                ? ParseNode(optional.Node)
                : null;
        }
        else if (node is ZeroOrMoreGrammarNode zeroOrMore)
        {
            return ParseXOrMoreNode(zeroOrMore.Node, 0);
        }
        else if (node is OneOrMoreGrammarNode oneOrMore)
        {
            return ParseXOrMoreNode(oneOrMore.Node, 1);
        }
        else if (node is TokenSeparatedGrammarNode tokenSeparated)
        {
            return ParseTokenSeparatedNode(tokenSeparated);
        }
        else if (node is TokenTerminatedGrammarNode tokenTerminated)
        {
            return ParseTokenTerminatedNode(tokenTerminated);
        }
        else if (node is GroupGrammarNode group)
        {
            List<ParseTreeNode> children = new();

            foreach (var n in group.Nodes)
            {
                var newChildren = ParseNode(n);

                if (newChildren != null)
                {
                    children.AddRange(newChildren);
                }
                else
                {
                    return null;
                }
            }

            return children;
        }
        else if (node is ExpressionGrammarNode expr)
        {
            var expression = ParseExpression();

            return (expression != null)
                ? new List<ParseTreeNode> { expression }
                : null;
        }
        else if (node is GrammarRuleReference ruleRef)
        {
            var resolvedRule = grammar.First(r => r.Name == ruleRef.Name);
            return ParseNode(resolvedRule);
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    private ParseTreeNode? ParseRuleGivenFirstChild(GrammarRule rule, ParseTreeNode firstChild)
    {
        List<ParseTreeNode> children = new()
        {
            firstChild
        };

        foreach (var n in rule.Nodes.Skip(1))
        {
            var newChildren = ParseNode(n);

            if (newChildren == null)
            {
                return null;
            }

            children.AddRange(newChildren);
        }

        return new ParseTreeNode(rule, children);
    }

    private GrammarRule? ResolveRule(IGrammarNode node)
    {
        if (node is GrammarRule rule)
        {
            return rule;
        }
        else if (node is GrammarRuleReference ruleRef)
        {
            return grammar.First(r => r.Name == ruleRef.Name);
        }
        else
        {
            return null;
        }
    }

    /// <summary>
    /// Pratt parser based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    /// </summary>
    /// <returns>An expression.</returns>
    public ParseTreeNode? ParseExpression()
    {
        var minBindingPower = minExprBindingPowerStack.Any()
            ? minExprBindingPowerStack.Peek()
            : 0;

        // Parse prefix expression.
        ParseTreeNode? prefixExpression = ParsePrefixExpression();
        if (prefixExpression == null)
        {
            return null;
        }

        while (true)
        {
            Token? operatorToken = TryPeekToken();
            if (operatorToken == null) { break; }

            // Parse postfix operators.
            (int leftBindingPower, IGrammarNode ruleNode)? postfixLeftBindingPower = GetPostfixLeftBindingPower(operatorToken.Type);

            if (postfixLeftBindingPower.HasValue)
            {
                int leftBindingPower = postfixLeftBindingPower.Value.leftBindingPower;
                GrammarRule? rule = ResolveRule(postfixLeftBindingPower.Value.ruleNode);

                if (rule == null)
                {
                    return null; // TODO: error
                }

                if (leftBindingPower < minBindingPower)
                {
                    break;
                }

                prefixExpression = ParseRuleGivenFirstChild(rule, prefixExpression!);
                if (prefixExpression == null)
                {
                    return null;
                }

                continue;
            }

            // Parse infix operators.
            (int leftBindingPower, int rightBindingPower, IGrammarNode ruleNode)? infixBindingPowers = GetInfixBindingPowers(operatorToken.Type);

            if (infixBindingPowers.HasValue)
            {
                int leftBindingPower = infixBindingPowers.Value.leftBindingPower;
                int rightBindingPower = infixBindingPowers.Value.rightBindingPower;
                GrammarRule? rule = ResolveRule(infixBindingPowers.Value.ruleNode);

                if (rule == null)
                {
                    return null; // TODO: error
                }

                if (leftBindingPower < minBindingPower)
                {
                    break;
                }

                // TODO: pass binding power
                minExprBindingPowerStack.Push(rightBindingPower);
                prefixExpression = ParseRuleGivenFirstChild(rule, prefixExpression!);
                minExprBindingPowerStack.Pop();

                if (prefixExpression == null)
                {
                    return null;
                }

                continue;
            }

            break;
        }

        return prefixExpression;
    }

    public ParseTreeNode? ParsePrefixExpression()
    {
        return ParseNode(expressionGrammarNode.PrefixExpressionNode)?.Single();
    }

    private List<ParseTreeNode>? ParseXOrMoreNode(IGrammarNode node, uint x)
    {
        uint numParsed = 0;

        var firstTokenTypes = GrammarHelpers.GetFirstTokenTypes(grammar, node);
        List<ParseTreeNode> children = new();

        while (true)
        {
            if (IsDoneReading)
            {
                break;
            }

            Token? nextToken = PeekToken();
            if (nextToken == null)
            {
                return null;
            }

            if (firstTokenTypes.Contains(nextToken.Type))
            {
                var newChildren = ParseNode(node);

                if (newChildren != null)
                {
                    children.AddRange(newChildren);
                    numParsed++;
                }
                else
                {
                    return null;
                }
            }
            else
            {
                break;
            }
        }

        return (numParsed >= x)
            ? children
            : null; // TODO: error
    }

    private List<ParseTreeNode>? ParseTokenSeparatedNode(TokenSeparatedGrammarNode tokenSeparated)
    {
        uint numParsed = 0;

        var firstTokenTypes = GrammarHelpers.GetFirstTokenTypes(grammar, tokenSeparated.Node);
        List<ParseTreeNode> children = new();

        while (true)
        {
            if (IsDoneReading)
            {
                break;
            }

            Token? nextToken;

            if (numParsed > 0)
            {
                nextToken = PeekToken();
                if (nextToken == null)
                {
                    return null;
                }

                if (nextToken.Type != tokenSeparated.TokenType)
                {
                    break;
                }

                if (ReadExpectedToken(tokenSeparated.TokenType) == null)
                {
                    return null;
                }
            }

            nextToken = PeekToken();
            if (nextToken == null)
            {
                return null;
            }

            if (firstTokenTypes.Contains(nextToken.Type))
            {
                var newChildren = ParseNode(tokenSeparated.Node);

                if (newChildren != null)
                {
                    children.AddRange(newChildren);
                    numParsed++;
                }
                else
                {
                    return null;
                }
            }
            else
            {
                break;
            }
        }

        return (!tokenSeparated.OneOrMore || (numParsed >= 1))
            ? children
            : null; // TODO: errors?
    }

    private List<ParseTreeNode>? ParseTokenTerminatedNode(TokenTerminatedGrammarNode tokenTerminated)
    {
        var firstTokenTypes = GrammarHelpers.GetFirstTokenTypes(grammar, tokenTerminated.Node);
        List<ParseTreeNode> children = new();

        while (true)
        {
            if (IsDoneReading)
            {
                break;
            }

            Token? nextToken;

            nextToken = PeekToken();
            if (nextToken == null)
            {
                return null;
            }

            if (firstTokenTypes.Contains(nextToken.Type))
            {
                var newChildren = ParseNode(tokenTerminated.Node);

                if (newChildren != null)
                {
                    if (ReadExpectedToken(tokenTerminated.TokenType) == null)
                    {
                        return null;
                    }

                    children.AddRange(newChildren);
                }
                else
                {
                    return null;
                }
            }
            else
            {
                break;
            }
        }

        return children;
    }

    private (int, IGrammarNode)? GetPostfixLeftBindingPower(TokenType operatorTokenType) =>
        expressionGrammarNode.PostfixOperatorLeftBindingPowers.ContainsKey(operatorTokenType)
            ? expressionGrammarNode.PostfixOperatorLeftBindingPowers[operatorTokenType]
        : null;

    private (int, int, IGrammarNode)? GetInfixBindingPowers(TokenType operatorTokenType) =>
        expressionGrammarNode.InfixOperatorBindingPowers.ContainsKey(operatorTokenType)
            ? expressionGrammarNode.InfixOperatorBindingPowers[operatorTokenType]
        : null;

    private List<GrammarRule> grammar;
    private List<Token> tokens;
    private int nextTokenIndex = 0;
    private List<IParserError> errors;
    private ExpressionGrammarNode expressionGrammarNode;
    private Stack<int> minExprBindingPowerStack;

    #region Helpers

    private bool IsStillReading => nextTokenIndex < tokens.Count;
    private bool IsDoneReading => nextTokenIndex >= tokens.Count;
    private TextRange TextRange
    {
        get
        {
            if (nextTokenIndex < tokens.Count)
            {
                return tokens[nextTokenIndex].TextRange;
            }
            else if (tokens.Any())
            {
                return tokens[nextTokenIndex - 1].TextRange;
            }
            else
            {
                return new TextRange(new TextPosition(0, 0), new TextPosition(0, 0));
            }
        }
    }

    private Token? TryPeekToken()
    {
        SkipWhitespaceAndComments();

        return IsStillReading
            ? tokens[nextTokenIndex]
            : null;
    }

    private Token? PeekToken()
    {
        Token? nextToken = TryPeekToken();

        if (nextToken == null)
        {
            errors.Add(new EndOfTokensError(TextRange));
            return null;
        }

        return nextToken;
    }

    private Token? TryReadToken()
    {
        SkipWhitespaceAndComments();

        if (IsStillReading)
        {
            Token token = tokens[nextTokenIndex];
            nextTokenIndex++;
            return token;
        }
        else
        {
            return null;
        }
    }

    private Token? ReadToken()
    {
        Token? nextToken = TryReadToken();

        if (nextToken == null)
        {
            errors.Add(new EndOfTokensError(TextRange));
            return null;
        }

        return nextToken;
    }

    private Token? ReadExpectedToken(TokenType expectedTokenType)
    {
        Token? nextToken = ReadToken();

        if ((nextToken != null) && (nextToken.Type != expectedTokenType))
        {
            errors.Add(new UnexpectedTokenError(TextRange, nextToken.Type, expectedTokenType));
            return null;
        }

        return nextToken;
    }

    private void SkipWhitespaceAndComments()
    {
        while (!IsDoneReading && ((tokens[nextTokenIndex].Type == TokenType.Whitespace) || (tokens[nextTokenIndex].Type == TokenType.SingleLineComment)))
        {
            nextTokenIndex++;
        }
    }

    #endregion Helpers
}