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

        return (ParseNode(rootRule), errors);
    }

    private ParseTreeNode? ParseNode(IGrammarNode node)
    {
        if (node is GrammarRule rule)
        {
            List<ParseTreeNode> children = new();

            foreach (var n in rule.Nodes)
            {
                var child = ParseNode(n);

                if (child == null)
                {
                    return null;
                }

                children.Add(child);
            }

            return new ParseTreeNode(node, children);
        }
        else if (node is TokenGrammarNode tokenNode)
        {
            var token = ReadExpectedToken(tokenNode.TokenType);
            return (token != null)
                ? new ParseTreeNode(node, new List<ParseTreeNode>())
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
        else if (node is GroupGrammarNode group)
        {
            List<ParseTreeNode> children = new();

            foreach (var n in group.Nodes)
            {
                var child = ParseNode(n);

                if (child != null)
                {
                    children.Add(child);
                }
                else
                {
                    return null;
                }
            }

            return new ParseTreeNode(group, children);
        }
        else if (node is ExpressionGrammarNode expr)
        {
            return ParseExpression();
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
            var child = ParseNode(n);

            if (child == null)
            {
                return null;
            }

            children.Add(child);
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
        return ParseNode(expressionGrammarNode.PrefixExpressionNode);
    }

    private ParseTreeNode? ParseXOrMoreNode(IGrammarNode node, uint x)
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
                var child = ParseNode(node);

                if (child != null)
                {
                    children.Add(child);
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
            ? new ParseTreeNode(node, children)
            : null; // TODO: error
    }

    private ParseTreeNode? ParseTokenSeparatedNode(TokenSeparatedGrammarNode tokenSeparated)
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
                var child = ParseNode(tokenSeparated.Node);

                if (child != null)
                {
                    children.Add(child);
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
            ? new ParseTreeNode(tokenSeparated, children)
            : null; // TODO: errors?
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

public class Parser
{
    public Parser(List<Token> tokens)
    {
        this.tokens = tokens;
        nextTokenIndex = 0;
        errors = new List<IParserError>();
    }

    public (Program, List<IParserError>) ParseProgram()
    {
        Program program = new(new List<IStatement>());

        while (true)
        {
            SkipWhitespaceAndComments();

            if (IsDoneReading)
            {
                break;
            }

            IStatement? statement = ParseStatement();
            
            if (statement != null)
            {
                program.Statements.Add(statement);
            }
        }

        return (program, errors);
    }

    public IStatement? ParseStatement() =>
        from nextToken in PeekToken()
        select (IStatement?)(nextToken.Type switch
        {
            TokenType.LetKeyword => ParseLetStatement(),
            TokenType.StructKeyword => ParseStructDeclaration(),
            TokenType.FunctionKeyword => ParseFunctionDefinition(),
            _ => ParseExpression()
        });

    public LetStatement? ParseLetStatement() =>
        from _ in ReadExpectedToken(TokenType.LetKeyword)
        from variableName in ParseIdentifier()
        from _2 in ReadExpectedToken(TokenType.Colon)
        from typeName in ParseQualifiedName()
        from _3 in ReadExpectedToken(TokenType.Equals)
        from value in ParseExpression()
        select new LetStatement(variableName, typeName, value);

    public StructDefinition? ParseStructDeclaration() =>
        from _ in ReadExpectedToken(TokenType.StructKeyword)
        from structName in ParseIdentifier()
        from _2 in ReadExpectedToken(TokenType.LeftCurlyBracket)
        from fieldDeclarations in ParseTokenSeparatedList(ParseFieldDeclaration, TokenType.Comma, TokenType.RightCurlyBracket)
        from _3 in ReadExpectedToken(TokenType.RightCurlyBracket)
        select new StructDefinition(structName, fieldDeclarations);

    public FunctionDefinition? ParseFunctionDefinition() =>
        from _ in ReadExpectedToken(TokenType.FunctionKeyword)
        from name in ParseIdentifier()
        from _2 in ReadExpectedToken(TokenType.LeftParen)
        from parameterDefinitions in ParseTokenSeparatedList(ParseParameterDefinition, TokenType.Comma, TokenType.RightParen)
        from _3 in ReadExpectedToken(TokenType.RightParen)
        from _4 in ReadExpectedToken(TokenType.LeftCurlyBracket)
        from bodyStatements in ParseTokenSeparatedList(ParseStatement, TokenType.Semicolon, TokenType.RightCurlyBracket)
        from _5 in ReadExpectedToken(TokenType.RightCurlyBracket)
        select new FunctionDefinition(name, parameterDefinitions, bodyStatements);

    public ParameterDefinition? ParseParameterDefinition() =>
        from fieldName in ParseIdentifier()
        from _ in ReadExpectedToken(TokenType.Colon)
        from typeName in ParseQualifiedName()
        select new ParameterDefinition(fieldName, typeName);

    public FieldDeclaration? ParseFieldDeclaration() =>
        from fieldName in ParseIdentifier()
        from _ in ReadExpectedToken(TokenType.Colon)
        from typeName in ParseQualifiedName()
        select new FieldDeclaration(fieldName, typeName);

    public IExpression? ParseExpression() => ParseExpression(0);

    /// <summary>
    /// Pratt parser based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    /// </summary>
    /// <param name="minBindingPower"></param>
    /// <returns>An expression.</returns>
    public IExpression? ParseExpression(int minBindingPower)
    {
        // Parse prefix expression.
        IExpression? prefixExpression = ParsePrefixExpression();
        if (prefixExpression == null)
        {
            return null;
        }

        while (true)
        {
            Token? operatorToken = TryPeekToken();
            if (operatorToken == null) { break; }

            // Parse postfix operators.
            int? postfixLeftBindingPower = GetPostfixLeftBindingPower(operatorToken.Type);

            if (postfixLeftBindingPower.HasValue)
            {
                if (postfixLeftBindingPower.Value < minBindingPower)
                {
                    break;
                }

                switch (operatorToken.Type)
                {
                    case TokenType.LeftParen:
                        prefixExpression = ParseCallExpression(prefixExpression!);
                        if (prefixExpression == null)
                        {
                            return null;
                        }
                        break;
                    default:
                        errors.Add(new UnexpectedTokenError(TextRange, operatorToken.Type));
                        return null;
                }

                continue;
            }

            // Parse infix operators.
            (int, int)? infixBindingPowers = GetInfixBindingPowers(operatorToken.Type);

            if (infixBindingPowers.HasValue)
            {
                int leftBindingPower = infixBindingPowers.Value.Item1;
                int rightBindingPower = infixBindingPowers.Value.Item2;

                if (leftBindingPower < minBindingPower)
                {
                    break;
                }
;
                switch (operatorToken.Type)
                {
                    case TokenType.Period:
                        ReadExpectedToken(TokenType.Period);

                        IExpression? rightExpr = ParseExpression(rightBindingPower);
                        if (rightExpr == null)
                        {
                            return null;
                        }

                        prefixExpression = new MemberAccessOperator(prefixExpression, rightExpr);
                        break;
                    default:
                        errors.Add(new UnexpectedTokenError(TextRange, operatorToken.Type));
                        return null;
                }

                continue;
            }

            break;
        }

        return prefixExpression;
    }

    public IExpression? ParsePrefixExpression()
    {
        Token? nextToken = ReadToken();
        if (nextToken == null)
        {
            return null;
        }

        switch (nextToken.Type)
        {
            case TokenType.Number:
                return new NumberLiteral(nextToken.Text);
            case TokenType.StringLiteral:
                return new StringLiteral(nextToken.Text);
            case TokenType.Identifier:
                return new Identifier(nextToken.Text);
            default:
                errors.Add(new UnexpectedTokenError(TextRange, nextToken.Type));
                return null;
        }
    }

    private int? GetPostfixLeftBindingPower(TokenType operatorTokenType) =>
        operatorTokenType switch
        {
            TokenType.LeftParen => 12,
            _ => null
        };

    private (int, int)? GetInfixBindingPowers(TokenType operatorTokenType) =>
        operatorTokenType switch
        {
            TokenType.Period => (14, 13),
            _ => null
        };

    private CallExpression? ParseCallExpression(IExpression functionExpression) =>
        from _ in ReadExpectedToken(TokenType.LeftParen)
        from arguments in ParseTokenSeparatedList(ParseExpression, TokenType.Comma, TokenType.RightParen)
        from _2 in ReadExpectedToken(TokenType.RightParen)
        select new CallExpression(functionExpression, arguments);

    private List<TNode>? ParseTokenSeparatedList<TNode>(Func<TNode?> parseNode, TokenType separatorTokenType, TokenType? closingTokenType = null)
    {
        List<TNode> nodes = new();

        while (true)
        {
            Token? nextToken;

            if (closingTokenType != null)
            {
                nextToken = PeekToken();
                if (nextToken == null)
                {
                    return null;
                }
                else if (nextToken.Type == closingTokenType)
                {
                    break;
                }
            }

            TNode? node = parseNode();
            if (node == null)
            {
                return null;
            }

            nodes.Add(node);

            nextToken = PeekToken();
            if ((nextToken == null) || (nextToken.Type != separatorTokenType))
            {
                break;
            }
            else
            {
                if (ReadExpectedToken(separatorTokenType) == null)
                {
                    return null;
                }
            }
        }

        return nodes;
    }

    private QualifiedName? ParseQualifiedName() =>
        from identifiers in ParseTokenSeparatedList(ParseIdentifier, TokenType.Period)
        select new QualifiedName(identifiers);

    private Identifier? ParseIdentifier() =>
        from token in ReadExpectedToken(TokenType.Identifier)
        select new Identifier(token.Text);

    private List<Token> tokens;
    private int nextTokenIndex = 0;
    private List<IParserError> errors;

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