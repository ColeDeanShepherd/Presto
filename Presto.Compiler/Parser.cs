using Presto.ParseTree;

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
    public string GetDescription()
    {
        return (expectedTokenType == null)
            ? $"Unexpected token: '{encounteredTokenType}'."
            : $"Expected token '{expectedTokenType}' but encountered '{encounteredTokenType}'.";
    }
};

public record EndOfTokensError(
    TextRange TextRange
) : IParserError
{
    public string GetDescription()
    {
        return $"Unexpectedly reached the end of the tokens.";
    }
};

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
        Program program = new(new List<IExpression>());

        while (!IsDoneReading)
        {
            IExpression? expression = ParseExpression();
            
            if (expression != null)
            {
                program.Expressions.Add(expression);
            }

            ReadExpectedToken(TokenType.Semicolon);
        }

        return (program, errors);
    }

    /// <summary>
    /// Pratt parser based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    /// </summary>
    /// <param name="minBindingPower"></param>
    /// <returns>An expression.</returns>
    public IExpression? ParseExpression(int minBindingPower = 0)
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
            case TokenType.Identifier:
                return new Identifier(nextToken.Text);
            case TokenType.StringLiteral:
                return new StringLiteral(nextToken.Text);
            default:
                errors.Add(new UnexpectedTokenError(TextRange, nextToken.Type));
                return null;
        }
    }

    private int? GetPostfixLeftBindingPower(TokenType operatorTokenType)
    {
        switch (operatorTokenType)
        {
            case TokenType.LeftParen:
                return 12;
            default:
                return null;
        }
    }

    private (int, int)? GetInfixBindingPowers(TokenType operatorTokenType)
    {
        switch (operatorTokenType)
        {
            case TokenType.Period:
                return (14, 13);
            default:
                return null;
        }
    }

    private CallExpression? ParseCallExpression(IExpression functionExpression)
    {
        CallExpression callExpression = new(functionExpression, new List<IExpression>());

        if (ReadExpectedToken(TokenType.LeftParen) == null)
        {
            return null;
        }

        bool isFirstArgument = true;

        while (!IsDoneReading && (PeekToken()!.Type != TokenType.RightParen))
        {
            IExpression? expression = ParseExpression();
            if (expression == null)
            {
                return null;
            }

            callExpression.Arguments.Add(expression);

            if (isFirstArgument)
            {
                isFirstArgument = false;
            }
            else
            {
                ReadExpectedToken(TokenType.Comma);
            }
        }

        if (ReadExpectedToken(TokenType.RightParen) == null)
        {
            return null;
        }

        return callExpression;
    }                                          

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

    #endregion Helpers
}