using Presto.ParseTree;

namespace Presto;

public class Parser
{
    public Parser(List<Token> tokens)
    {
        this.tokens = tokens;
        nextTokenIndex = 0;
    }

    public Program ParseProgram()
    {
        Program program = new(new List<IExpression>());

        while (!IsDoneReading)
        {
            program.Expressions.Add(ParseExpression());
            ReadExpectedToken(TokenType.Semicolon);
        }

        return program;
    }

    /// <summary>
    /// Pratt parser based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    /// </summary>
    /// <param name="minBindingPower"></param>
    /// <returns>An expression.</returns>
    public IExpression ParseExpression(int minBindingPower = 0)
    {
        IExpression prefixExpression = ParsePrefixExpression();

        while (true)
        {
            Token? operatorToken = TryPeekToken();
            if (operatorToken == null) { break; }

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
                        prefixExpression = ParseCallExpression(prefixExpression);
                        break;
                    default:
                        throw new Exception();
                }

                continue;
            }

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
                        SkipToken();
                        IExpression rightExpr = ParseExpression(rightBindingPower);
                        prefixExpression = new MemberAccessOperator(prefixExpression, rightExpr);
                        break;
                    default:
                        throw new Exception();
                }

                continue;
            }

            break;
        }

        return prefixExpression;
    }

    public IExpression ParsePrefixExpression()
    {
        Token nextToken = ReadToken();

        switch (nextToken.Type)
        {
            case TokenType.Identifier:
                return new Identifier(nextToken.Text);
            case TokenType.StringLiteral:
                return new StringLiteral(nextToken.Text);
            default:
                throw new NotImplementedException($"Unexpected token type: {nextToken.Type}");
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

    private CallExpression ParseCallExpression(IExpression functionExpression)
    {
        CallExpression callExpression = new(functionExpression, new List<IExpression>());

        ReadExpectedToken(TokenType.LeftParen);

        bool isFirstArgument = true;

        while (!IsDoneReading && (PeekToken().Type != TokenType.RightParen))
        {
            callExpression.Arguments.Add(ParseExpression());

            if (isFirstArgument)
            {
                isFirstArgument = false;
            }
            else
            {
                ReadExpectedToken(TokenType.Semicolon);
            }
        }

        ReadExpectedToken(TokenType.RightParen);

        return callExpression;
    }                                          

    private List<Token> tokens;
    private int nextTokenIndex = 0;

    private Token? TryPeekToken()
    {
        if (nextTokenIndex < tokens.Count)
        {
            return tokens[nextTokenIndex];
        }
        else
        {
            return null;
        }
    }

    private Token PeekToken()
    {
        if (nextTokenIndex < tokens.Count)
        {
            return tokens[nextTokenIndex];
        }
        else
        {
            throw new Exception();
        }
    }

    private Token? TryReadToken()
    {
        if (nextTokenIndex < tokens.Count)
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

    private Token ReadToken()
    {
        if (nextTokenIndex < tokens.Count)
        {
            Token token = tokens[nextTokenIndex];
            nextTokenIndex++;
            return token;
        }
        else
        {
            throw new Exception();
        }
    }

    private Token ReadExpectedToken(TokenType expectedTokenType)
    {
        if (nextTokenIndex < tokens.Count)
        {
            Token token = tokens[nextTokenIndex];
            if (token.Type != expectedTokenType)
            {
                // TODO: better error handling
                throw new Exception();
            }

            nextTokenIndex++;
            return token;
        }
        else
        {
            // TODO: better error handling
            throw new Exception();
        }
    }

    private void SkipToken()
    {
        if (nextTokenIndex < tokens.Count)
        {
            nextTokenIndex++;
        }
    }

    private bool IsDoneReading => nextTokenIndex >= tokens.Count;
}