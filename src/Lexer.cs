namespace Presto;

public class Lexer
{
    public Lexer(string sourceCode)
    {
        this.sourceCode = sourceCode;
        nextCharIndex = 0;
    }

    public List<Token> Tokenize()
    {
        List<Token> tokens = new List<Token>();

        while (!IsDoneReading)
        {
            char nextChar = PeekChar();

            if (IsIdentifierChar(nextChar))
            {
                tokens.Add(ReadIdentifier());
            }
            else if (nextChar == '"')
            {
                tokens.Add(ReadStringLiteral());
            }
            else if (nextChar == '.')
            {
                tokens.Add(new Token(TokenType.Period, ReadChar().ToString()));
            }
            else if (nextChar == '(')
            {
                tokens.Add(new Token(TokenType.LeftParen, ReadChar().ToString()));
            }
            else if (nextChar == ')')
            {
                tokens.Add(new Token(TokenType.RightParen, ReadChar().ToString()));
            }
            else if (nextChar == ';')
            {
                tokens.Add(new Token(TokenType.Semicolon, ReadChar().ToString()));
            }
            else
            {
                throw new Exception();
            }
        }

        return tokens;
    }

    private string sourceCode;
    private int nextCharIndex = 0;

    private char? TryPeekChar()
    {
        if (nextCharIndex < sourceCode.Length)
        {
            return sourceCode[nextCharIndex];
        }
        else
        {
            return null;
        }
    }

    private char PeekChar()
    {
        if (nextCharIndex < sourceCode.Length)
        {
            return sourceCode[nextCharIndex];
        }
        else
        {
            throw new Exception();
        }
    }

    private char? TryReadChar()
    {
        if (nextCharIndex < sourceCode.Length)
        {
            char c = sourceCode[nextCharIndex];
            nextCharIndex++;
            return c;
        }
        else
        {
            return null;
        }
    }

    private char ReadChar()
    {
        if (nextCharIndex < sourceCode.Length)
        {
            char c = sourceCode[nextCharIndex];
            nextCharIndex++;
            return c;
        }
        else
        {
            throw new Exception();
        }
    }

    private char ReadExpectedChar(char expectedChar)
    {
        if (nextCharIndex < sourceCode.Length)
        {
            char c = sourceCode[nextCharIndex];
            if (c != expectedChar)
            {
                // TODO: better error handling
                throw new Exception();
            }

            nextCharIndex++;
            return c;
        }
        else
        {
            // TODO: better error handling
            throw new Exception();
        }
    }

    private void SkipChar()
    {
        if (nextCharIndex < sourceCode.Length)
        {
            nextCharIndex++;
        }
    }

    private bool IsIdentifierChar(char c)
    {
        return char.IsLetterOrDigit(c);
    }

    private Token ReadIdentifier()
    {
        string tokenText = "";

        while (!IsDoneReading && (IsIdentifierChar(PeekChar())))
        {
            tokenText += ReadChar();
        }

        return new Token(TokenType.Identifier, tokenText);
    }

    private Token ReadStringLiteral()
    {
        ReadChar();

        string tokenText = "";

        while (PeekChar() != '"')
        {
            tokenText += ReadChar();
        }

        ReadChar();

        return new Token(TokenType.StringLiteral, tokenText);
    }

    private bool IsDoneReading => nextCharIndex >= sourceCode.Length;
}