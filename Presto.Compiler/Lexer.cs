namespace Presto;

public class Lexer
{
    public Lexer(string sourceCode)
    {
        this.sourceCode = sourceCode;
        nextCharIndex = 0;
        textPosition = new(0, 0);
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
                TextPosition textPosition = this.textPosition;
                tokens.Add(new Token(TokenType.Period, ReadChar().ToString(), textPosition));
            }
            else if (nextChar == '(')
            {
                TextPosition textPosition = this.textPosition;
                tokens.Add(new Token(TokenType.LeftParen, ReadChar().ToString(), textPosition));
            }
            else if (nextChar == ')')
            {
                TextPosition textPosition = this.textPosition;
                tokens.Add(new Token(TokenType.RightParen, ReadChar().ToString(), textPosition));
            }
            else if (nextChar == ';')
            {
                TextPosition textPosition = this.textPosition;
                tokens.Add(new Token(TokenType.Semicolon, ReadChar().ToString(), textPosition));
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
    private TextPosition textPosition;

    private char? TryPeekChar()
    {
        return IsStillReading
            ? sourceCode[nextCharIndex]
            : null;
    }

    private char PeekChar()
    {
        char? nextChar = TryPeekChar();

        if (nextChar == null)
        {
            throw new Exception();
        }

        return nextChar.Value;
    }

    private char? TryReadChar()
    {
        if (IsStillReading)
        {
            char nextChar = sourceCode[nextCharIndex];
            AdvanceTextPosition(nextChar);
            return nextChar;
        }
        else
        {
            return null;
        }
    }

    private char ReadChar()
    {
        char? nextChar = TryReadChar();

        if (nextChar == null)
        {
            throw new Exception();
        }

        return nextChar.Value;
    }

    private char ReadExpectedChar(char expectedChar)
    {
        char nextChar = ReadChar();

        if (nextChar != expectedChar)
        {
            throw new Exception();
        }

        return nextChar;
    }

    private bool IsIdentifierChar(char c)
    {
        return char.IsLetterOrDigit(c);
    }

    private Token ReadIdentifier()
    {
        TextPosition textPosition = this.textPosition;
        string tokenText = "";

        while (!IsDoneReading && (IsIdentifierChar(PeekChar())))
        {
            tokenText += ReadChar();
        }

        return new Token(TokenType.Identifier, tokenText, textPosition);
    }

    private Token ReadStringLiteral()
    {
        TextPosition textPosition = this.textPosition;

        ReadChar();

        string tokenText = "";

        while (PeekChar() != '"')
        {
            tokenText += ReadChar();
        }

        ReadChar();

        return new Token(TokenType.StringLiteral, tokenText, textPosition);
    }

    private void AdvanceTextPosition(char c)
    {
        nextCharIndex++;

        if (c != '\n')
        {
            textPosition = new TextPosition(textPosition.LineIndex, textPosition.ColumnIndex + 1);
        }
        else
        {
            textPosition = new TextPosition(textPosition.LineIndex + 1, 0);
        }
    }

    private bool IsStillReading => nextCharIndex < sourceCode.Length;

    private bool IsDoneReading => nextCharIndex >= sourceCode.Length;
}