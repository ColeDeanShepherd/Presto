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
        List<Token> tokens = new();

        while (!IsDoneReading)
        {
            char nextChar = PeekChar();

            if (IsValidFirstIdentifierChar(nextChar))
            {
                tokens.Add(ReadIdentifier());
            }
            else if (nextChar == '"')
            {
                tokens.Add(ReadStringLiteral());
            }
            else if (nextChar == '.')
            {
                tokens.Add(ReadSingleCharacterToken(nextChar, TokenType.Period));
            }
            else if (nextChar == '(')
            {
                tokens.Add(ReadSingleCharacterToken(nextChar, TokenType.LeftParen));
            }
            else if (nextChar == ')')
            {
                tokens.Add(ReadSingleCharacterToken(nextChar, TokenType.RightParen));
            }
            else if (nextChar == ';')
            {
                tokens.Add(ReadSingleCharacterToken(nextChar, TokenType.Semicolon));
            }
            else
            {
                throw new Exception();
            }
        }

        return tokens;
    }

    private string sourceCode;
    private int nextCharIndex;
    private TextPosition textPosition;

    #region Helpers

    private bool IsStillReading => nextCharIndex < sourceCode.Length;
    private bool IsDoneReading => nextCharIndex >= sourceCode.Length;

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
            MoveToNextChar();
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

    private bool IsValidFirstIdentifierChar(char c)
    {
        return char.IsLetter(c) || (c == '_');
    }

    private bool IsValidIdentifierChar(char c)
    {
        return char.IsLetterOrDigit(c) || (c == '_');
    }

    private Token ReadIdentifier()
    {
        TextPosition textPosition = this.textPosition;
        int startCharIndex = nextCharIndex;

        while (!IsDoneReading && (IsValidIdentifierChar(PeekChar())))
        {
            ReadChar();
        }

        int tokenTextLength = nextCharIndex - startCharIndex;
        string tokenText = sourceCode.Substring(startCharIndex, tokenTextLength);
        return new Token(TokenType.Identifier, tokenText, textPosition);
    }

    private Token ReadStringLiteral()
    {
        TextPosition textPosition = this.textPosition;

        ReadExpectedChar('"');

        int valueStartCharIndex = nextCharIndex;

        while (PeekChar() != '"')
        {
            ReadChar();
        }

        int tokenTextLength = nextCharIndex - valueStartCharIndex;

        ReadExpectedChar('"');

        string tokenText = sourceCode.Substring(valueStartCharIndex, tokenTextLength);
        return new Token(TokenType.StringLiteral, tokenText, textPosition);
    }

    private Token ReadSingleCharacterToken(char c, TokenType tokenType)
    {
        TextPosition textPosition = this.textPosition;
        return new Token(tokenType, ReadExpectedChar(c).ToString(), textPosition);
    }

    private void MoveToNextChar()
    {
        if (IsStillReading)
        {
            char c = sourceCode[nextCharIndex];

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
    }

    #endregion Helpers
}