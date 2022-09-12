namespace Presto;

public enum TokenType
{
    Identifier,
    StringLiteral,

    Period,
    LeftParen,
    RightParen,
    Semicolon,
    Comma
};

public readonly record struct TextPosition(
    int LineIndex,
    int ColumnIndex)
{
    public override string ToString()
    {
        return $"({LineIndex + 1}, {ColumnIndex + 1})";
    }

    public TextPosition AddColumns(int deltaColumns)
    {
        if ((ColumnIndex + deltaColumns) < 0)
        {
            throw new ArgumentOutOfRangeException(nameof(deltaColumns), $"Sum {nameof(TextPosition)} has a negative {nameof(ColumnIndex)}");
        }

        return new TextPosition(LineIndex, ColumnIndex + deltaColumns);
    }
}

public readonly record struct TextRange(
    TextPosition StartPosition,
    TextPosition EndPosition)
{
    public override string ToString()
    {
        return $"({StartPosition}, {EndPosition})";
    }
}

public record Token(
    TokenType Type,
    string Text,
    TextRange TextRange);

public interface ILexerError
{
    public TextRange TextRange { get; }

    public string GetDescription();
}

public record UnexpectedCharacterError(
    TextRange TextRange,
    char encounteredCharacter,
    char? expectedCharacter = null
) : ILexerError
{
    public string GetDescription()
    {
        return (expectedCharacter == null)
            ? $"Unexpected character: '{encounteredCharacter}'."
            : $"Expected character '{expectedCharacter}' but encountered '{encounteredCharacter}'.";
    }
};

public record EndOfSourceCodeError(
    TextPosition TextPosition
) : ILexerError
{
    public TextRange TextRange => new(TextPosition, TextPosition);

    public string GetDescription()
    {
        return $"Unexpectedly reached the end of the source code.";
    }
};

public class Lexer
{
    public static readonly Dictionary<char, TokenType> TokenTypesBySingleCharacters = new()
    {
        { '.', TokenType.Period },
        { '(', TokenType.LeftParen },
        { ')', TokenType.RightParen },
        { ';', TokenType.Semicolon },
        { ',', TokenType.Comma },
    };

    public Lexer(string sourceCode)
    {
        this.sourceCode = sourceCode;
        nextCharIndex = 0;
        textPosition = new(0, 0);
        errors = new List<ILexerError>();
    }

    public (List<Token>, List<ILexerError>) Tokenize()
    {
        List<Token> tokens = new();

        while (!IsDoneReading)
        {
            char nextChar = PeekChar()!.Value;

            if (IsValidIdentifierChar(nextChar, identifierCharIndex: 0))
            {
                tokens.Add(ReadIdentifier());
            }
            else if (nextChar == '"')
            {
                Token? stringLiteral = ReadStringLiteral();

                if (stringLiteral != null)
                {
                    tokens.Add(stringLiteral);
                }
            }
            else if (TokenTypesBySingleCharacters.ContainsKey(nextChar))
            {
                Token? token = ReadSingleCharacterToken(nextChar, TokenTypesBySingleCharacters[nextChar]);
                if (token != null)
                {
                    tokens.Add(token);
                }
            }
            else
            {
                errors.Add(new UnexpectedCharacterError(new TextRange(textPosition, GetNextTextPosition()), nextChar));
                MoveToNextChar();
            }
        }

        return (tokens, errors);
    }

    private string sourceCode;
    private int nextCharIndex;
    private TextPosition textPosition;
    private List<ILexerError> errors;

    #region Helpers

    private bool IsStillReading => nextCharIndex < sourceCode.Length;
    private bool IsDoneReading => nextCharIndex >= sourceCode.Length;

    private char? TryPeekChar()
    {
        return IsStillReading
            ? sourceCode[nextCharIndex]
            : null;
    }

    private char? PeekChar()
    {
        char? nextChar = TryPeekChar();

        if (nextChar == null)
        {
            errors.Add(new EndOfSourceCodeError(textPosition));
            return null;
        }

        return nextChar;
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

    private char? ReadChar()
    {
        char? nextChar = TryReadChar();

        if (nextChar == null)
        {
            errors.Add(new EndOfSourceCodeError(textPosition));
            return null;
        }

        return nextChar;
    }

    private char? ReadExpectedChar(char expectedChar)
    {
        char? nextChar = ReadChar();

        if ((nextChar != null) && (nextChar != expectedChar))
        {
            errors.Add(new UnexpectedCharacterError(new TextRange(textPosition, GetNextTextPosition()), nextChar.Value, expectedChar));
            return null;
        }

        return nextChar;
    }

    private bool IsValidIdentifierChar(char c, uint identifierCharIndex)
    {
        if (identifierCharIndex == 0)
        {
            return char.IsLetter(c) || (c == '_');
        }
        else
        {
            return char.IsLetterOrDigit(c) || (c == '_');
        }
    }

    private Token ReadIdentifier()
    {
        TextPosition startTextPosition = this.textPosition;
        int startCharIndex = nextCharIndex;

        uint GetIdentifierCharIndex() => (uint)(nextCharIndex - startCharIndex);

        while (!IsDoneReading && (IsValidIdentifierChar(PeekChar()!.Value, GetIdentifierCharIndex())))
        {
            ReadChar();
        }

        int tokenTextLength = nextCharIndex - startCharIndex;

        if (tokenTextLength == 0)
        {
            if (IsDoneReading)
            {
                errors.Add(new EndOfSourceCodeError(this.textPosition));
            }
            else
            {
                errors.Add(new UnexpectedCharacterError(new TextRange(startTextPosition, GetNextTextPosition()), PeekChar()!.Value));
            }
        }

        TextPosition endTextPosition = this.textPosition;
        string tokenText = sourceCode.Substring(startCharIndex, tokenTextLength);
        return new Token(TokenType.Identifier, tokenText, new TextRange(startTextPosition, endTextPosition));
    }

    private Token? ReadStringLiteral()
    {
        TextPosition startTextPosition = this.textPosition;

        if (ReadExpectedChar('"') == null)
        {
            return null;
        }

        int valueStartCharIndex = nextCharIndex;

        while (true)
        {
            char? nextChar = PeekChar();

            if (nextChar == null)
            {
                return null;
            }
            else if (nextChar.Value == '"')
            {
                break;
            }

            ReadChar();
        }

        int tokenTextLength = nextCharIndex - valueStartCharIndex;

        if (ReadExpectedChar('"') == null)
        {
            return null;
        }

        TextPosition endTextPosition = this.textPosition;
        string tokenText = sourceCode.Substring(valueStartCharIndex, tokenTextLength);
        return new Token(TokenType.StringLiteral, tokenText, new TextRange(startTextPosition, endTextPosition));
    }

    private Token? ReadSingleCharacterToken(char c, TokenType tokenType)
    {
        TextPosition startTextPosition = this.textPosition;

        char? nextChar = ReadExpectedChar(c);
        if (nextChar == null)
        {
            return null;
        }

        TextPosition endTextPosition = this.textPosition;

        return new Token(tokenType, nextChar.Value.ToString(), new TextRange(startTextPosition, endTextPosition));
    }

    private TextPosition GetNextTextPosition()
    {
        if (IsStillReading)
        {
            char c = sourceCode[nextCharIndex];

            if (c != '\n')
            {
                return new TextPosition(textPosition.LineIndex, textPosition.ColumnIndex + 1);
            }
            else
            {
                return new TextPosition(textPosition.LineIndex + 1, 0);
            }
        }
        else
        {
            return textPosition;
        }
    }

    private void MoveToNextChar()
    {
        if (IsStillReading)
        {
            textPosition = GetNextTextPosition();
            nextCharIndex++;
        }
    }

    #endregion Helpers
}