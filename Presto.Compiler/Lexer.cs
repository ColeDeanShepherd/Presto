using System.Text.RegularExpressions;

namespace Presto;

public enum TokenType
{
    Identifier,
    Number,
    StringLiteral,

    SingleLineComment,

    Period,
    LeftParen,
    RightParen,
    Semicolon,
    Comma,
    Colon,
    Equals,
    LeftCurlyBracket,
    RightCurlyBracket,

    Whitespace,

    LetKeyword,
    StructKeyword,
    FunctionKeyword
};

public readonly record struct TextPosition(
    int LineIndex,
    int ColumnIndex)
{
    public override string ToString() => $"({LineIndex + 1}, {ColumnIndex + 1})";

    public TextPosition AddColumns(int deltaColumns) =>
        ((ColumnIndex + deltaColumns) < 0)
            ? throw new ArgumentOutOfRangeException(nameof(deltaColumns), $"Sum {nameof(TextPosition)} has a negative {nameof(ColumnIndex)}")
            : new TextPosition(LineIndex, ColumnIndex + deltaColumns);
}

public readonly record struct TextRange(
    TextPosition StartPosition,
    TextPosition EndPosition)
{
    public override string ToString() => $"({StartPosition}, {EndPosition})";
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
    public string GetDescription() =>
        (expectedCharacter == null)
            ? $"Unexpected character: '{encounteredCharacter}'."
            : $"Expected character '{expectedCharacter}' but encountered '{encounteredCharacter}'.";
};

public record EndOfSourceCodeError(
    TextPosition TextPosition
) : ILexerError
{
    public TextRange TextRange => new(TextPosition, TextPosition);

    public string GetDescription() => "Unexpectedly reached the end of the source code.";
};

public class Lexer
{
    public static List<(Regex Regex, TokenType TokenType)> Rules = new()
    {
        (new Regex(@"\."), TokenType.Period),
        (new Regex(@"\("), TokenType.LeftParen),
        (new Regex(@"\)"), TokenType.RightParen),
        (new Regex(";"), TokenType.Semicolon),
        (new Regex(","), TokenType.Comma),
        (new Regex(":"), TokenType.Colon),
        (new Regex("="), TokenType.Equals),
        (new Regex("{"), TokenType.LeftCurlyBracket),
        (new Regex("}"), TokenType.RightCurlyBracket),
        (new Regex("let"), TokenType.LetKeyword),
        (new Regex("struct"), TokenType.StructKeyword),
        (new Regex("fn"), TokenType.FunctionKeyword),
        (new Regex(@"\s+"), TokenType.Whitespace),
        (new Regex(@"[_a-zA-Z][_0-9a-zA-Z]*"), TokenType.Identifier),
        (new Regex(@"[0-9]+"), TokenType.Number),
        (new Regex(@"""[^""]*"""), TokenType.StringLiteral),
        (new Regex(@"\s+"), TokenType.Whitespace),
        (new Regex(@"#[^\r\n]*"), TokenType.SingleLineComment)
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
        var rules = Rules
            .Map(r => (Regex: new Regex($"^{r.Regex}"), TokenType: r.TokenType))
            .ToList();

        List<Token> tokens = new();

        while (!IsDoneReading)
        {
            string sourceCodeLeft = sourceCode.Substring(nextCharIndex);

            (Match, TokenType)? match = rules
                .Map(r => (r.Regex.Match(sourceCodeLeft), r.TokenType))
                .FirstOrDefault(x => x.Item1.Success);

            if (match.Value.Item1.Success)
            {
                TextPosition startTextPosition = this.textPosition;

                for (int i = 0; i < match.Value.Item1.Length; i++)
                {
                    if (ReadChar() == null)
                    {
                        throw new Exception();
                    }
                }

                TextPosition endTextPosition = this.textPosition;

                tokens.Add(new Token(match.Value.Item2, match.Value.Item1.Value, new TextRange(startTextPosition, endTextPosition)));
            }
            else
            {
                errors.Add(new UnexpectedCharacterError(new TextRange(textPosition, GetNextTextPosition()), PeekChar()!.Value));
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

    private char? TryPeekChar() =>
        IsStillReading
            ? sourceCode[nextCharIndex]
            : null;

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

    private TextPosition GetNextTextPosition()
    {
        if (IsStillReading)
        {
            char c = sourceCode[nextCharIndex];

            return
                (c != '\n')
                    ? new TextPosition(textPosition.LineIndex, textPosition.ColumnIndex + 1)
                    : new TextPosition(textPosition.LineIndex + 1, 0);
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