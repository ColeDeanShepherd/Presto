namespace Presto;

public enum TokenType
{
    Identifier,
    StringLiteral,

    Period,
    LeftParen,
    RightParen,
    Semicolon
};

public readonly record struct TextPosition(
    int LineIndex,
    int ColumnIndex);

public record Token(
    TokenType Type,
    string Text,
    TextPosition TextPosition);