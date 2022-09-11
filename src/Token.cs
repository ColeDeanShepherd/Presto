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

public record Token(TokenType Type, string Text);