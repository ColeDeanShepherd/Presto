using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Presto.Lexer
{
    public enum TokenType
    {
        Identifier,

        // keywords
        FnKeyword,

        // literals
        StringLiteral,

        // punctuation
        LParen,
        RParen,
        LCurlyBrace,
        RCurlyBrace,
        Period,
        Semicolon
    }

    public struct TextPosition
    {
        public readonly int LineNumber;
        public readonly int ColumnNumber;

        public TextPosition(int lineNumber, int columnNumber)
        {
            // preconditions
            Debug.Assert(lineNumber > 0);
            Debug.Assert(columnNumber > 0);

            // body
            LineNumber = lineNumber;
            ColumnNumber = columnNumber;
        }
    }

    public struct Token
    {
        public readonly TokenType Type;
        public readonly string Text;
        public readonly TextPosition Position;

        public Token(TokenType type, string text, TextPosition position)
        {
            Type = type;
            Text = text;
            Position = position;
        }
    }

    public struct LexerError
    {
        public readonly string Description;
        public readonly TextPosition Position;

        public LexerError(string description, TextPosition position)
        {
            Description = description;
            Position = position;
        }
    }

    public class Lexer
    {
        public string SourceCode { get; private set; }
        public TextPosition Position { get; private set; }
        public List<Token> Tokens { get; private set; }
        public List<LexerError> Errors { get; private set; }

        public Result<List<Token>, List<LexerError>> Tokenize(string sourceCode)
        {
            // preconditions
            Debug.Assert(sourceCode != null);

            // body
            SourceCode = sourceCode;
            Position = new TextPosition(lineNumber: 1, columnNumber: 1);
            Tokens = new List<Token>();
            Errors = new List<LexerError>();
            charIndex = 0;

            char? nextChar;
            while ((nextChar = PeekPossibleCharacter()) != null)
            {
                if (IsValidIdentifierCharacter(nextChar.Value))
                {
                    var token = ReadIdentifierOrKeyword();
                    if (token != null)
                    {
                        Tokens.Add(token.Value);
                    }
                }
                else if (nextChar.Value == '"')
                {
                    var token = ReadStringLiteral();
                    if (token != null)
                    {
                        Tokens.Add(token.Value);
                    }
                }
                else if (SingleCharacterTokenTypes.TryGetValue(nextChar.Value, out TokenType nextTokenType))
                {
                    Tokens.Add(new Token(nextTokenType, nextChar.Value.ToString(), Position));
                    var _ = ReadCharacter();
                }
                else if (char.IsWhiteSpace(nextChar.Value))
                {
                    SkipWhitespace();
                }
                else
                {
                    Errors.Add(new LexerError($"Unexpected character: {nextChar.Value}", Position));
                    var _ = ReadCharacter();
                }
            }

            return !Errors.Any()
                ? Result<List<Token>, List<LexerError>>.Ok(Tokens)
                : Result<List<Token>, List<LexerError>>.Err(Errors);
        }

        private static readonly Dictionary<string, TokenType> KeywordTokenTypes = new Dictionary<string, TokenType>
        {
            { "fn", TokenType.FnKeyword }
        };
        private static readonly Dictionary<char, TokenType> SingleCharacterTokenTypes = new Dictionary<char, TokenType>
        {
            { '(', TokenType.LParen },
            { ')', TokenType.RParen },
            { '{', TokenType.LCurlyBrace },
            { '}', TokenType.RCurlyBrace },
            { '.', TokenType.Period },
            { ';', TokenType.Semicolon }
        };

        private int charIndex;

        #region Helper Methods

        private char? PeekPossibleCharacter()
        {
            return (charIndex < SourceCode.Length)
                ? SourceCode[charIndex]
                : (char?)null;
        }
        private char? PeekCharacter()
        {
            var possibleChar = PeekPossibleCharacter();
            if (possibleChar == null)
            {
                Errors.Add(new LexerError("Unexpectedly reached the end of the source code.", Position));
            }

            return possibleChar;
        }
        private char? ReadCharacter()
        {
            var possibleChar = PeekCharacter();
            if (possibleChar != null)
            {
                charIndex++;

                if (possibleChar != '\n')
                {
                    Position = new TextPosition(Position.LineNumber, Position.ColumnNumber + 1);
                }
                else
                {
                    Position = new TextPosition(Position.LineNumber + 1, columnNumber: 1);
                }
            }
            // PeekCharacter already outputs an error if possibleChar != null

            return possibleChar;
        }
        private char? ReadExpectedCharacter(char expectedChar)
        {
            var actualChar = ReadCharacter();
            if ((actualChar != null) && (actualChar.Value != expectedChar))
            {
                var errorDescription = $"Expected '{expectedChar}' ({(int)expectedChar}) but read '{actualChar.Value}' ({(int)actualChar.Value})";
                Errors.Add(new LexerError(errorDescription, Position));
            }
            // ReadCharacter already outputs an error if actualChar == null

            return actualChar;
        }

        private void SkipWhitespace()
        {
            char? nextChar;
            while (((nextChar = PeekPossibleCharacter()) != null) && char.IsWhiteSpace(nextChar.Value))
            {
                ReadCharacter();
            }
        }

        private bool IsValidIdentifierCharacter(char c)
        {
            return char.IsLetter(c);
        }
        private Token? ReadIdentifierOrKeyword()
        {
            var startCharIndex = charIndex;

            char? nextChar;
            while (((nextChar = PeekPossibleCharacter()) != null) && IsValidIdentifierCharacter(nextChar.Value))
            {
                var _ = ReadCharacter();
            }

            if (charIndex == startCharIndex)
            {
                Errors.Add(new LexerError("Failed reading identifier", Position));
                return null;
            }

            var textLength = charIndex - startCharIndex;
            var text = SourceCode.Substring(startCharIndex, textLength);
            TokenType tokenType;
            tokenType = KeywordTokenTypes.TryGetValue(text, out tokenType)
                ? tokenType
                : TokenType.Identifier;

            return new Token(tokenType, text, Position);
        }

        private Token? ReadStringLiteral()
        {
            if (ReadExpectedCharacter('"') == null) { return null; }

            var startCharIndex = charIndex;

            char? nextChar;
            while (((nextChar = PeekPossibleCharacter()) != null) && (nextChar.Value != '"'))
            {
                var _ = ReadCharacter();
            }

            if (charIndex == startCharIndex)
            {
                Errors.Add(new LexerError("Failed reading string literal", Position));
                return null;
            }

            var textLength = charIndex - startCharIndex;
            var text = SourceCode.Substring(startCharIndex, textLength);

            if (ReadExpectedCharacter('"') == null) { return null; }

            return new Token(TokenType.StringLiteral, text, Position);
        }

        #endregion
    }
}
