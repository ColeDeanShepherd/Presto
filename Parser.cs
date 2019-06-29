using Presto.AST;
using Presto.Lexer;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Presto.Parser
{
    public struct ParserError
    {
        public readonly string Description;
        public readonly TextPosition Position;

        public ParserError(string description, TextPosition position)
        {
            Description = description;
            Position = position;
        }
    }

    public class Parser
    {
        public (AST.Program, List<ParserError>) Parse(List<Token> tokens)
        {
            // preconditions
            Debug.Assert(tokens != null);

            // body
            this.tokens = tokens;
            tokenIndex = 0;
            errors = new List<ParserError>();

            var program = new AST.Program
            {
                Definitions = new List<IDefinition>()
            };

            Token? nextToken;
            while ((nextToken = PeekPossibleToken()) != null)
            {
                if (nextToken.Value.Type == TokenType.FnKeyword)
                {
                    var functionDefinition = ReadFunctionDefinition();
                    if (functionDefinition != null)
                    {
                        program.Definitions.Add(functionDefinition);
                    }
                }
                else
                {
                    errors.Add(new ParserError($"Unexpected token type: {nextToken.Value.Type}", Position));
                    var _ = ReadToken();
                }
            }

            return (program, errors);
        }

        private List<Token> tokens;
        private int tokenIndex;
        private List<ParserError> errors;

        private TextPosition Position
        {
            get
            {
                if (!tokens.Any()) { return new TextPosition(lineNumber: 1, columnNumber: 1); }
                if (tokenIndex >= tokens.Count) { return tokens.Last().Position; }
                return tokens[tokenIndex].Position;
            }
        }

        #region Helper Methods

        private Token? PeekPossibleToken()
        {
            return (tokenIndex < tokens.Count)
                ? tokens[tokenIndex]
                : (Token?)null;
        }
        private Token? PeekToken()
        {
            var possibleToken = PeekPossibleToken();
            if (possibleToken == null)
            {
                errors.Add(new ParserError("Unexpectedly reached the end of the source code.", Position));
            }

            return possibleToken;
        }
        private Token? ReadToken()
        {
            var possibleToken = PeekToken();
            if (possibleToken != null)
            {
                tokenIndex++;
            }
            // PeekToken already outputs an error if possibleToken != null

            return possibleToken;
        }
        private Token? ReadExpectedToken(TokenType expectedTokenType)
        {
            var actualToken = ReadToken();
            if ((actualToken != null) && (actualToken.Value.Type != expectedTokenType))
            {
                var errorDescription = $"Expected {expectedTokenType} but read \"{actualToken.Value.Text}\" ({actualToken.Value.Type})";
                errors.Add(new ParserError(errorDescription, Position));
            }
            // ReadToken already outputs an error if actualToken == null

            return actualToken;
        }

        private FunctionDefinition ReadFunctionDefinition()
        {
            if (ReadExpectedToken(TokenType.FnKeyword) == null) { return null; }

            var functionName = ReadIdentifier();
            if (functionName == null) { return null; }

            if (ReadExpectedToken(TokenType.LParen) == null) { return null; }
            if (ReadExpectedToken(TokenType.RParen) == null) { return null; }

            var functionBody = ReadBlock();
            if (functionBody == null) { return null; }

            return new FunctionDefinition
            {
                Name = functionName,
                Body = functionBody
            };
        }
        private Block ReadBlock()
        {
            if (ReadExpectedToken(TokenType.LCurlyBrace) == null) { return null; }

            var statements = new List<IStatement>();

            Token? nextToken;
            while (((nextToken = PeekPossibleToken()) != null) && (nextToken.Value.Type != TokenType.RCurlyBrace))
            {
                var statement = ReadStatement();
                if (statement != null)
                {
                    statements.Add(statement);
                }
            }

            if (ReadExpectedToken(TokenType.RCurlyBrace) == null) { return null; }

            return new Block
            {
                Statements = statements
            };
        }
        private IStatement ReadStatement()
        {
            var nextToken = PeekToken();
            if (nextToken == null) { return null; }

            var expression = ReadExpression();
            if (expression == null) { return null; }

            var expressionStatement = expression as IStatement;
            if (expressionStatement == null)
            {
                errors.Add(new ParserError("Invalid expression statement", Position));
                return null;
            }

            var _ = ReadExpectedToken(TokenType.Semicolon);

            return expressionStatement;
        }
        private IExpression ReadExpression()
        {
            var expression = ReadPrefixExpression();
            if (expression == null) { return null; }

            Token? nextToken;
            while ((nextToken = PeekPossibleToken()) != null)
            {
                if (nextToken.Value.Type == TokenType.Period)
                {
                    expression = ReadMemberAccessOperator(expression);
                    if (expression == null) { return null; }
                }
                else if (nextToken.Value.Type == TokenType.LParen)
                {
                    expression = ReadFunctionCallOperator(expression);
                    if (expression == null) { return null; }
                }
                else
                {
                    break;
                }
            }

            return expression;
        }
        private IExpression ReadPrefixExpression()
        {
            var nextToken = PeekToken();
            if (nextToken == null) { return null; }

            switch (nextToken.Value.Type)
            {
                case TokenType.Identifier:
                    return ReadIdentifier();
                case TokenType.StringLiteral:
                    return ReadStringLiteral();
                default:
                    var errorDescription = $"Unexpected token while parsing expression: \"{nextToken.Value.Text}\" ({nextToken.Value.Type})";
                    errors.Add(new ParserError(errorDescription, Position));
                    return null;
            }
        }
        private MemberAccessOperator ReadMemberAccessOperator(IExpression memberContainer)
        {
            var _ = ReadExpectedToken(TokenType.Period);

            var identifier = ReadIdentifier();
            if (identifier == null) { return null; }

            return new MemberAccessOperator
            {
                MemberContainer = memberContainer,
                MemberIdentifier = identifier
            };
        }
        private FunctionCall ReadFunctionCallOperator(IExpression functionExpression)
        {
            if (ReadExpectedToken(TokenType.LParen) == null) { return null; }

            var arguments = new List<IExpression>();

            Token? nextToken;
            while (((nextToken = PeekToken()) != null) && (nextToken.Value.Type != TokenType.RParen))
            {
                if (arguments.Any())
                {
                    if (ReadExpectedToken(TokenType.Comma) == null) { return null; }
                }

                var argument = ReadExpression();
                if (argument == null) { return null; }

                arguments.Add(argument);
            }

            if (ReadExpectedToken(TokenType.RParen) == null) { return null; }

            return new FunctionCall
            {
                FunctionExpression = functionExpression,
                Arguments = arguments
            };
        }

        private Identifier ReadIdentifier()
        {
            var nextToken = ReadExpectedToken(TokenType.Identifier);
            if (nextToken == null) { return null; }

            return new Identifier { Text = nextToken.Value.Text };
        }
        private StringLiteral ReadStringLiteral()
        {
            var nextToken = ReadExpectedToken(TokenType.StringLiteral);
            if (nextToken == null) { return null; }

            return new StringLiteral { Value = nextToken.Value.Text };
        }

        #endregion
    }
}
