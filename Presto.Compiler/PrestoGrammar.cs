namespace Presto.Compiler;

using static GrammarBuilder;

public static class PrestoGrammarConstants
{
    public static readonly List<GrammarRule> Grammar = new()
    {
        Rule("Program", Group(RuleRef("Statement"), Token(TokenType.Semicolon)).ZeroOrMore()),
        Rule("Statement", OneOf(RuleRef("LetStatement"), RuleRef("StructDeclaration"), RuleRef("Expression"))),
        Rule("LetStatement", Token(TokenType.LetKeyword), Token(TokenType.Identifier), Token(TokenType.Colon), RuleRef("QualifiedName"), Token(TokenType.Equals), RuleRef("Expression")),
        Rule("StructDeclaration", Token(TokenType.StructKeyword), Token(TokenType.Identifier), Token(TokenType.LeftCurlyBracket), TokenSeparated(RuleRef("FieldDeclaration"), TokenType.Comma), Token(TokenType.RightCurlyBracket)),
        Rule("FieldDeclaration", Token(TokenType.Identifier), Token(TokenType.Colon), RuleRef("QualifiedName")),
        Rule(
            "Expression",
            new ExpressionGrammarNode(
                PrefixExpressionNode: OneOf(
                    Token(TokenType.Number),
                    Token(TokenType.StringLiteral),
                    Token(TokenType.Identifier)
                ),
                PostfixOperatorLeftBindingPowers: new Dictionary<TokenType, int?>
                {
                    { TokenType.LeftParen, 12 },
                },
                InfixOperatorBindingPowers: new Dictionary<TokenType, (int, int)?>
                {
                    { TokenType.Period, (14, 13) }
                }
            )
        ),
        Rule("CallExpression", RuleRef("Expression"), Token(TokenType.LeftParen), TokenSeparated(RuleRef("Expression"), TokenType.Comma), Token(TokenType.RightParen)),
        Rule("MemberAccessOperator", RuleRef("Expression"), Token(TokenType.Period), Token(TokenType.Identifier)),
        Rule("QualifiedName", TokenSeparated(Token(TokenType.Identifier), TokenType.Period, OneOrMore: true))
    };
}