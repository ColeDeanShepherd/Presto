namespace Presto.Compiler;

using static GrammarBuilder;

public static class PrestoGrammarConstants
{
    public static readonly List<GrammarRule> Grammar = new()
    {
        Rule("Program", TokenTerminated(RuleRef("Statement"), TokenType.Semicolon).ZeroOrMore()),
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
                PostfixOperatorLeftBindingPowers: new Dictionary<TokenType, (int, IGrammarNode)>
                {
                    { TokenType.LeftParen, (12, RuleRef("CallExpression")) },
                },
                InfixOperatorBindingPowers: new Dictionary<TokenType, (int, int, IGrammarNode)>
                {
                    { TokenType.Period, (14, 13, RuleRef("MemberAccessOperator")) }
                }
            )
        ),
        Rule("CallExpression", RuleRef("Expression"), Token(TokenType.LeftParen), TokenSeparated(RuleRef("Expression"), TokenType.Comma), Token(TokenType.RightParen)),
        Rule("MemberAccessOperator", RuleRef("Expression"), Token(TokenType.Period), RuleRef("Expression")), // TODO: is 2nd expr OK?
        Rule("QualifiedName", TokenSeparated(Token(TokenType.Identifier), TokenType.Period, OneOrMore: true))
    };
}