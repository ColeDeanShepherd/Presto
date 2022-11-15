namespace Presto.Compiler;

using Presto.ParseTree;
using static GrammarBuilder;

public static class PrestoGrammarConstants
{
    public static readonly List<GrammarRule> Grammar = new()
    {
        Rule(
            "Program",
            children => new Program(children),
            TokenTerminated(RuleRef("Statement"), TokenType.Semicolon).ZeroOrMore()),
        Rule(
            "Statement",
            children => new Statement(children),
            OneOf(RuleRef("LetStatement"), RuleRef("StructDeclaration"), RuleRef("Expression"))),
        Rule(
            "LetStatement",
            children => new LetStatement(children),
            Token(TokenType.LetKeyword), Token(TokenType.Identifier), Token(TokenType.Colon), RuleRef("QualifiedName"), Token(TokenType.Equals), RuleRef("Expression")),
        Rule(
            "StructDeclaration",
            children => new StructDeclaration(children),
            Token(TokenType.StructKeyword), Token(TokenType.Identifier), Token(TokenType.LeftCurlyBracket), TokenSeparated(RuleRef("FieldDeclaration"), TokenType.Comma), Token(TokenType.RightCurlyBracket)),
        Rule(
            "FieldDeclaration",
            children => new FieldDeclaration(children),
            Token(TokenType.Identifier), Token(TokenType.Colon), RuleRef("QualifiedName")),
        Rule(
            "Expression",
            children => children.First(),
            new ExpressionGrammarNode(
                PrefixExpressionNode: OneOf(
                    RuleRef("NumberLiteral"),
                    RuleRef("StringLiteral"),
                    RuleRef("Identifier")
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
        Rule(
            "CallExpression",
            children => new CallExpression(children),
            RuleRef("Expression"), Token(TokenType.LeftParen), TokenSeparated(RuleRef("Expression"), TokenType.Comma), Token(TokenType.RightParen)),
        Rule(
            "MemberAccessOperator",
            children => new MemberAccessOperator(children),
            RuleRef("Expression"), Token(TokenType.Period), RuleRef("Expression")), // TODO: is 2nd expr OK?
        Rule(
            "QualifiedName",
            children => new QualifiedName(children),
            TokenSeparated(Token(TokenType.Identifier), TokenType.Period, OneOrMore: true)),
        Rule(
            "Identifier",
            children => new Identifier(((TerminalParseTreeNode)children.First()).Token.Text),
            Token(TokenType.Identifier)),
        Rule(
            "NumberLiteral",
            children => new NumberLiteral(((TerminalParseTreeNode)children.First()).Token.Text),
            Token(TokenType.Number)),
        Rule(
            "StringLiteral",
            children => new StringLiteral(((TerminalParseTreeNode)children.First()).Token.Text),
            Token(TokenType.StringLiteral))
    };
}