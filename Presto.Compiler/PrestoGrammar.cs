namespace Presto.Compiler;

using Presto.ParseTree;
using System.Text.RegularExpressions;
using static GrammarBuilder;

public static class PrestoGrammarConstants
{
    public static List<(Regex Regex, TokenType TokenType)> LexerGrammar = new()
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

    public static readonly List<GrammarRule> Grammar = new()
    {
        Rule(
            "Program",
            children => new Program(children),
            RuleRef("Statement").ZeroOrMore()),
        Rule(
            "Statement",
            children => new Statement(children),
            OneOf(RuleRef("LetStatement"), RuleRef("FunctionDefinition"), RuleRef("StructDeclaration"), RuleRef("Expression"))),
        Rule(
            "LetStatement",
            children => new LetStatement(children),
            Token(TokenType.LetKeyword), Token(TokenType.Identifier), Token(TokenType.Colon), RuleRef("QualifiedName"), Token(TokenType.Equals), RuleRef("Expression")),
        Rule(
            "FunctionDefinition",
            children => new FunctionDefinition(children),
            Token(TokenType.FunctionKeyword), Token(TokenType.Identifier),
            Token(TokenType.LeftParen), TokenSeparated(RuleRef("FieldDeclaration"), TokenType.Comma), Token(TokenType.RightParen),
            Token(TokenType.LeftCurlyBracket), Token(TokenType.RightCurlyBracket)),
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