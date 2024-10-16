package plc.project;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Standard JUnit5 parameterized tests. See the RegexTests file from Homework 1
 * or the LexerTests file from the last project part for more information.
 */
final class ParserExpressionTests {

    @ParameterizedTest
    @MethodSource
    void testExpressionStatement(String test, List<Token> tokens, Ast.Statement.Expression expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testExpressionStatement() {
        return Stream.of(
                Arguments.of("Function Expression",
                        Arrays.asList(
                                // name();
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "(", 4),
                                new Token(Token.Type.OPERATOR, ")", 5),
                                new Token(Token.Type.OPERATOR, ";", 6)
                        ),
                        new Ast.Statement.Expression(new Ast.Expression.Function(Optional.empty(), "name", Arrays.asList()))
                ),
                Arguments.of("Variable",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "expr", 0),
                                new Token(Token.Type.OPERATOR, ";", 4)
                        ),
                        new Ast.Statement.Expression(new Ast.Expression.Access(Optional.empty(), "expr"))
                ),
                Arguments.of("Function",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "func", 0),
                                new Token(Token.Type.IDENTIFIER, "(", 4),
                                new Token(Token.Type.IDENTIFIER, ")", 5),
                                new Token(Token.Type.OPERATOR, ";", 6)
                        ),
                        new Ast.Statement.Expression(new Ast.Expression.Function(Optional.empty(), "func", Arrays.asList()))
                ), Arguments.of("Missing Semicolon",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "f", 0)
                        ),
                        null
                ),
                Arguments.of("Method Call",
                        Arrays.asList(
                                // obj.method
                                new Token(Token.Type.IDENTIFIER, "obj", 0),
                                new Token(Token.Type.OPERATOR, ".", 3),
                                new Token(Token.Type.IDENTIFIER, "method", 4),
                                new Token(Token.Type.OPERATOR, "(", 10),
                                new Token(Token.Type.OPERATOR, ")", 11),
                                new Token(Token.Type.OPERATOR, ";", 12)

                        ),
                        new Ast.Statement.Expression(new Ast.Expression.Function(Optional.of(new Ast.Expression.Access(Optional.empty(), "obj")), "method", Arrays.asList()))
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testAssignmentStatement(String test, List<Token> tokens, Ast.Statement.Assignment expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testAssignmentStatement() {
        return Stream.of(
                Arguments.of("Assignment",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "=", 5),
                                new Token(Token.Type.IDENTIFIER, "value", 7),
                                new Token(Token.Type.OPERATOR, ";", 12)
                        ),
                        new Ast.Statement.Assignment(
                                new Ast.Expression.Access(Optional.empty(), "name"),
                                new Ast.Expression.Access(Optional.empty(), "value")
                        )
                ),
                Arguments.of("Missing Value",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "=", 5),
                                new Token(Token.Type.OPERATOR, ";", 7)
                        ),
                        null
                ),
                Arguments.of("Field",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "obj", 0),
                                new Token(Token.Type.OPERATOR, ".", 3),
                                new Token(Token.Type.IDENTIFIER, "field", 4),
                                new Token(Token.Type.OPERATOR, "=", 6),
                                new Token(Token.Type.IDENTIFIER, "expr", 8),
                                new Token(Token.Type.OPERATOR, ";", 12)
                        ),
                        new Ast.Statement.Assignment(
                                new Ast.Expression.Access(Optional.of(new Ast.Expression.Access(Optional.empty(), "obj")), "field"),
                                new Ast.Expression.Access(Optional.empty(), "expr")
                        )
                ),
                Arguments.of("Missing Semicolon",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "=", 5),
                                new Token(Token.Type.IDENTIFIER, "expr", 7)
                        ),
                        null
                ),
                Arguments.of("Invalid Name",
                        // obj.5
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "obj", 0),
                                new Token(Token.Type.OPERATOR, ".", 3),
                                new Token(Token.Type.IDENTIFIER, "5", 4)
                        ),
                        null
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testLiteralExpression(String test, List<Token> tokens, Ast.Expression.Literal expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testLiteralExpression() {
        return Stream.of(
                Arguments.of("Nil Literal",
                        Arrays.asList(new Token(Token.Type.IDENTIFIER, "NIL", 0)),
                        new Ast.Expression.Literal(null)
                ),
                Arguments.of("Boolean Literal",
                        Arrays.asList(new Token(Token.Type.IDENTIFIER, "TRUE", 0)),
                        new Ast.Expression.Literal(Boolean.TRUE)
                ),
                Arguments.of("Integer Literal",
                        Arrays.asList(new Token(Token.Type.INTEGER, "1", 0)),
                        new Ast.Expression.Literal(new BigInteger("1"))
                ),
                Arguments.of("Decimal Literal",
                        Arrays.asList(new Token(Token.Type.DECIMAL, "2.0", 0)),
                        new Ast.Expression.Literal(new BigDecimal("2.0"))
                ),
                Arguments.of("Character Literal",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'c'", 0)),
                        new Ast.Expression.Literal('c')
                ),
                Arguments.of("String Literal",
                        Arrays.asList(new Token(Token.Type.STRING, "\"string\"", 0)),
                        new Ast.Expression.Literal("string")
                ),
                Arguments.of("Escape Character",
                        Arrays.asList(new Token(Token.Type.STRING, "\"Hello,\\nWorld!\"", 0)),
                        new Ast.Expression.Literal("Hello,\nWorld!")
                ),
                Arguments.of("Character Escape",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'\\b'", 0)),
                        new Ast.Expression.Literal('\b')
                ),
                Arguments.of("Character Escape",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'\\n'", 0)),
                        new Ast.Expression.Literal('\n')
                ),
                Arguments.of("Character Escape",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'\\r'", 0)),
                        new Ast.Expression.Literal('\r')
                ),
                Arguments.of("Character Escape",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'\\t'", 0)),
                        new Ast.Expression.Literal('\t')
                ),
                Arguments.of("Character Escape",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'\\''", 0)),
                        new Ast.Expression.Literal('\'')
                ),
                Arguments.of("Character Escape",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'\\\"'", 0)),
                        new Ast.Expression.Literal('\"')
                ),
                Arguments.of("Character Escape",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'\\\\'", 0)),
                        new Ast.Expression.Literal('\\')
                ),
                Arguments.of("String Escape Double Quote",
                        Arrays.asList(new Token(Token.Type.STRING, "\"\\\"\"", 0)),
                        new Ast.Expression.Literal("\"")
                ),
                Arguments.of("String Escape Single Quote",
                        Arrays.asList(new Token(Token.Type.STRING, "\"\\'\"", 0)),
                        new Ast.Expression.Literal("'")
                ),
                Arguments.of("String Escape Slash",
                        Arrays.asList(new Token(Token.Type.STRING, "\"\\\\\"", 0)),
                        new Ast.Expression.Literal("\\")
                ),
                Arguments.of("String Escape B",
                        Arrays.asList(new Token(Token.Type.STRING, "\"\\b\"", 0)),
                        new Ast.Expression.Literal("\b")
                ),
                Arguments.of("String Escape N",
                        Arrays.asList(new Token(Token.Type.STRING, "\"\\n\"", 0)),
                        new Ast.Expression.Literal("\n")
                ),
                Arguments.of("String Escape R",
                        Arrays.asList(new Token(Token.Type.STRING, "\"\\r\"", 0)),
                        new Ast.Expression.Literal("\r")
                ),
                Arguments.of("String Escape T",
                        Arrays.asList(new Token(Token.Type.STRING, "\"\\t\"", 0)),
                        new Ast.Expression.Literal("\t")
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testGroupExpression(String test, List<Token> tokens, Ast.Expression.Group expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testGroupExpression() {
        return Stream.of(
                Arguments.of("Grouped Variable",
                        Arrays.asList(
                                // (expr)
                                new Token(Token.Type.OPERATOR, "(", 0),
                                new Token(Token.Type.IDENTIFIER, "expr", 1),
                                new Token(Token.Type.OPERATOR, ")", 5)
                        ),
                        new Ast.Expression.Group(new Ast.Expression.Access(Optional.empty(), "expr"))
                ),
                Arguments.of("Grouped Binary",
                        Arrays.asList(
                                // (expr1 + expr2)
                                new Token(Token.Type.OPERATOR, "(", 0),
                                new Token(Token.Type.IDENTIFIER, "expr1", 1),
                                new Token(Token.Type.OPERATOR, "+", 7),
                                new Token(Token.Type.IDENTIFIER, "expr2", 9),
                                new Token(Token.Type.OPERATOR, ")", 14)
                        ),
                        new Ast.Expression.Group(new Ast.Expression.Binary("+",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        ))
                ),
                Arguments.of("Missing Closing Parenthesis",
                        Arrays.asList(
                                // (expr
                                new Token(Token.Type.OPERATOR, "(", 0),
                                new Token(Token.Type.IDENTIFIER, "expr", 1)
                        ),
                        null
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testBinaryExpression(String test, List<Token> tokens, Ast.Expression.Binary expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testBinaryExpression() {
        return Stream.of(
                Arguments.of("Binary And",
                        Arrays.asList(
                                // expr1 && expr2
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "&&", 6),
                                new Token(Token.Type.IDENTIFIER, "expr2", 10)
                        ),
                        new Ast.Expression.Binary("&&",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        )
                ),
                Arguments.of("Binary Equality",
                        Arrays.asList(
                                // expr1 == expr2
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "==", 6),
                                new Token(Token.Type.IDENTIFIER, "expr2", 9)
                        ),
                        new Ast.Expression.Binary("==",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        )
                ),
                Arguments.of("Binary Addition",
                        Arrays.asList(
                                // expr1 + expr2
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "+", 6),
                                new Token(Token.Type.IDENTIFIER, "expr2", 8)
                        ),
                        new Ast.Expression.Binary("+",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        )
                ),
                Arguments.of("Binary Multiplication",
                        Arrays.asList(
                                // expr1 * expr2
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "*", 6),
                                new Token(Token.Type.IDENTIFIER, "expr2", 8)
                        ),
                        new Ast.Expression.Binary("*",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        )
                ),
                Arguments.of("Missing Operand",
                        Arrays.asList(
                                // expr1 -
                                new Token(Token.Type.IDENTIFIER, "expr", 0),
                                new Token(Token.Type.OPERATOR, "-", 5)
                        ),
                        null
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testAccessExpression(String test, List<Token> tokens, Ast.Expression.Access expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testAccessExpression() {
        return Stream.of(
                Arguments.of("Variable",
                        // name
                        Arrays.asList(new Token(Token.Type.IDENTIFIER, "name", 0)),
                        new Ast.Expression.Access(Optional.empty(), "name")
                ),
                Arguments.of("Field Access",
                        Arrays.asList(
                                // obj.field
                                new Token(Token.Type.IDENTIFIER, "obj", 0),
                                new Token(Token.Type.OPERATOR, ".", 3),
                                new Token(Token.Type.IDENTIFIER, "field", 4)
                        ),
                        new Ast.Expression.Access(Optional.of(new
                                Ast.Expression.Access(Optional.empty(), "obj")), "field")
                ),
                Arguments.of("Nested Field Access",
                        Arrays.asList(
                                // obj.field
                                new Token(Token.Type.IDENTIFIER, "obj", 0),
                                new Token(Token.Type.OPERATOR, ".", 3),
                                new Token(Token.Type.IDENTIFIER, "field", 4),
                                new Token(Token.Type.OPERATOR, ".", 9),
                                new Token(Token.Type.IDENTIFIER, "field2", 10)
                        ),
                        new Ast.Expression.Access(Optional.of(new Ast.Expression.Access(Optional.of(new Ast.Expression.Access(Optional.empty(), "obj")), "field")), "field2")
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testFunctionExpression(String test, List<Token> tokens, Ast.Expression.Function expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testFunctionExpression() {
        return Stream.of(
                Arguments.of("Zero Arguments",
                        Arrays.asList(
                                // name()
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "(", 4),
                                new Token(Token.Type.OPERATOR, ")", 5)
                        ),
                        new Ast.Expression.Function(Optional.empty(), "name", Arrays.asList())
                ),
                Arguments.of("Multiple Arguments",
                        Arrays.asList(
                                // name(expr1, expr2, expr3)
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "(", 4),
                                new Token(Token.Type.IDENTIFIER, "expr1", 5),
                                new Token(Token.Type.OPERATOR, ",", 10),
                                new Token(Token.Type.IDENTIFIER, "expr2", 12),
                                new Token(Token.Type.OPERATOR, ",", 17),
                                new Token(Token.Type.IDENTIFIER, "expr3", 19),
                                new Token(Token.Type.OPERATOR, ")", 24)
                        ),
                        new Ast.Expression.Function(Optional.empty(), "name", Arrays.asList(
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2"),
                                new Ast.Expression.Access(Optional.empty(), "expr3")
                        ))
                ),
                Arguments.of("Trailing comma",
                        Arrays.asList(
                                // name(expr,)
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "(", 4),
                                new Token(Token.Type.IDENTIFIER, "expr", 5),
                                new Token(Token.Type.OPERATOR, ",", 9),
                                new Token(Token.Type.OPERATOR, ")", 10)
                        ),
                        null
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testPriorityExpression(String test, List<Token> tokens, Ast.Expression.Binary expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testPriorityExpression() {
        return Stream.of(
                Arguments.of("Addition Multiplication",
                        Arrays.asList(
                                // expr1 + expr2 * expr3
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "+", 5),
                                new Token(Token.Type.IDENTIFIER, "expr2", 6),
                                new Token(Token.Type.OPERATOR, "*", 11),
                                new Token(Token.Type.IDENTIFIER, "expr3", 12)
                        ),

                        new Ast.Expression.Binary("+", new Ast.Expression.Access(Optional.empty(), "expr1"), new Ast.Expression.Binary("*", new Ast.Expression.Access(Optional.empty(), "expr2"), new Ast.Expression.Access(Optional.empty(), "expr3")))
                ),
                Arguments.of("And Or",
                        Arrays.asList(
                                // expr1 && expr2 || expr3
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "&&", 5),
                                new Token(Token.Type.IDENTIFIER, "expr2", 7),
                                new Token(Token.Type.OPERATOR, "||", 12),
                                new Token(Token.Type.IDENTIFIER, "expr3", 14)
                        ),

                        new Ast.Expression.Binary("||", new Ast.Expression.Binary("&&", new Ast.Expression.Access(Optional.empty(), "expr1"), new Ast.Expression.Access(Optional.empty(), "expr2")), new Ast.Expression.Access(Optional.empty(), "expr3"))
                ),
                Arguments.of("Equals Not Equals",
                        Arrays.asList(
                                // expr1 == expr2 != expr3
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "==", 5),
                                new Token(Token.Type.IDENTIFIER, "expr2", 7),
                                new Token(Token.Type.OPERATOR, "!=", 12),
                                new Token(Token.Type.IDENTIFIER, "expr3", 14)
                        ),

                        new Ast.Expression.Binary("!=", new Ast.Expression.Binary("==", new Ast.Expression.Access(Optional.empty(), "expr1"), new Ast.Expression.Access(Optional.empty(), "expr2")), new Ast.Expression.Access(Optional.empty(), "expr3"))
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testDeclarationStatement(String test, List<Token> tokens, Ast.Statement.Declaration expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testDeclarationStatement() {
        return Stream.of(
                Arguments.of("Let",
                        Arrays.asList(
                                // LET expr;
                                new Token(Token.Type.IDENTIFIER, "LET", 0),
                                new Token(Token.Type.IDENTIFIER, "expr", 3),
                                new Token(Token.Type.OPERATOR, ";", 7)
                        ),

                        new Ast.Statement.Declaration("expr", Optional.empty())
                )
        );
    }

    /**
     * Standard test function. If expected is null, a ParseException is expected
     * to be thrown (not used in the provided tests).
     */
    private static <T extends Ast> void test(List<Token> tokens, T expected, Function<Parser, T> function) {
        Parser parser = new Parser(tokens);
        if (expected != null) {
            Assertions.assertEquals(expected, function.apply(parser));
        } else {
            Assertions.assertThrows(ParseException.class, () -> function.apply(parser));
        }
    }

}
