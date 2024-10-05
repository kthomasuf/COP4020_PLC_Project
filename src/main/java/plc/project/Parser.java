package plc.project;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The parser takes the sequence of tokens emitted by the lexer and turns that
 * into a structured representation of the program, called the Abstract Syntax
 * Tree (AST).
 *
 * The parser has a similar architecture to the lexer, just with {@link Token}s
 * instead of characters. As before, {@link #peek(Object...)} and {@link
 * #match(Object...)} are helpers to make the implementation easier.
 *
 * This type of parser is called <em>recursive descent</em>. Each rule in our
 * grammar will have it's own function, and reference to other rules correspond
 * to calling those functions.
 */
public final class Parser {

    private final TokenStream tokens;

    public Parser(List<Token> tokens) {
        this.tokens = new TokenStream(tokens);
    }

    /**
     * Parses the {@code source} rule.
     */
    public Ast.Source parseSource() throws ParseException {
        // returning an Ast.Source object so we need to return List<Fields> and List<Methods>
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code expression} rule.
     */
    public Ast.Expression parseExpression() throws ParseException {
        return parseLogicalExpression();
    }

    /**
     * Parses the {@code logical-expression} rule.
     */
    public Ast.Expression parseLogicalExpression() throws ParseException {
        Ast.Expression expr = parseEqualityExpression();

        if (peek("||")) {
            match("||");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseEqualityExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("&&")) {
            match("&&");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseEqualityExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }

        return expr;
    }

    /**
     * Parses the {@code equality-expression} rule.
     */
    public Ast.Expression parseEqualityExpression() throws ParseException {
        Ast.Expression expr = parseAdditiveExpression();

        if (peek("<")) {
            match("<");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseAdditiveExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("<=")) {
            match("<=");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseAdditiveExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek(">")) {
            match(">");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseAdditiveExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek(">=")) {
            match(">=");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseAdditiveExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("==")) {
            match("==");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseAdditiveExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("!=")) {
            match("!=");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseAdditiveExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }

        return expr;
    }

    /**
     * Parses the {@code additive-expression} rule.
     */
    public Ast.Expression parseAdditiveExpression() throws ParseException {
        Ast.Expression expr = parseMultiplicativeExpression();

        if (peek("+")) {
            match("+");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseMultiplicativeExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("-")) {
            match("-");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseMultiplicativeExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }

        return expr;
    }

    /**
     * Parses the {@code multiplicative-expression} rule.
     */
    public Ast.Expression parseMultiplicativeExpression() throws ParseException {
        Ast.Expression expr = parseSecondaryExpression();

        if (peek("/")) {
            match("/");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseSecondaryExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("*")) {
            match("*");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseSecondaryExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }

        return expr;
    }

    /**
     * Parses the {@code secondary-expression} rule.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
        Ast.Expression expr = parsePrimaryExpression();

        if (peek(".", Token.Type.IDENTIFIER)) {
            match(".", Token.Type.IDENTIFIER);
            String firstIdentifier = tokens.get(-1).getLiteral();

            if (peek("(")) {
                match("(");
                List<Ast.Expression> arguments = new ArrayList<Ast.Expression>();

                while (!peek(")")) {
                    Ast.Expression exprArg = parseExpression();
                    arguments.add(exprArg);

                    if (peek(",")) {
                        match(",");
                    }
                }

                if (peek(")")) {
                    match(")");
                    return new Ast.Expression.Function(Optional.empty(), firstIdentifier, arguments);
                }
                else {
                    throw new ParseException("Parse Error", tokens.index);
                }
            }
            else {
                return new Ast.Expression.Access(Optional.of(expr), tokens.get(-1).getLiteral());
            }
        }
        else {
            return expr;
        }
    }

    /**
     * Parses the {@code primary-expression} rule. This is the top-level rule
     * for expressions and includes literal values, grouping, variables, and
     * functions. It may be helpful to break these up into other methods but is
     * not strictly necessary.
     */
    public Ast.Expression parsePrimaryExpression() throws ParseException {
        // use access in relation to accessing methods ex. obj.field
        if (peek("NIL")) {
            match("NIL");
            return new Ast.Expression.Literal(null);
        }
        else if (peek("TRUE")) {
            match("TRUE");
            return new Ast.Expression.Literal(Boolean.TRUE);
        }
        else if (peek("FALSE")) {
            match("FALSE");
            return new Ast.Expression.Literal(Boolean.FALSE);
        }
        else if (peek(Token.Type.INTEGER)) {
            match(Token.Type.INTEGER);
            return new Ast.Expression.Literal(new BigInteger(tokens.get(-1).getLiteral()));
        }
        else if (peek(Token.Type.DECIMAL)) {
            match(Token.Type.DECIMAL);
            return new Ast.Expression.Literal(new BigDecimal(tokens.get(-1).getLiteral()));
        }
        else if (peek(Token.Type.CHARACTER)) {
            match(Token.Type.CHARACTER);
            return new Ast.Expression.Literal(tokens.get(-1).getLiteral().charAt(1));
        }
        else if (peek(Token.Type.STRING)) {
            match(Token.Type.STRING);

            if (tokens.get(-1).getLiteral().contains("\\b")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\b", "\b"));
            }
            else if (tokens.get(-1).getLiteral().contains("\\n")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\n", "\n"));
            }
            else if (tokens.get(-1).getLiteral().contains("\\r")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\r", "\r"));
            }
            else if (tokens.get(-1).getLiteral().contains("\\t")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\t", "\t"));
            }
            else {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1));
            }
        }
        else if (peek("(")) {
            match("(");
            Ast.Expression expr = parseExpression();

            if (peek(")")) {
                match(")");
                return new Ast.Expression.Group(expr);
            }
            else {
                throw new ParseException("Parse Error", tokens.index);
            }
        }
        else if (peek(Token.Type.IDENTIFIER)) {
            match(Token.Type.IDENTIFIER);
            String firstIdentifier = tokens.get(-1).getLiteral();

            if (peek("(")) {
                match("(");
                List<Ast.Expression> arguments = new ArrayList<Ast.Expression>();

                while (!peek(")")) {
                    Ast.Expression expr = parseExpression();
                    arguments.add(expr);

                    if (peek(",")) {
                        match(",");
                    }
                }

                if (peek(")")) {
                    match(")");
                    return new Ast.Expression.Function(Optional.empty(), firstIdentifier, arguments);
                }
                else {
                    throw new ParseException("Parse Error", tokens.index);
                }
            }
            else {
                return new Ast.Expression.Access(Optional.empty(), tokens.get(-1).getLiteral());
            }
        }

        return new Ast.Expression.Access(Optional.empty(), tokens.get(-1).getLiteral());
    }

    /**
     * As in the lexer, returns {@code true} if the current sequence of tokens
     * matches the given patterns. Unlike the lexer, the pattern is not a regex;
     * instead it is either a {@link Token.Type}, which matches if the token's
     * type is the same, or a {@link String}, which matches if the token's
     * literal is the same.
     *
     * In other words, {@code Token(IDENTIFIER, "literal")} is matched by both
     * {@code peek(Token.Type.IDENTIFIER)} and {@code peek("literal")}.
     */
    private boolean peek(Object... patterns) {
        for (int i = 0; i < patterns.length; i++) {
            if (!tokens.has(i)) {
                return false;
            }
            else if (patterns[i] instanceof Token.Type) {
                if (patterns[i] != tokens.get(i).getType()) {
                    return false;
                }
            }
            else if (patterns[i] instanceof String) {
                if (!patterns[i].equals(tokens.get(i).getLiteral())) {
                    return false;
                }
            }
            else {
                throw new AssertionError("Invalid pattern object: " + patterns[i].getClass());
            }
        }
        return true;
        // throw new UnsupportedOperationException(); //TODO (in lecture)
    }

    /**
     * As in the lexer, returns {@code true} if {@link #peek(Object...)} is true
     * and advances the token stream.
     */
    private boolean match(Object... patterns) {
        boolean peek = peek(patterns);

        if (peek) {
            for (int i = 0; i < patterns.length; i++) {
                tokens.advance();
            }
        }

        return peek;
        // throw new UnsupportedOperationException(); //TODO (in lecture)
    }

    private static final class TokenStream {

        private final List<Token> tokens;
        private int index = 0;

        private TokenStream(List<Token> tokens) {
            this.tokens = tokens;
        }

        /**
         * Returns true if there is a token at index + offset.
         */
        public boolean has(int offset) {
            return index + offset < tokens.size();
        }

        /**
         * Gets the token at index + offset.
         */
        public Token get(int offset) {
            return tokens.get(index + offset);
        }

        /**
         * Advances to the next token, incrementing the index.
         */
        public void advance() {
            index++;
        }

    }

}
