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
        List<Ast.Field> fieldsArray = new ArrayList<Ast.Field>();
        List<Ast.Method> methodsArray = new ArrayList<Ast.Method>();

        while (peek("LET")) {
            Ast.Field newField = parseField();
            fieldsArray.add(newField);
        }
        while (peek("DEF")) {
            Ast.Method newMethod = parseMethod();
            methodsArray.add(newMethod);
        }

        return new Ast.Source(fieldsArray, methodsArray);
    }

    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {
        if (peek("LET")) {
            match("LET");

            boolean constant = false;
            if (peek("CONST")) {
                match("CONST");
                constant = true;
            }

            if (peek(Token.Type.IDENTIFIER)) {
                match(Token.Type.IDENTIFIER);
                String fieldName = tokens.get(-1).getLiteral();

                if (peek("=")) {
                    match("=");

                    Ast.Expression expr = parseExpression();

                    if (peek(";")) {
                        match(";");
                        return new Ast.Field(fieldName, constant, Optional.of(expr));
                    }
                    else {
                        throw new ParseException("Expected ;", tokens.index);
                    }
                }

                if (peek(";")) {
                    match(";");
                    return new Ast.Field(fieldName, constant, Optional.empty());
                }
                else {
                    throw new ParseException("Expected ;", tokens.index);
                }
            }
            else {
                throw new ParseException("Expected Identifier", tokens.index);
            }
        }
        else {
            throw new ParseException("Expected LET", tokens.index);
        }
    }

    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {
        if (peek("DEF")) {
            match("DEF");

            if (peek(Token.Type.IDENTIFIER)) {
                match(Token.Type.IDENTIFIER);
                String methodName = tokens.get(-1).getLiteral();

                if (peek("(")) {
                    match("(");

                    List<String> parameters = new ArrayList<String>();
                    if (peek(Token.Type.IDENTIFIER)) {
                        match(Token.Type.IDENTIFIER);
                        parameters.add(tokens.get(-1).getLiteral());

                        while(peek(",")) {
                            match(",");

                            if (peek(Token.Type.IDENTIFIER)) {
                                match(Token.Type.IDENTIFIER);
                                parameters.add(tokens.get(-1).getLiteral());
                            }
                            else {
                                throw new ParseException("Expected Identifier", tokens.index);
                            }
                        }
                        if (peek(",")) {
                            throw new ParseException("Trailing ,", tokens.index);
                        }
                    }

                    if (peek(")")) {
                        match(")");

                        if (peek("DO")) {
                            match("DO");

                            List<Ast.Statement> statements = new ArrayList<Ast.Statement>();
                            while (tokens.index < tokens.tokens.size()) {
                                if (peek("END")) {
                                    match("END");
                                    return new Ast.Method(methodName, parameters, statements);
                                }
                                statements.add(parseStatement());
                            }
                            throw new  ParseException("Expected END", tokens.index);
                        }
                        else {
                            throw new ParseException("Expected DO", tokens.index);
                        }
                    }
                    else {
                        throw new ParseException("Expected )", tokens.index);
                    }
                }
                else {
                    throw new ParseException("Expected (", tokens.index);
                }
            }
            else {
                throw new ParseException("Expected Identifier", tokens.index);
            }
        }
        else {
            throw new ParseException("Expected DEF", tokens.index);
        }
    }

    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {
        if (peek("IF")) {
            return parseIfStatement();
        } else if (peek("FOR")) {
            return parseForStatement();
        } else if (peek("WHILE")) {
            return parseWhileStatement();
        } else if (peek("LET")) {
            return parseDeclarationStatement();
        } else if (peek("RETURN")){
            return parseReturnStatement();
        } else if (peek(Token.Type.IDENTIFIER)) {
            Ast.Expression expr = parseExpression();
            if (peek("=")) {
                if (!match("=")) {
                    throw new ParseException("Expected =", tokens.index);
                }
                if (!peek(Token.Type.IDENTIFIER)) {
                    throw new ParseException("Expected IDENTIFIER", tokens.index);
                }
                Ast.Expression expr2 = parseExpression();
                if (!match(";")) {
                    throw new ParseException("Expected ;", tokens.index);
                }
                return new Ast.Statement.Assignment(expr, expr2);
            } else if (peek(Token.Type.IDENTIFIER)) {
            }
            if (!match(";")) {
                throw new ParseException("Expected ;", tokens.index);
            }
            return new Ast.Statement.Expression(expr);
        }
        return new Ast.Statement.Expression(parseExpression());
    }

    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {
        if (!match("LET")) {
            throw new ParseException("Expected LET", tokens.index);
        }
        if (!peek(Token.Type.IDENTIFIER)) {
            throw new ParseException("Expected IDENTIFIER", tokens.index);
        }

        match(Token.Type.IDENTIFIER);
        String identifierStr = tokens.get(-1).getLiteral();
        if (!peek("=")) {
            return new Ast.Statement.Declaration(identifierStr, Optional.empty());
        }
        match("=");

        Ast.Expression expr = parseExpression();
        return new Ast.Statement.Declaration(identifierStr, Optional.of(expr));
    }

    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
        if (!match("IF")) {
            throw new ParseException("Expected IF", tokens.index);
        }

        Ast.Expression condition = parseExpression();

        if (!match("DO")) {
            throw new ParseException("Expected DO", tokens.index);
        }


        List<Ast.Statement> thenStatements = new ArrayList<>();
        while (!peek("ELSE") && !peek("END")) {
            thenStatements.add(parseStatement());
        }
        if (!match("ELSE")) {
            throw new ParseException("Expected ELSE", tokens.index);
        }


        List<Ast.Statement> elseStatements = new ArrayList<>();
        while (!peek("END")) {
            elseStatements.add(parseStatement());
        }
        if (!match("END")) {
            throw new ParseException("Expected END", tokens.index);
        }

        return new Ast.Statement.If(condition, thenStatements, elseStatements);
    }

    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
        if (!match("FOR")) {
            throw new ParseException("Expected FOR", tokens.index);
        }
        if (!match("(")) {
            throw new ParseException("Expected (", tokens.index);
        }


        Ast.Statement.Declaration initialization = null;
        if (peek(Token.Type.IDENTIFIER)) {
            match(Token.Type.IDENTIFIER);
            String identifierStr = tokens.get(-1).getLiteral();
            if (!peek("=")) {
                throw new ParseException("Expected =", tokens.index);
            }
            match("=");
            Ast.Expression expr = parseExpression();
            initialization = new Ast.Statement.Declaration(identifierStr, Optional.of(expr));
        }


        if (!peek(";")) {
            throw new ParseException("Expected ;", tokens.index);
        }
        match(";");


        Ast.Expression condition = parseExpression();
        if (!match(";")) {
            throw new ParseException("Expected ;", tokens.index);
        }


        Ast.Statement.Expression increment = null;
        if (peek(Token.Type.IDENTIFIER)) {
            match(Token.Type.IDENTIFIER);
            String identifierStr = tokens.get(-1).getLiteral();
            if (!peek("=")) {
                throw new ParseException("Expected =", tokens.index);
            }
            match("=");

            Ast.Expression expr = parseExpression();
            Ast.Expression.Access access = new Ast.Expression.Access(Optional.of(expr), identifierStr);
            increment = new Ast.Statement.Expression(access);
        }
        if (!match(")")) {
            throw new ParseException("Expected )", tokens.index);
        }

        List<Ast.Statement> statements = new ArrayList<>();
        while (!peek("END")) {
            statements.add((parseStatement()));
        }
        if (!match("END")) {
            throw new ParseException("Expected END", tokens.index);
        }

        return new Ast.Statement.For(initialization, condition, increment, statements);
    }

    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
        if (!match("WHILE")) {
            throw new ParseException("Expected WHILE", tokens.index);
        }

        Ast.Expression condition = parseExpression();
        if (!match("DO")) {
            throw new ParseException("Expected DO", tokens.index);
        }

        List<Ast.Statement> statements = new ArrayList<>();
        while(!peek("END")) {
            statements.add(parseStatement());
        }
        if (!match("END")) {
            throw new ParseException("Expected END", tokens.index);
        }
        return new Ast.Statement.While(condition, statements);
    }

    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {
        if (!match("RETURN")) {
            throw new ParseException("Expected RETURN", tokens.index);
        }

        Ast.Expression value = parseExpression();
        return new Ast.Statement.Return(value);
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
            Ast.Expression exprRight = parseLogicalExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("&&")) {
            match("&&");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseLogicalExpression();
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
            Ast.Expression exprRight = parseEqualityExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("<=")) {
            match("<=");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseEqualityExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek(">")) {
            match(">");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseEqualityExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek(">=")) {
            match(">=");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseEqualityExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("==")) {
            match("==");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseEqualityExpression();
            expr = new Ast.Expression.Binary(prev, expr, exprRight);
        }
        else if (peek("!=")) {
            match("!=");
            String prev = tokens.get(-1).getLiteral();
            Ast.Expression exprRight = parseEqualityExpression();
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

        if (peek(".")) {
            match(".");
            if (peek(Token.Type.IDENTIFIER)) {
                match(Token.Type.IDENTIFIER);
                String firstIdentifier = tokens.get(-1).getLiteral();

                if (peek("(")) {
                    match("(");
                    List<Ast.Expression> arguments = new ArrayList<Ast.Expression>();

                    if (!peek(")")) {
                        Ast.Expression exprArg = parseExpression();
                        arguments.add(exprArg);

                        while (!peek(")")) {
                            if (peek(",")) {
                                match(",");
                                exprArg = parseExpression();
                                arguments.add(exprArg);
                            }
                            else {
                                throw new ParseException("Expected ,", tokens.index);
                            }
                        }
                    }

                    if (peek(")")) {
                        match(")");
                        return new Ast.Expression.Function(Optional.of(expr), firstIdentifier, arguments);
                    }
                    else {
                        throw new ParseException("Expected )", tokens.index);
                    }
                }
                else {
                    return new Ast.Expression.Access(Optional.of(expr), tokens.get(-1).getLiteral());
                }
            }
            else {
                throw new ParseException("Expected Identifier", tokens.index);
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
            return new Ast.Expression.Literal(true);
        }
        else if (peek("FALSE")) {
            match("FALSE");
            return new Ast.Expression.Literal(false);
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
            if (tokens.get(-1).getLiteral().contains("\\b")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\b", "\b").charAt(0));
            }
            else if (tokens.get(-1).getLiteral().contains("\\n")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\n", "\n").charAt(0));
            }
            else if (tokens.get(-1).getLiteral().contains("\\r")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\r", "\r").charAt(0));
            }
            else if (tokens.get(-1).getLiteral().contains("\\t")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\t", "\t").charAt(0));
            }
            else if (tokens.get(-1).getLiteral().contains("\\'")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\'", "\'").charAt(0));
            }
            else if (tokens.get(-1).getLiteral().contains("\\\"")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\\"", "\"").charAt(0));
            }
            else if (tokens.get(-1).getLiteral().contains("\\\\")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\\\", "\\").charAt(0));
            }
            else {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().charAt(1));
            }
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
            else if (tokens.get(-1).getLiteral().contains("\\'")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\'", "'"));
            }
            else if (tokens.get(-1).getLiteral().contains("\\\\")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\\\", "\\"));
            }
            else if (tokens.get(-1).getLiteral().contains("\\\"")) {
                return new Ast.Expression.Literal(tokens.get(-1).getLiteral().substring(1, tokens.get(-1).getLiteral().length() - 1).replace("\\\"", "\""));
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
                throw new ParseException("Expected )", tokens.index);
            }
        }
        else if (peek(Token.Type.IDENTIFIER)) {
            match(Token.Type.IDENTIFIER);
            String firstIdentifier = tokens.get(-1).getLiteral();

            if (peek("(")) {
                match("(");
                List<Ast.Expression> arguments = new ArrayList<Ast.Expression>();

                if (!peek(")")) {
                    Ast.Expression expr = parseExpression();
                    arguments.add(expr);

                    while(!peek(")")) {
                        if (peek(",")) {
                            match(",");
                            expr = parseExpression();
                            arguments.add(expr);
                        }
                        else {
                            throw new ParseException("Expected ,", tokens.index);
                        }
                    }
                }

                if (peek(")")) {
                    match(")");
                    return new Ast.Expression.Function(Optional.empty(), firstIdentifier, arguments);
                }
                else {
                    throw new ParseException("Expected )", tokens.index);
                }
            }
            else {
                return new Ast.Expression.Access(Optional.empty(), tokens.get(-1).getLiteral());
            }
        }
        else {
            throw new ParseException("Invalid primary expression", tokens.index);
        }
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
