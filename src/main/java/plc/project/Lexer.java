package plc.project;

import java.util.ArrayList;
import java.util.List;

/**
 * The lexer works through three main functions:
 *
 *  - {@link #lex()}, which repeatedly calls lexToken() and skips whitespace
 *  - {@link #lexToken()}, which lexes the next token
 *  - {@link CharStream}, which manages the state of the lexer and literals
 *
 * If the lexer fails to parse something (such as an unterminated string) you
 * should throw a {@link ParseException} with an index at the invalid character.
 *
 * The {@link #peek(String...)} and {@link #match(String...)} functions are
 * helpers you need to use, they will make the implementation easier.
 */
public final class Lexer {

    private final CharStream chars;

    public Lexer(String input) {
        chars = new CharStream(input);
    }

    /**
     * Repeatedly lexes the input using {@link #lexToken()}, also skipping over
     * whitespace where appropriate.
     */
    public List<Token> lex() {
        List<Token> tokenList = new ArrayList<Token>();
        while (chars.index < chars.input.length()) {
            tokenList.add(lexToken());
        }
        return tokenList;
        // throw new UnsupportedOperationException(); //TODO
    }

    /**
     * This method determines the type of the next token, delegating to the
     * appropriate lex method. As such, it is best for this method to not change
     * the state of the char stream (thus, use peek not match).
     *
     * The next character should start a valid token since whitespace is handled
     * by {@link #lex()}
     */
    public Token lexToken() {
        if (peek("[A-Za-z_]")) {
            return lexIdentifier();
        }
        else if (peek("([-]|[0-9])")) {
            return lexNumber();
        }
        else if (peek("'")) {
            return lexCharacter();
        }
        // add calls to other lex methods here // TODO
        else {
            throw new ParseException("", chars.index);
        }
    }

    public Token lexIdentifier() {
        String identifierRegex = "[A-Za-z0-9_-]*";

        while (peek(identifierRegex)) {
            match(identifierRegex);
        }

        return chars.emit(Token.Type.IDENTIFIER);
    }

    public Token lexNumber() {
        String validInteger = "[0-9]";

        // check if negative is correctly formatted
        if (peek("[-]")) {
            if (peek("[-]", "[0]", "[\\.]", "[0-9]")) {
                match("[-]", "[0]", "[\\.]", "[0-9]");

                while(peek(validInteger)) {
                    match(validInteger);
                }

                return chars.emit(Token.Type.DECIMAL);
            }
            else if (peek("[-]", "[1-9]")) {
                match("[-]", "[1-9]");
            }
            else {
                throw new ParseException("", chars.index);
            }
        }
        // check if zero
        if (peek("[0]")) {
            if (peek("[0]", "[\\.]", "[0-9]")) {
                match("[0]", "[\\.]", "[0-9]");

                while(peek(validInteger)) {
                    match(validInteger);
                }

                return chars.emit(Token.Type.DECIMAL);
            }
            else {
                match("[0]");
                return chars.emit(Token.Type.INTEGER);
            }
        }

        // iterate through valid numbers
        while(peek(validInteger)) {
            match(validInteger);
        }

        // check for valid decimal
        if (peek("[\\.]", "[0-9]")) {
            match("[\\.]", "[0-9]");

            while(peek(validInteger)) {
                match(validInteger);
            }

            return chars.emit(Token.Type.DECIMAL);
        }

        return chars.emit(Token.Type.INTEGER);
    }

    public Token lexCharacter() {
        if (peek("'", "[^\\\\']", "'")) {
            match("'", "[^\\\\']", "'");
            return chars.emit(Token.Type.CHARACTER);
        }
        else if (peek("'", "[\\\\]", "[bnrt'\"\\\\]","'")) {
            match("'", "[\\\\]", "[bnrt'\"\\\\]","'");
            return chars.emit(Token.Type.CHARACTER);
        }
        else {
            throw new ParseException("", chars.index);
        }
    }

    public Token lexString() {
        throw new UnsupportedOperationException(); //TODO
    }

    public void lexEscape() {
        throw new UnsupportedOperationException(); //TODO
    }

    public Token lexOperator() {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Returns true if the next sequence of characters match the given patterns,
     * which should be a regex. For example, {@code peek("a", "b", "c")} would
     * return true if the next characters are {@code 'a', 'b', 'c'}.
     */
    public boolean peek(String... patterns) {
        for (int i = 0; i < patterns.length; i++) {
            if (!chars.has(i) || !String.valueOf(chars.get(i)).matches(patterns[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns true in the same way as {@link #peek(String...)}, but also
     * advances the character stream past all matched characters if peek returns
     * true. Hint - it's easiest to have this method simply call peek.
     */
    public boolean match(String... patterns) {
        boolean peek = peek(patterns);
        if (peek) {
            for (int i = 0; i < patterns.length; i++) {
                chars.advance();
            }
        }
        return peek;
    }

    /**
     * A helper class maintaining the input string, current index of the char
     * stream, and the current length of the token being matched.
     *
     * You should rely on peek/match for state management in nearly all cases.
     * The only field you need to access is {@link #index} for any {@link
     * ParseException} which is thrown.
     */
    public static final class CharStream {

        private final String input;
        private int index = 0;
        private int length = 0;

        public CharStream(String input) {
            this.input = input;
        }

        public boolean has(int offset) {
            return index + offset < input.length();
        }

        public char get(int offset) {
            return input.charAt(index + offset);
        }

        public void advance() {
            index++;
            length++;
        }

        public void skip() {
            length = 0;
        }

        public Token emit(Token.Type type) {
            int start = index - length;
            skip();
            return new Token(type, input.substring(start, index), start);
        }

    }

}
