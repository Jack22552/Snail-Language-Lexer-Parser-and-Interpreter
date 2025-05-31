// 3aeee2ffb6bdcec698011572b6bbcaf180807419

public class Token {
    private int line, column;
    private Tok tokentype;
    private Object value;

    public Token(int line, int column, Tok tokentype) {
        this.line = line;
        this.column = column;
        this.tokentype = tokentype;
    }

    public Token(int line, int column, Tok tokentype, Object value) {
        this(line, column, tokentype);
        this.value = value;
    }

    @Override
    public String toString() {
        // Create the output in the SL-Lex format
        StringBuilder fmt = new StringBuilder();
        fmt.append(line).append("\n").append(column).append("\n").append(tokentype);
        // Check if there is a value to include
        if (value != null) {
            fmt.append("\n").append(value); // Print value on a new line
        }
        return fmt.toString();
}
}