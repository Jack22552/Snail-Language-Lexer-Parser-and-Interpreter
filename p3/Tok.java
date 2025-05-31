// 3aeee2ffb6bdcec698011572b6bbcaf180807419

public enum Tok {
    AT("at"),
    ASSIGN("assign"),
    UMINUS("uminus"),
    CLASS("class"),
    COLON("colon"),
    COMMA("comma"),
    DIVIDE("divide"),
    DOT("dot"),
    ELSE("else"),
    EQUALS("equals"),
    FALSE("false"),
    IDENT("ident"),
    IF("if"),
    INT("int"),
    ISVOID("isvoid"),
    LBRACE("lbrace"),
    LBRACKET("lbracket"),
    LET("let"),
    LPAREN("lparen"),
    LT("lt"),
    LTE("lte"),
    MINUS("minus"),
    NEW("new"),
    NOT("not"),
    PLUS("plus"),
    RBRACE("rbrace"),
    RBRACKET("rbracket"),
    RPAREN("rparen"),
    SEMI("semi"),
    STRING("string"),
    TIMES("times"),
    FLOAT("float"),
    TRUE("true"),
    WHILE("while");



    private final String name;

    private Tok(String s) {
        name = s;
    }

    @Override
    public String toString() {
        return name;
    }
}