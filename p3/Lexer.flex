// 3aeee2ffb6bdcec698011572b6bbcaf180807419
/*
* JFlex spec for the Snail programming language
*/

// section 1: user code
%%
%class Lexer
%unicode
%line
%column
%type Token

%{
    // Code here gets copied verbatim to the beggining of the lexer class

    // helper function to create tokens
    private Token token(Tok type) {
        return new Token(yyline + 1, yycolumn + 1, type);
    }

    private Token token(Tok type, Object value) {
        return new Token(yyline + 1, yycolumn + 1, type, value);
    }
    private int commentCount;

    // Check if the identifier is a keyword
    private boolean isKeyword(String identifier) {
        switch (identifier.toLowerCase()) {
            case "class":
            case "else":
            case "if":
            case "isvoid":
            case "let":
            case "new":
            case "while":
            case "true":
            case "false":
                return true;
            default:
                return false;
        }
    }

    // Get the Tok type for the keyword
    private Tok getKeywordType(String keyword) {
        switch (keyword.toLowerCase()) {
            case "class":
                return Tok.CLASS;
            case "else":
                return Tok.ELSE;
            case "if":
                return Tok.IF;
            case "isvoid":
                return Tok.ISVOID;
            case "let":
                return Tok.LET;
            case "new":
                return Tok.NEW;
            case "while":
                return Tok.WHILE;
            case "true":
                return Tok.TRUE;
            case "false":
                return Tok.FALSE;
            default:
                throw new IllegalArgumentException("Unexpected keyword: " + keyword);
        }
    }
%}

// section 2: token definitions
Identifier = [_a-zA-Zàáâäçèéêëìíîïñòóôöùúûü][_a-zA-Z0-9àáâäçèéêëìíîïñòóôöùúûü]* 
IntegerLiteral = [0-9]+
FloatLiteral = [0-9]+ "." [0-9]* | "." [0-9]+ | [0-9]+ "." [0-9]+
StringLiteral = \"(\\\"|\\\\|[^\"])*\"
Whitespace = [ \t\n\r]+

%state COMMENT

%%

// section 3: lexical rules
// YYINITIAL is the initial state of the lexer
<YYINITIAL> {
    {Identifier} {
        if (isKeyword(yytext())) {
            return token(getKeywordType(yytext())); // Return keyword token if it's a keyword
        } else {
            return token(Tok.IDENT, yytext()); // Return identifier token otherwise
        }
    }
    
    // Keywords should come after identifiers
    "else"          { return token(Tok.ELSE); }
    "if"            { return token(Tok.IF); }
    "isvoid"        { return token(Tok.ISVOID); }
    "let"           { return token(Tok.LET); }
    "new"           { return token(Tok.NEW); }
    "not"           { return token(Tok.NOT); }
    "true"          { return token(Tok.TRUE); }
    "false"         { return token(Tok.FALSE); }

    "@"              { return token(Tok.AT); }
    "="              { return token(Tok.ASSIGN); }
    ":"              { return token(Tok.COLON); }
    ","              { return token(Tok.COMMA); }
    "/"              { return token(Tok.DIVIDE); }
    "."              { return token(Tok.DOT); }
    "=="             { return token(Tok.EQUALS); }
    "{"              { return token(Tok.LBRACE); }
    "["              { return token(Tok.LBRACKET); }
    "("              { return token(Tok.LPAREN); }
    "<"              { return token(Tok.LT); }
    "<="             { return token(Tok.LTE); }
    "-"              { return token(Tok.MINUS); }
    "+"              { return token(Tok.PLUS); }
    "*"              { return token(Tok.TIMES); }
    "!"              { return token(Tok.NOT); }
    "}"              { return token(Tok.RBRACE); }
    "]"              { return token(Tok.RBRACKET); }
    ")"              { return token(Tok.RPAREN); }
    ";"              { return token(Tok.SEMI); }
    "~"              { return token(Tok.UMINUS); }
    "//"             {
                    yybegin(COMMENT); // Start a single-line comment
                    }
    "/*"            {
                        commentCount++; // Enter multi-line comment
                        yybegin(COMMENT);
                    }
    {IntegerLiteral} {
    String intText = yytext();
    try {
        long value = Long.parseLong(intText); // Use Long to handle larger values
        return token(Tok.INT, value);
    } catch (NumberFormatException e) {
        System.out.printf("ERROR: %d:%d: Lexer: integer literal out of range: '%s'%n", 
                          yyline + 1, yycolumn + 1, intText);
        System.exit(0); // Terminate the program with exit code 0
    }
    }
    {FloatLiteral}  { return token(Tok.FLOAT, Float.parseFloat(yytext()));} 
    {StringLiteral} {
    String str = yytext();
    // Remove the surrounding quotes
    str = str.substring(1, str.length() - 1);

    // Set yycolumn to the position right after the opening quote
    yycolumn += 1; // Move to the position right after the first quote

    Token stringToken = token(Tok.STRING, str); // Create the string token

    // After returning the token, decrement yycolumn by 1
    yycolumn -= 1; // Adjust back for the string token

    return stringToken; // Return the string token
    }
   {Whitespace} { /* Ignore whitespace */ }
}


<COMMENT> {
    "//"            {
                    // Remain in comment state
                    }      
    "*/"            {
                        commentCount--;
                        if (commentCount == 0) {
                            yybegin(YYINITIAL); // Exit comment state
                        }
                    }
    "/*"            {commentCount++; // Enter multi-line comment
                        // Remain in Comment state
                    }
    "*"             { /* Ignore standalone asterisk; could be for formatting */ }
    "\n"             {
                         if (commentCount == 0) {
                            // If in single-line comment, exit state (or handle appropriately)
                            yybegin(YYINITIAL); // Exit comment state
                        } else {
                            // If in a multi-line comment, ignore the newline                           
                        }
                    }
    [^]             { /* Ignore all characters in comments */ }
}
// Catch-all for unrecognized characters
. { 
    System.out.printf("ERROR: %d:%d: Lexer: invalid character: '%s'%n", yyline + 1, yycolumn + 1, yytext());
    System.exit(0); // Terminate the program with exit code 0
}