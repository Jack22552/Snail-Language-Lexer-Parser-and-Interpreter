// 3aeee2ffb6bdcec698011572b6bbcaf180807419
import java.io.InputStreamReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        Lexer lexer = new Lexer(new InputStreamReader(System.in));

        try {
            // Read tokens until the end of input
            while (true) {
                Token token = lexer.yylex();
                if (token == null) {
                    break; // End of input
                } else {
                    System.out.println(token); // Print the token
                }
            }
        } catch (IOException | Error e) {
            System.err.println(e.getLocalizedMessage());
        }
    }
}
