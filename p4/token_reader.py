# 201967cc8d6a85b5befe76602ccbb86bd55df57f
# module to read in SL-LEX formatted tokens

class Token:
    """""
    Token class will represent a snail token
    """""
    # 1st param = self/this always
    def __init__(self, lineno, colno, token_type, lexeme):
        """""
        Constructor for a token
        lineno - line number of the token
        colno - column number of the token
        token_type - type of the token
        lexeme - actual size of the token
        """""
        # upper = uppercase the token type and store it in self.type
        self.type = token_type.upper()
        self.value = lexeme
        self.lineno = lineno
        self.lexpos = colno # we are hacking this

    def __repr__(self):
        tok = f"{self.lineno}:{self.lexpos} {self.type}"
        if self.type in ["IDENT", "STRING", "INT"]:
            tok += f" {self.value}"
        return tok

class TokenReader:
    """""
    Class to read in SL-LEX Token
    """""
    def __init__(self, f):
        """""
        f = file-like object containing the SL-LEX token information
        """""

        # Create a list of all of the lines from f file
        lines = f.readlines()


        # lits of token types
        # any member variable with a name beggining with __ is essentially private
        self.__tokens = []

        # create member variables for line and number positions
        self.lineno = 0
        self.lexpos = 0

        # keep track of the current line in the SL-LEX file
        # start at line 0
        i = 0

        while i < len(lines):
            # read line by line of the file and store each line in the respective variables
            # turn them to ints using int()
            line_no = int(lines[i])
            col_no = int(lines[i+1])
            # remove anyhting at beggining or end of line that is whitespace with strip()
            tok_type = lines[i+2].strip()

            # there's only a forth line if token type is either identifier/list/string
            if tok_type in ["ident", "int", "string"]:
                # remove anyhting at beggining or end of line that is whitespace with strip()
                # FIXME strip() may cause a bug
                lexeme = lines[i+3].strip()
                # increment line number if we do have a 4th line
                i += 1
            else:
                # if theres no 4th line, set lexeme equal to token type
                lexeme = tok_type

            # increment i (number of line read)
            i += 3

            # add token object to list of tokens
            self.__tokens.append(Token(line_no, col_no, tok_type, lexeme))

        # wrap the list of tokens in an iterator
        # iterator just gives position in a collection
        self.token_stream = iter(self.__tokens)


    def token(self):
        """""
        Return the next object or none if there are no more
        """""
        # next returns next value in iterator (position)
        # get next token
        tmp = next(self.token_stream, None)

        # tmp will be None if we have read all tokens
        if tmp is not None:
            # update line info for lexer
            self.lineno = tmp.lineno
            self.lexpos = tmp.lexpos

        # return the token
        return tmp


# main program
if __name__ == "__main__":
    # this will only run when executed from the command line
    import argparse

    # construct a parser to read command line arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("sl_file", help = "The SL-LEX file to load")

    # let me know what arguments wwere passed
    args = parser.parse_args()

    #args.sl_file will be the file name of the SL-LEX file
    # open the file and assign it to f
    with open(args.sl_file) as f:
        reader = TokenReader(f)

        # "":="" assigns reader.token() value to "token" and checks if it is None
        while (token := reader.token()) is not None:
            # print out the token
            print(token)
