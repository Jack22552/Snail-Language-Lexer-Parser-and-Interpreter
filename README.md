# Snail-Language-Lexer-Parser-and-Interpreter
Jack Salinas

p3: I wrote a lexical analyzer, also called a scanner or tokenizer, using a lexical analyzer generator. I described the set of tokens for snail in an appropriate input format and the analyzer generator generates actual code. I then wrote additional code to serialize the tokens for use by later interpreter stages.

p4: I wrote a parser using a parser generator. I described the snail grammar in an appropriate input format and the parser generator generated actual code (in Python). I then wrote additional code to unserialize the tokens produced by the lexer stage and serialized the abstract syntax tree produced by my parser.

p5: This is my written interpreter. Among other things, this involved implementing the operational semantics specification of snail. I tracked enough information to generate legitimate run-time errors (e.g., dispatch on void) and typing errors.
