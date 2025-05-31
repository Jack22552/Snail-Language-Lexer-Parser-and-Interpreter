# Snail-Language-Lexer-Parser-and-Interpreter
Jack Salinas

p3: I wrote a lexical analyzer, also called a scanner or tokenizer, using a lexical analyzer generator. I described the set of tokens for snail in an appropriate input format and the analyzer generator generates actual code. I then wrote additional code to serialize the tokens for use by later interpreter stages.

p4: I wrote a parser using a parser generator. I described the snail grammar in an appropriate input format and the parser generator generated actual code (in Python). I then wrote additional code to unserialize the tokens produced by the lexer stage and serialized the abstract syntax tree produced by my parser.

p5: This is my written interpreter. Among other things, this involved implementing the operational semantics specification of snail. I tracked enough information to generate legitimate run-time errors (e.g., dispatch on void) and typing errors.

Results:

Formal Grammar to Code Translation: Gained hands-on experience converting a language's formal grammar into a working parser using tools like yacc/PLY, reinforcing understanding of context-free grammars.

Lexical and Syntactic Separation: Learned the importance of clearly separating lexical and syntactic responsibilities, including how token types and precedence rules affect grammar design.

Abstract Syntax Trees (ASTs): Understood how to represent program structure via ASTs, and how they serve as the bridge between parsing and interpretation.

Operational Semantics: Implemented key aspects of operational semantics, including variable scope, function calls, and control flow, deepening understanding of how high-level code translates to runtime behavior.

Error Propagation: Gained practical experience implementing safe error propagation and type checking, including handling null references (e.g., void dispatch) gracefully.

Tooling and Modularity: Developed modular code by separating lexical, syntactic, and semantic responsibilities across multiple files and stages, improving maintainability and debuggability.
