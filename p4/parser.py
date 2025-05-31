# 201967cc8d6a85b5befe76602ccbb86bd55df57f
# Define precedence rules here
precedence = (
    ('left', 'DOT',),  # Highest precedence for method calls and array access
    ('right', 'TILDE', 'NOT'),  # Unary operators
    ('left', 'TIMES', 'DIVIDE'),  # Multiplication and division
    ('left', 'PLUS', 'MINUS'),  # Addition and subtraction
    ('left', 'LT', 'LTEQ', 'EQUALS'),  # Comparison operators
    ('right', 'ASSIGN'),  # Lowest precedence for assignment
)

tokens = (
    'CLASS', 'IDENT', 'COLON', 'LBRACE', 'LET', 'RBRACE', 'SEMI',
    'ASSIGN', 'LPAREN', 'RPAREN', 'COMMA', 'DOT', 'IF', 'ELSE', 'WHILE',
    'NEW', 'ISVOID', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EQUALS', 'LT',
    'LTEQ', 'NOT', 'TILDE', 'INT', 'STRING', 'TRUE', 'FALSE',
    'LBRACKET', 'RBRACKET'
)

def p_program(p):
    """
    program ::= class_list
    """
    p[0] = p[1]

def p_class_list(p):
    """
    class_list ::= class class_list
               |   class
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else: 
        p[0] = [p[1]] + p[2]

# class ::= 'class' 'ident' (':' 'ident')? '{' (feature)* '}' ';'
def p_class(p):
    """
    class ::= CLASS IDENT LBRACE feature_list RBRACE SEMI
            | CLASS IDENT COLON IDENT LBRACE feature_list RBRACE SEMI
    """
    p[0] = {
        "class_name": p[2],
        "members": [],
        "methods": [],
    }

    if len(p) == 7:
        features = p[4]
    else:
        p[0]["inherits"] = p[4]
        features = p[6]

    # Separate members and methods based on type
    p[0]["members"] = [f for f in features if f["type"] == "member"]
    p[0]["methods"] = [f for f in features if f["type"] == "method"]

def p_feature_list(p):
    """
    feature_list ::= empty_list
                | nonempty_feature_list
    """
    p[0] = p[1]

def p_nonempty_feature_list(p):
    """
    nonempty_feature_list ::= feature nonempty_feature_list
                            | feature
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else: 
        p[0] = [p[1]] + p[2]

def p_empty_list(p):
    """
    empty_list ::= 
    """
    p[0] = []

# uninitialized member variable
def p_feature_member_noinit(p):
    """
    feature ::= LET IDENT SEMI
    """
    p[0] = {
        "name": {"value": p[2], "line": p.lineno(2), "col": p.lexpos(2)},
        "type": "member",
        "init": None
    }

# initialized member variable
def p_feature_member_init(p):
    """
    feature ::= LET IDENT ASSIGN expr SEMI
    """
    p[0] = {
        "name": {"value": p[2], "line": p.lineno(2), "col": p.lexpos(2)},
        "type": "member",
        "init": p[4]
    }

# method feature: IDENT (param_list) block SEMI
def p_feature_method(p):
    """
    feature ::= IDENT LPAREN param_list RPAREN block SEMI
    """
    p[0] = {
        "name": {"value": p[1], "line": p.lineno(1), "col": p.lexpos(1)},
        "type": "method",
        "parameters": [{"value": param, "line": p.lineno(i), "col": p.lexpos(i)} for i, param in enumerate(p[3], start=3)],
        "body": p[5]  # body should be an expression (block)
    }

# parameter list: IDENT | IDENT ',' param_list
def p_param_list(p):
    """
    param_list ::= IDENT
                | IDENT COMMA param_list
                | empty_list
    """
    if len(p) == 2:  # single parameter
        p[0] = [p[1]]
    elif len(p) == 4:  # multiple parameters
        p[0] = [p[1]] + p[3]
    else:
        p[0] = []  # empty parameter list for cases like foo()

# expression rules

# Assignment: IDENT '=' expr
def p_expr_assign(p):
    """
    expr ::= IDENT ASSIGN expr SEMI
    """
    p[0] = {
        "type": "assign",
        "lhs": {"value": p[1], "line": p.lineno(1), "col": p.lexpos(1)},
        "rhs": p[3]  # The right-hand side expression
    }

# Array access and assignment: expr '[' expr ']' '=' expr
def p_expr_array_access_assign(p):
    """
    expr ::= expr LBRACKET expr RBRACKET ASSIGN expr
    """
    p[0] = {
        "type": "array-assign",
        "lhs": p[1],  # The array being accessed
        "index": p[3],  # The index expression
        "rhs": p[5]  # The value being assigned
    }

# Method call: expr '.' IDENT LPAREN expr_list RPAREN
def p_expr_method_call(p):
    """
    expr ::= expr DOT IDENT LPAREN expr_list RPAREN
    """
    p[0] = {"type": "method_call", "object": p[1], "method": p[3], "args": p[5]}

# Function call: IDENT LPAREN expr_list RPAREN
def p_expr_function_call(p):
    """
    expr ::= IDENT LPAREN expr_list RPAREN SEMI
    """
    p[0] = {"type": "function_call", "name": p[1], "args": p[3]}

# If expression: 'if' '(' expr ')' block 'else' block
def p_expr_if(p):
    """
    expr ::= IF LPAREN expr RPAREN block ELSE block SEMI
    """
    p[0] = {
        "type": "if",
        "guard": p[3],
        "then": p[5],
        "else": p[7],
        "line": p.lineno(1),
        "col": p.lexpos(1)
    }

# While expression: 'while' '(' expr ')' block
def p_expr_while(p):
    """
    expr ::= WHILE LPAREN expr RPAREN block SEMI
    """
    p[0] = {
        "type": "while",
        "guard": p[3],
        "body": p[5],
        "line": p.lineno(1),
        "col": p.lexpos(1)
    }

# Block expression: '{' expr_list '}'
def p_expr_block(p):
    """
    expr ::= LBRACE expr_list RBRACE SEMI
    """
    p[0] = {"type": "block", "expressions": p[2]}

# Let expression: 'let' IDENT '=' expr
def p_expr_let(p):
    """
    expr ::= LET IDENT ASSIGN expr SEMI
    """
    p[0] = {"type": "let", "name": p[2], "value": p[4]}

# New object expression: 'new' IDENT
def p_expr_new_object(p):
    """
    expr ::= NEW IDENT
    """
    p[0] = {"type": "new_object", "class": p[2]}

# New array expression: 'new' '[' expr ']' IDENT
def p_expr_new_array(p):
    """
    expr ::= NEW LBRACKET expr RBRACKET IDENT
    """
    p[0] = {"type": "new_array", "size": p[3], "class": p[5]}

# Isvoid expression: 'isvoid' '(' expr ')'
def p_expr_isvoid(p):
    """
    expr ::= ISVOID LPAREN expr RPAREN
    """
    p[0] = {"type": "isvoid", "expression": p[3]}

# Arithmetic and comparison operations: +, -, *, /, ==, <, <=, !
def p_expr_arithmetic(p):
    """
    expr ::= expr PLUS expr
           | expr MINUS expr
           | expr TIMES expr
           | expr DIVIDE expr
           | expr EQUALS expr
           | expr LT expr
           | expr LTEQ expr
           | NOT expr
           | TILDE expr
    """
    # For binary operations
    if p[2] in ['+', '-', '*', '/', '==', '<', '<=']:
        # Handle EQUALS (==) as equality comparison
        if p[2] == 'EQUALS':
            p[0] = {
                "type": "equals",  # Explicitly label this as "equals"
                "lhs": p[1],
                "rhs": p[3],
                "line": p.lineno(1),
                "col": p.lexpos(1)
            }
        else:
            p[0] = {
                "type": p[2],  # Keep the operation type as is (e.g., PLUS, MINUS)
                "lhs": p[1],
                "rhs": p[3],
                "line": p.lineno(1),
                "col": p.lexpos(1)
            }
    # For unary operations
    elif p[1] == 'NOT':
        p[0] = {
            "type": "not",
            "body": p[2],
            "line": p.lineno(1),
            "col": p.lexpos(1)
        }
    elif p[1] == 'TILDE':
        p[0] = {
            "type": "negate",  
            "body": p[2],
            "line": p.lineno(1),
            "col": p.lexpos(1)
        }
# Parentheses expression: '(' expr ')'
def p_expr_paren(p):
    """
    expr ::= LPAREN expr RPAREN
    """
    p[0] = p[2]  # just return the inner expression

# Literal values (identifiers, int, string, true, false)
def p_expr_literal(p):
    """
    expr ::= IDENT
           | INT
           | STRING
           | TRUE
           | FALSE
    """
    if p[1] == 'TRUE' or p[1] == 'FALSE':
        p[0] = {
            "type": "bool",
            "value": p[1] == 'TRUE',
            "line": p.lineno(1),
            "col": p.lexpos(1)
        }
    elif isinstance(p[1], int):  # assuming INT is parsed as an integer
        p[0] = {
            "type": "number",
            "line": p.lineno(1),
            "col": p.lexpos(1),
            "value": p[1]
        }
    elif isinstance(p[1], str):  # assuming STRING is parsed as a string
        p[0] = {
            "type": "string",
            "line": p.lineno(1),
            "col": p.lexpos(1),
            "value": p[1]
        }
    else:  # For IDENT
        p[0] = {
            "type": "identifier",
            "value": p[1],
            "line": p.lineno(1),
            "col": p.lexpos(1)
        }

# expr_list rule (needed for blocks, methods, etc.)
def p_expr_list(p):
    """
    expr_list ::= expr 
               | expr expr_list
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

# Block rule
def p_block(p):
    """
    block ::= LBRACE expr_list RBRACE
    """
    p[0] = {"type": "block", "expressions": p[2]}

def p_error(p):
    if p is None:
        print("Unexpected EOF")
    else:
        print(f"ERROR: {p.lineno}:{p.lexpos}: Parser")
    exit(0)

# main program
if __name__ == "__main__":
    import sys
    # import "yet another compilor compilor" or YACC and save it with the name yacc
    import ply.yacc as yacc
    import json


    from token_reader import TokenReader

    # create the parser
    # this reads all of the p_ functions and compiles them into a grammar and parser object
    # that we can use to process a token stream
    parser = yacc.yacc()

    # create a lexer object
    # stdin (standard in) obtains everything typed on the console
    lexer = TokenReader(sys.stdin)

    # parse the tokens and produce an AST
    # tracking=True turns on line number tracking for us
    program = parser.parse(lexer=lexer, tracking=True)

    print(json.dumps(program, indent=2))