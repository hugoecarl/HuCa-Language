import ply.lex as lex
import ply.yacc as yacc
import sys


tokens = [
    'INTEGER',
    'RETURN',
    'WHILE', 
    'IF', 
    'ELSE',
    'OR',
    'AND',
    'LESS',
    'LESS_EQ',
    'GREATER',
    'GREATER_EQ',
    'EQTO',
    'TEXT',
    'VAR',
    'ABRE_COLCH',
    'FECHA_COLCH',
    'ABRE_PAREN',
    'FECHA_PAREN',
    'EQUAL',
    'ABRE_STATE',
    'FECHA_STATE',
    'MAIS',
    'MENOS',
    'VEZES',
    'DIVIDE',
    'END_STATE',
    'SHOW',
    'TRUE',
    'FALSE',
    'BLOCK',
    'NOT',
    'CONCAT',
    'ASP',
    'READ',
    'START_PROG',
    'END_PROG',
    'VIRG'
]

t_ABRE_COLCH = r'\<'
t_FECHA_COLCH = r'\>'
t_ABRE_PAREN = r'\('
t_FECHA_PAREN = r'\)'
t_EQUAL = r'\='
t_ABRE_STATE = r'\=>'
t_FECHA_STATE = r'\<='
t_MAIS = r'\+'
t_MENOS = r'\-'
t_VEZES = r'\*'
t_DIVIDE = r'\/'
t_END_STATE = r'\|'
t_ASP = r'\"'
t_VIRG = r'\,'
reserved = ['@', '?', '?.', 'or', 'and', 'less', 'less_eq', 'greater', 'concat', 'greater_eq', 'eqto', 'show', 'return', 'true', 'false', 'block', 'not', 'read', '#init', '#end']

t_ignore = r' '

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    t.type = 'INTEGER'
    return t 

def t_RESERVED_OR_TEXT(t):
    r'[a-zA-Z_.@?#][a-zA-Z_0-9_.@?#]*'
    if t.value.lower() in reserved:
        if t.value.lower() == '@':
            t.type = 'WHILE'
        elif t.value.lower() == '?':
            t.type = 'IF'
        elif t.value.lower() == '?.':
            t.type = 'ELSE'
        elif t.value.lower() == '#init':
            t.type = 'START_PROG'
        elif t.value.lower() == '#end':
            t.type = 'END_PROG'
        else:
            t.type = t.value.upper()
    elif t.value[0] == '_':
        t.type = 'VAR'
    else:
        t.type = 'TEXT'
    return t


def t_error(t):
    print('Lexical Error')
    t.lexer.skip(1)

s = input('')

lexer = lex.lex()

lexer.input(s)

while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok)

################################################################################
#Importante espaco entre os tokens que contem de texto para funcionar
def p_upper_program(p):
    '''
    upper_statement : START_PROG middle_statement END_PROG
    '''
    p[0] = p[2]
    print(p[0])

def p_function(p):
    '''
    function : BLOCK TEXT ABRE_COLCH declarator upper_statement
    '''
    p[0] = (p[4], p[5])

def p_function_call(p):
    '''
    function_call : TEXT ABRE_COLCH declarator 
    '''
    p[0] = p[3]

def p_declarator(p):
    '''
    declarator : VAR FECHA_COLCH
               | FECHA_COLCH
               | VAR VIRG declarator
               | expression FECHA_COLCH
               | expression VIRG FECHA_COLCH
    '''
    if len(p) == 4:
        p[0] = (p[1], p[3])
    elif len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = None 



def p_upper_statement(p):
    '''
    upper_statement : ABRE_STATE middle_statement FECHA_STATE
    '''
    p[0] = p[2]

def p_middle_statement(p):
    '''
    middle_statement : statement middle_statement
                     | statement
 
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = p[1]

def p_statement(p):
    '''
    statement : IF ABRE_COLCH expression FECHA_COLCH upper_statement
              | IF ABRE_COLCH expression FECHA_COLCH upper_statement ELSE upper_statement
              | WHILE ABRE_COLCH expression FECHA_COLCH upper_statement
              | upper_statement
              | RETURN expression END_STATE
              | END_STATE
              | SHOW expression END_STATE
              | VAR EQUAL expression END_STATE
              | function
              | function_call END_STATE

    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    elif len(p) == 4:
        p[0] = (p[1], p[2])
    elif len(p) == 5:
        p[0] = (p[1], p[2], p[3])
    elif len(p) == 6:
        p[0] = (p[1], p[3], p[5])
    elif len(p) == 8:
        p[0] = (p[1], p[3], p[5], p[7])
    else:
        p[0] = p[1]

def p_expression(p):
    '''
    expression : relexpr
    '''
    p[0] = p[1]

def  p_relexpr(p):
    '''
    relexpr : expr
            | relexpr LESS expr
            | relexpr LESS_EQ expr
            | relexpr GREATER expr
            | relexpr GREATER_EQ expr
            | relexpr EQTO expr
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]

def  p_expr(p):
    '''
    expr : term
         | expr MAIS term
         | expr MENOS term
         | expr OR term
         | expr CONCAT term
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_term(p):
    '''
    term : factor
         | term VEZES factor
         | term DIVIDE factor
         | term AND factor
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]
    

def p_factor(p):
    '''
    factor : INTEGER
           | MAIS factor
           | MENOS factor
           | NOT factor
           | ABRE_PAREN relexpr FECHA_PAREN
           | ASP TEXT ASP
           | TRUE
           | FALSE
           | READ ABRE_COLCH FECHA_COLCH
           | function_call
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    elif len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = p[1]

parser = yacc.yacc()

parser.parse(s)



