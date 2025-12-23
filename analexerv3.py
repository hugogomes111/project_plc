import ply.lex as lex

# Lista de nomes de tokens
tokens = (
    # palavras reservadas
    'AND', 'ARRAY', 'BEGIN', 'CASE', 'CONST', 'DIV', 'DO', 'DOWNTO', 'ELSE', 'END',
    'FILE', 'FOR', 'FUNCTION', 'GOTO', 'IF', 'IN', 'LABEL', 'MOD', 'NIL', 'NOT',
    'OF', 'OR', 'PACKED', 'PROCEDURE', 'PROGRAM', 'RECORD', 'REPEAT', 'SET', 'STRING', 'THEN',
    'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE', 'WITH', 'FORWARD', 'INTEGER', 'WRITELN',
    # simbolos especiais de um caractere
    #'PLUS', 'MINUS', 'TIMES', 'SLASH', 'EQUAL', 'LESS', 'GREATER', 'LBRACKET', 'RBRACKET',
    #'PERIOD', 'COMMA', 'COLON', 'SEMICOLON', 'CARET', 'LPAREN', 'RPAREN',
    # simbolos
    'NE', 'LE', 'GE', 'ASSIGN', 'DOTDOT',
    # tipos de dados
    'ID', 'INT', 'BOOLEAN','REAL', 'STR', 'COMMENT', 'BOOL'
)

literals = ( '+', '-', '*', '/', '=', '<', '>', '[', ']', '.', ',', ':', ';', '^', '(', ')' )

reserved = {
    'and': 'AND',
    'array': 'ARRAY',
    'begin': 'BEGIN',
    'boolean': 'BOOLEAN',
    'case': 'CASE',
    'const': 'CONST',
    'div': 'DIV',
    'do': 'DO',
    'downto': 'DOWNTO',
    'else': 'ELSE',
    'end': 'END',
    'file': 'FILE',
    'for': 'FOR',
    'function': 'FUNCTION',
    'goto': 'GOTO',
    'if': 'IF',
    'integer': 'INTEGER',
    'in': 'IN',
    'label': 'LABEL',
    'mod': 'MOD',
    'nil': 'NIL',
    'not': 'NOT',
    'of': 'OF',
    'or': 'OR',
    'packed': 'PACKED',
    'procedure': 'PROCEDURE',
    'program': 'PROGRAM',
    'record': 'RECORD',
    'repeat': 'REPEAT',
    'set': 'SET',
    'string': 'STRING',
    'then': 'THEN',
    'to': 'TO',
    'type': 'TYPE',
    'until': 'UNTIL',
    'var': 'VAR',
    'while': 'WHILE',
    'with': 'WITH',
    'writeln': 'WRITELN',
    'forward': 'FORWARD',
}


def t_COMMENT(t):
    r'\(\*[^*]*\*+(?:[^*)*][^*]*\*+)*\)|\{[^}]*\}'
    t.lexer.lineno += t.value.count('\n')
    pass

def t_STR(t):
    r"'(?:''|[^'])*'"
    t.value = t.value[1:-1].replace("''", "'")
    return t

def t_REAL(t):
    r'\d+\.\d+([eE][+-]?\d+)?|\.\d+([eE][+-]?\d+)?|\d+[eE][+-]?\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_NE(t):
    r'<>'
    return t

def t_LE(t):
    r'<='
    return t

def t_GE(t):
    r'>='
    return t

def t_ASSIGN(t):
    r':='
    return t

def t_DOTDOT(t):
    r'\.\.'
    return t

"""
def t_PLUS(t):
    r'\+'
    return t

def t_MINUS(t):
    r'-'
    return t

def t_TIMES(t):
    r'\*'
    return t

def t_SLASH(t):
    r'/'
    return t

def t_EQUAL(t):
    r'='
    return t

def t_LESS(t):
    r'<'
    return t

def t_GREATER(t):
    r'>'
    return t

def t_LBRACKET(t):
    r'\['
    return t

def t_RBRACKET(t):
    r'\]'
    return t

def t_PERIOD(t):
    r'\.'
    return t

def t_COMMA(t):
    r','
    return t

def t_COLON(t):
    r':'
    return t

def t_SEMICOLON(t):
    r';'
    return t

def t_CARET(t):
    r'\^'
    return t

def t_LPAREN(t):
    r'\('
    return t

def t_RPAREN(t):
    r'\)'
    return t

"""

def t_BOOL(t):
    r'(?:True)|(?:False)'
    t.type = "BOOL"
    return t

def t_IDENTIFIER(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = reserved.get(t.value.lower(), 'ID') 
    return t

t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Caractere ilegal '{t.value[0]}' na linha {t.lineno}")
    t.lexer.skip(1)

lexer = lex.lex()


exemplo1 = """
program Hello;
begin
    writeln('Hello, world!');
end.
"""

exemplo2 = """
{ Este é um comentário }
(* Este é outro comentário *)
(* Comentário com múltiplas
    linhas e caracteres especiais: ( ) { } *)
const
    max = 100;
var
    x, y: integer; (*bla bla bla *)
    a: array[1..10] of real; { bla bla bla }
begin
    x := 10;
    y := 20;
    if x < y then
        writeln('x é menor que y');
    for i := 1 to 10 do
        a[i] := i * 3.14;
end.
"""

exemplo3 = """
(* Isto é um comentário *)
x := 10 * 5;  { Isto é outro comentário }
(**)  { Este é um comentário vazio? }
"""
"""
lexer.input(exemplo3)
for token in lexer:
    print(token)
"""
