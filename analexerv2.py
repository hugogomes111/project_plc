import ply.lex as lex

# Tokens atualizados baseados na documentação Pascal completa
tokens = (
    # reserved standard
    'PROGRAM', 'VAR', 'BEGIN', 'END', 'IF', 'THEN', 'ELSE', 'FOR', 'TO', 'DO', 
    'WHILE', 'REPEAT', 'UNTIL', 'CASE', 'OF', 'FUNCTION', 'PROCEDURE', 'CONST',
    'TYPE', 'ARRAY', 'RECORD', 'DIV', 'MOD', 'AND', 'OR', 'NOT', 'IN', 'DOWNTO',
    'GOTO', 'LABEL', 'PACKED', 'SET', 'FILE', 'NIL', 'FORWARD', 'MAIN',
    
    # reserved nonstandard
    'OTHERWISE', 'EXIT', 'NEXT', 'RETURN', 'ASSERT', 'DEFINE', 'PRIVATE',
    'EXTERN', 'EXTERNAL', 'MODULE', 'PUBLIC', 'STATIC', 'UNIV',
    
    # simbolos e identificadores
    'ID', 'INTEGER', 'REAL', 'STRING', 'CHAR', 'BASED_INTEGER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'ASSIGN', 'EQUALS', 'NOTEQUAL',
    'LESS', 'LESSEQUAL', 'GREATER', 'GREATEREQUAL', 'LPAREN', 'RPAREN',
    'LBRACKET', 'RBRACKET', 'COMMA', 'COLON', 'SEMI', 'DOT', 'RANGE', 
    'BITWISE_NOT', 'BITWISE_AND', 'BITWISE_OR', 'HASH', 'PERCENT'
)

# tokens simples
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_ASSIGN = r':='
t_EQUALS = r'='
t_NOTEQUAL = r'<>'
t_LESS = r'<'
t_LESSEQUAL = r'<='
t_GREATER = r'>'
t_GREATEREQUAL = r'>='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COMMA = r','
t_COLON = r':'
t_SEMI = r';'
t_DOT = r'\.'
t_RANGE = r'\.\.'
t_BITWISE_NOT = r'~'
t_BITWISE_AND = r'&'
t_BITWISE_OR = r'\|'
t_HASH = r'\#'
t_PERCENT = r'\%'

reserved = {
    'program': 'PROGRAM',
    'var': 'VAR',
    'begin': 'BEGIN',
    'end': 'END',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'for': 'FOR',
    'to': 'TO',
    'do': 'DO',
    'while': 'WHILE',
    'repeat': 'REPEAT',
    'until': 'UNTIL',
    'case': 'CASE',
    'of': 'OF',
    'function': 'FUNCTION',
    'procedure': 'PROCEDURE',
    'const': 'CONST',
    'type': 'TYPE',
    'array': 'ARRAY',
    'record': 'RECORD',
    'div': 'DIV',
    'mod': 'MOD',
    'and': 'AND',
    'or': 'OR',
    'not': 'NOT',
    'in': 'IN',
    'downto': 'DOWNTO',
    'goto': 'GOTO',
    'label': 'LABEL',
    'packed': 'PACKED',
    'set': 'SET',
    'file': 'FILE',
    'nil': 'NIL',
    'forward': 'FORWARD',
    'main': 'MAIN',
    
    # reserved nonstandard
    'otherwise': 'OTHERWISE',
    'exit': 'EXIT',
    'next': 'NEXT',
    'return': 'RETURN',
    'assert': 'ASSERT',
    'define': 'DEFINE',
    'private': 'PRIVATE',
    'extern': 'EXTERN',
    'external': 'EXTERNAL',
    'module': 'MODULE',
    'public': 'PUBLIC',
    'static': 'STATIC',
    'univ': 'UNIV'
}

def t_ID(t):
    r'[a-zA-Z_$][a-zA-Z0-9_$]*'  # inclui $ nos identificadores
    t.type = reserved.get(t.value.lower(), 'ID')
    return t

def t_BASED_INTEGER(t):
    r'\d+\#[0-9A-Fa-f]+'  # numeros em outras bases: 2#10111, 8#76543
    return t

def t_REAL(t):
    r'\d+\.\d*(e[-+]?\d+)?|\.\d+(e[-+]?\d+)?|\d+e[-+]?\d+'
    t.value = float(t.value)
    return t

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r"'([^'\\]|\\.)*'"
    t.value = t.value[1:-1]  
    return t

def t_CHAR(t):
    r"'([^'\\]|\\.)'"
    t.value = t.value[1:-1]  
    return t

def t_COMMENT(t):
    r'(\{([^}]|\n)*\})|(\(\*([^*]|\*[^)])*\*\))|"([^"]|\n)*"|/\*([^*]|\*[^/])*\*/'
    # Suporte melhorado para comentários aninhados de tipos diferentes
    pass

t_ignore = ' \t\n'

def t_error(t):
    print(f"Caractere ilegal '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

test1 = """
    program Hello;
    var x: integer;
    begin
        x := 10;
        if x > 0 then
            x := x - 1
    end.
    """

test2 = """
    program Test;
    var i, sum: integer;
    begin
        sum := 0;
        for i := 1 to 10 do
            sum := sum + i;
        while sum > 0 do
            sum := sum - 1
    end.
    """

lexer.input(test2)

for token in lexer:
    print(token.type, token.value)