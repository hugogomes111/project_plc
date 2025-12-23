import ply.yacc as yacc
from analexerv3 import *

def p_Program(t):
    r'Program : PROGRAM ID ";" Code "."'
    t[0] = t[4]

def p_Code(t):
    r'Code : Declarations BEGIN Blocks END'
    t[0] = "\n".join(t[1] + ["start"] + ["".join(t[3])])

def p_Declarations(t):
    r'Declarations : Vars Declarations'
    t[0] = t[1] + t[2]

def p_Declarations_vazio(t):
    r'Declarations : '
    t[0] = []

def p_Vars(t):
    r'Vars : VAR VarList'
    t[0] = t[2]

def p_VarList(t):
    r'VarList : IDList ":" Type ";" VarList'
    pushcode = []
    for var_name in t[1]:
        parser.var[var_name] = parser.varstotal
        parser.vartype[var_name] = t[3]
        pushcode.append(parser.pushdict[t[3]])
        parser.varstotal += 1
    t[0] = t[5] + pushcode

def p_VarList_vazio(t):
    r'VarList : '
    t[0] = []

def p_IDList(t):
    r'IDList : ID RestoIDs'
    t[0] = [t[1]] + t[2]

def p_RestoIDs(t):
    r'RestoIDs : "," ID RestoIDs'
    t[0] = [t[2]] + t[3]

def p_RestoIDs_vazio(t):
    r'RestoIDs : '
    t[0] = []

def p_Type_integer(t):
    r'Type : INTEGER'
    t[0] = t[1]

def p_Type_boolean(t):
    r'Type : BOOLEAN'
    t[0] = t[1]

def p_Type_string(t):
    r'Type : STRING'
    t[0] = t[1]

def p_Type_real(t):
    r'Type : REAL'
    t[0] = t[1]

def p_Blocks(t):
    r'Blocks : Block Blocks'
    t[0] = t[1] + t[2]

def p_Blocks1(t):
    r'Blocks : '
    t[0] = []

def p_Block_writeln(t):
    r'Block : WRITELN "(" STR ")" ";"'
    t[0] = ['pushs '] + ['"'] + [t[3]] + ['"'] + ['\n'] + ['writes']

def p_block_writeln_var(t):
    r'Block : WRITELN "(" ID ")" ";"'
    t[0] = [f'pushg {parser.var[t[3]]}\n'] + ['writes']

def p_Block_ass(t):
    r'Block : ID ASSIGN Exp ";"'
    print(parser.var)
    t[0] = t[3] + [f"storeg {parser.var[t[1]]}\n"]

def p_Exp_term(t):
    r'Exp : Term'
    t[0] = t[1]

def p_Term(t):
    r'Term : Factor'
    t[0] = t[1]

def p_Factor_integer(t):
    r'Factor : INT'
    t[0] = [f"pushi {t[1]}\n"]

def p_Factor_real(t):
    r'Factor : REAL'
    t[0] = [f"pushf {t[1]}\n"]

def p_Factor_boolean(t):
    r'Factor : BOOLEAN'
    t[0] = [f"pushi {t[1]}\n"]

def p_Factor_string(t):
    r'Factor : STR'
    t[0] = [f'pushs "{t[1]}"\n']

ex1 = """
program Hello;
var
    x, y, z: integer;
    primo: boolean;
    teste: string;
begin
    x := 5;
    teste := 'oi';
    writeln('Hello, world!');
end.
"""

ex2 ="""
program ex2;
var
    a,b,c: string;
    x: integer;
begin
    x := 5;
    b := 'ola mundo';
    writeln(b);
end.
"""

lexer.lineno = 1

lexer.input(ex2)
for token in lexer:
    print(token)

parser = yacc.yacc() 
parser.pushdict = {'integer':'pushi 0','real':'pushf 0','string':'pushs ""', 'boolean':'pushi 0'}
parser.varstotal = 0
parser.var = dict()
parser.vartype = dict()
result = parser.parse(ex2)
print(result)