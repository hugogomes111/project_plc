import ply.yacc as yacc
from analexerv4 import *

"""
VERSÃO SIMPLIFICADA - SEM GESTÃO DE ESCOPOS
- Mais fácil de entender
- Funciona para exemplos 1-4
- NÃO funciona bem com funções (exemplo 5)
"""

def p_Program(t):
    r'Program : PROGRAM ID ";" Code "."'
    t[0] = t[4] + ['stop\n']

def p_Code(t):
    r'Code : Declarations BEGIN Blocks END'
    t[0] = t[1] + ["start\n"] + t[3]

# Declarations
def p_Declarations_vars(t):
    r'Declarations : Vars Declarations'
    t[0] = t[1] + t[2]

def p_Declarations_consts(t):
    r'Declarations : Consts Declarations'
    t[0] = t[1] + t[2]

def p_Declarations_functions(t):
    r'Declarations : Functions Declarations'
    t[0] = t[1] + t[2]

def p_Declarations_procedures(t):
    r'Declarations : Procedures Declarations'
    t[0] = t[1] + t[2]

def p_Declarations_vazio(t):
    r'Declarations : '
    t[0] = []

# CONSTANTES - versão simples (substitui valores)
def p_Consts(t):
    r'Consts : CONST ConstDefs'
    t[0] = []  # Constantes não geram código

def p_ConstDefs(t):
    r'ConstDefs : ID "=" ConstValue ";" ConstDefs'
    # Guarda o valor da constante
    parser.constants[t[1]] = t[3]
    t[0] = []

def p_ConstDefs_vazio(t):
    r'ConstDefs : '
    t[0] = []

def p_ConstValue_int(t):
    r'ConstValue : INT'
    t[0] = ('int', t[1])

def p_ConstValue_real(t):
    r'ConstValue : REAL'
    t[0] = ('real', t[1])

def p_ConstValue_str(t):
    r'ConstValue : STR'
    t[0] = ('str', t[1])

def p_ConstValue_bool_true(t):
    r'ConstValue : TRUE'
    t[0] = ('bool', True)

def p_ConstValue_bool_false(t):
    r'ConstValue : FALSE'
    t[0] = ('bool', False)

# VARIÁVEIS
def p_Vars(t):
    r'Vars : VAR VarList'
    t[0] = t[2]

def p_VarList(t):
    r'VarList : IDList ":" Type ";" VarList'
    pushcode = []
    for var_name in t[1]:
        var_type = t[3]
        
        # Arrays
        if isinstance(var_type, tuple) and var_type[0] == 'array':
            array_size = var_type[2] - var_type[1] + 1
            element_type = var_type[3]
            parser.var[var_name] = parser.varcount
            parser.vartype[var_name] = var_type
            for _ in range(array_size):
                pushcode.append(parser.pushdict.get(element_type, 'pushi 0\n'))
                parser.varcount += 1
        else:
            # Variável simples
            parser.var[var_name] = parser.varcount
            parser.vartype[var_name] = var_type
            pushcode.append(parser.pushdict.get(var_type, 'pushi 0\n'))
            parser.varcount += 1
    
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

# TIPOS
def p_Type_integer(t):
    r'Type : INTEGER'
    t[0] = 'integer'

def p_Type_boolean(t):
    r'Type : BOOLEAN'
    t[0] = 'boolean'

def p_Type_string(t):
    r'Type : STRING'
    t[0] = 'string'

def p_Type_real(t):
    r'Type : REALTYPE'
    t[0] = 'real'

def p_Type_char(t):
    r'Type : CHAR'
    t[0] = 'char'

def p_Type_array(t):
    r'Type : ARRAY "[" INT DOTDOT INT "]" OF Type'
    t[0] = ('array', t[3], t[5], t[8])

# FUNÇÕES - versão simplificada (não suporta variáveis locais bem)
def p_Functions(t):
    r'Functions : FUNCTION ID "(" Parameters ")" ":" Type ";" FunctionBody'
    func_name = t[2]
    func_label = f'func_{func_name}'
    
    parser.functions[func_name] = func_label
    
    # Gera código
    code = [f'jump end_{func_label}\n']
    code.append(f'{func_label}:\n')
    code.extend(t[9])
    code.append('return\n')
    code.append(f'end_{func_label}:\n')
    
    t[0] = code

def p_Functions_no_params(t):
    r'Functions : FUNCTION ID ":" Type ";" FunctionBody'
    func_name = t[2]
    func_label = f'func_{func_name}'
    
    parser.functions[func_name] = func_label
    
    code = [f'jump end_{func_label}\n']
    code.append(f'{func_label}:\n')
    code.extend(t[6])
    code.append('return\n')
    code.append(f'end_{func_label}:\n')
    
    t[0] = code

def p_FunctionBody(t):
    r'FunctionBody : Declarations BEGIN Blocks END ";"'
    t[0] = t[1] + t[3]

# PROCEDURES
def p_Procedures(t):
    r'Procedures : PROCEDURE ID "(" Parameters ")" ";" ProcedureBody'
    proc_name = t[2]
    proc_label = f'proc_{proc_name}'
    
    parser.functions[proc_name] = proc_label
    
    code = [f'jump end_{proc_label}\n']
    code.append(f'{proc_label}:\n')
    code.extend(t[7])
    code.append('return\n')
    code.append(f'end_{proc_label}:\n')
    
    t[0] = code

def p_Procedures_no_params(t):
    r'Procedures : PROCEDURE ID ";" ProcedureBody'
    proc_name = t[2]
    proc_label = f'proc_{proc_name}'
    
    parser.functions[proc_name] = proc_label
    
    code = [f'jump end_{proc_label}\n']
    code.append(f'{proc_label}:\n')
    code.extend(t[4])
    code.append('return\n')
    code.append(f'end_{proc_label}:\n')
    
    t[0] = code

def p_ProcedureBody(t):
    r'ProcedureBody : Declarations BEGIN Blocks END ";"'
    t[0] = t[1] + t[3]

def p_Parameters(t):
    r'Parameters : IDList ":" Type MoreParameters'
    t[0] = []

def p_Parameters_vazio(t):
    r'Parameters : '
    t[0] = []

def p_MoreParameters(t):
    r'MoreParameters : ";" IDList ":" Type MoreParameters'
    t[0] = []

def p_MoreParameters_vazio(t):
    r'MoreParameters : '
    t[0] = []

# BLOCOS
def p_Blocks(t):
    r'Blocks : Block Blocks'
    t[0] = t[1] + t[2]

def p_Blocks_vazio(t):
    r'Blocks : '
    t[0] = []

# I/O - WRITELN
def p_Block_writeln(t):
    r'Block : WRITELN "(" WriteList ")" ";"'
    t[0] = t[3] + ['pushs "\\n"\nwrites\n']

def p_Block_write(t):
    r'Block : WRITE "(" WriteList ")" ";"'
    t[0] = t[3]

def p_WriteList(t):
    r'WriteList : WriteItem MoreWriteItems'
    t[0] = t[1] + t[2]

def p_MoreWriteItems(t):
    r'MoreWriteItems : "," WriteItem MoreWriteItems'
    t[0] = t[2] + t[3]

def p_MoreWriteItems_vazio(t):
    r'MoreWriteItems : '
    t[0] = []

def p_WriteItem_string(t):
    r'WriteItem : STR'
    t[0] = [f'pushs "{t[1]}"\nwrites\n']

def p_WriteItem_exp(t):
    r'WriteItem : Exp'
    t[0] = t[1] + ['writei\n']

# I/O - READLN
def p_Block_readln(t):
    r'Block : READLN "(" ID ")" ";"'
    if t[3] in parser.var:
        var_offset = parser.var[t[3]]
        var_type = parser.vartype.get(t[3], 'integer')
        
        if var_type == 'integer':
            t[0] = [f'read\natoi\nstoreg {var_offset}\n']
        elif var_type == 'string':
            t[0] = [f'read\nstoreg {var_offset}\n']
        else:
            t[0] = [f'read\nstoreg {var_offset}\n']
    else:
        print(f"Erro: variável '{t[3]}' não declarada")
        t[0] = []

def p_Block_readln_array(t):
    r'Block : READLN "(" ID "[" Exp "]" ")" ";"'
    if t[3] in parser.var:
        var_offset = parser.var[t[3]]
        t[0] = t[5] + [f'pushg {var_offset}\npadd\nread\natoi\nstoreg 0\n']
    else:
        t[0] = []

# ATRIBUIÇÃO
def p_Block_ass(t):
    r'Block : ID ASSIGN Exp ";"'
    if t[1] in parser.var:
        var_offset = parser.var[t[1]]
        t[0] = t[3] + [f'storeg {var_offset}\n']
    else:
        # Retorno de função
        t[0] = t[3] + ['storel 0\n']

def p_Block_ass_array(t):
    r'Block : ID "[" Exp "]" ASSIGN Exp ";"'
    if t[1] in parser.var:
        var_offset = parser.var[t[1]]
        t[0] = t[3] + [f'pushg {var_offset}\npadd\n'] + t[6] + ['storeg 0\n']
    else:
        t[0] = []

# CONTROLE DE FLUXO - IF
def p_Block_if_then(t):
    r'Block : IF Condition THEN Block'
    label = f'label_{parser.labelcount}'
    parser.labelcount += 1
    t[0] = t[2] + [f'jz {label}\n'] + t[4] + [f'{label}:\n']

def p_Block_if_then_else(t):
    r'Block : IF Condition THEN Block ELSE Block'
    label_else = f'label_{parser.labelcount}'
    label_end = f'label_{parser.labelcount + 1}'
    parser.labelcount += 2
    t[0] = t[2] + [f'jz {label_else}\n'] + t[4] + [f'jump {label_end}\n', f'{label_else}:\n'] + t[6] + [f'{label_end}:\n']

def p_Block_if_then_begin(t):
    r'Block : IF Condition THEN BEGIN Blocks END ";"'
    label = f'label_{parser.labelcount}'
    parser.labelcount += 1
    t[0] = t[2] + [f'jz {label}\n'] + t[5] + [f'{label}:\n']

def p_Block_if_then_else_begin(t):
    r'Block : IF Condition THEN BEGIN Blocks END ELSE BEGIN Blocks END ";"'
    label_else = f'label_{parser.labelcount}'
    label_end = f'label_{parser.labelcount + 1}'
    parser.labelcount += 2
    t[0] = t[2] + [f'jz {label_else}\n'] + t[5] + [f'jump {label_end}\n', f'{label_else}:\n'] + t[9] + [f'{label_end}:\n']

# WHILE
def p_Block_while(t):
    r'Block : WHILE Condition DO Block'
    label_start = f'label_{parser.labelcount}'
    label_end = f'label_{parser.labelcount + 1}'
    parser.labelcount += 2
    t[0] = [f'{label_start}:\n'] + t[2] + [f'jz {label_end}\n'] + t[4] + [f'jump {label_start}\n', f'{label_end}:\n']

def p_Block_while_begin(t):
    r'Block : WHILE Condition DO BEGIN Blocks END ";"'
    label_start = f'label_{parser.labelcount}'
    label_end = f'label_{parser.labelcount + 1}'
    parser.labelcount += 2
    t[0] = [f'{label_start}:\n'] + t[2] + [f'jz {label_end}\n'] + t[5] + [f'jump {label_start}\n', f'{label_end}:\n']

# FOR
def p_Block_for_to(t):
    r'Block : FOR ID ASSIGN Exp TO Exp DO Block'
    if t[2] in parser.var:
        var_offset = parser.var[t[2]]
        label_start = f'label_{parser.labelcount}'
        label_end = f'label_{parser.labelcount + 1}'
        parser.labelcount += 2
        
        t[0] = t[4] + [f'storeg {var_offset}\n', f'{label_start}:\n', f'pushg {var_offset}\n'] + t[6] + [f'inf\njz {label_end}\n'] + t[8] + [f'pushg {var_offset}\npushi 1\nadd\nstoreg {var_offset}\njump {label_start}\n', f'{label_end}:\n']
    else:
        t[0] = []

def p_Block_for_to_begin(t):
    r'Block : FOR ID ASSIGN Exp TO Exp DO BEGIN Blocks END ";"'
    if t[2] in parser.var:
        var_offset = parser.var[t[2]]
        label_start = f'label_{parser.labelcount}'
        label_end = f'label_{parser.labelcount + 1}'
        parser.labelcount += 2
        
        t[0] = t[4] + [f'storeg {var_offset}\n', f'{label_start}:\n', f'pushg {var_offset}\n'] + t[6] + [f'inf\njz {label_end}\n'] + t[9] + [f'pushg {var_offset}\npushi 1\nadd\nstoreg {var_offset}\njump {label_start}\n', f'{label_end}:\n']
    else:
        t[0] = []

def p_Block_for_downto(t):
    r'Block : FOR ID ASSIGN Exp DOWNTO Exp DO Block'
    if t[2] in parser.var:
        var_offset = parser.var[t[2]]
        label_start = f'label_{parser.labelcount}'
        label_end = f'label_{parser.labelcount + 1}'
        parser.labelcount += 2
        
        t[0] = t[4] + [f'storeg {var_offset}\n', f'{label_start}:\n', f'pushg {var_offset}\n'] + t[6] + [f'sup\njz {label_end}\n'] + t[8] + [f'pushg {var_offset}\npushi 1\nsub\nstoreg {var_offset}\njump {label_start}\n', f'{label_end}:\n']
    else:
        t[0] = []

def p_Block_for_downto_begin(t):
    r'Block : FOR ID ASSIGN Exp DOWNTO Exp DO BEGIN Blocks END ";"'
    if t[2] in parser.var:
        var_offset = parser.var[t[2]]
        label_start = f'label_{parser.labelcount}'
        label_end = f'label_{parser.labelcount + 1}'
        parser.labelcount += 2
        
        t[0] = t[4] + [f'storeg {var_offset}\n', f'{label_start}:\n', f'pushg {var_offset}\n'] + t[6] + [f'sup\njz {label_end}\n'] + t[9] + [f'pushg {var_offset}\npushi 1\nsub\nstoreg {var_offset}\njump {label_start}\n', f'{label_end}:\n']
    else:
        t[0] = []

# REPEAT
def p_Block_repeat(t):
    r'Block : REPEAT Blocks UNTIL Condition ";"'
    label_start = f'label_{parser.labelcount}'
    parser.labelcount += 1
    t[0] = [f'{label_start}:\n'] + t[2] + t[4] + [f'jz {label_start}\n']

# CONDIÇÕES
def p_Condition_or(t):
    r'Condition : Condition OR CondTerm'
    t[0] = t[1] + t[3] + ['add\n']

def p_Condition_term(t):
    r'Condition : CondTerm'
    t[0] = t[1]

def p_CondTerm_and(t):
    r'CondTerm : CondTerm AND CondFactor'
    t[0] = t[1] + t[3] + ['mul\n']

def p_CondTerm_factor(t):
    r'CondTerm : CondFactor'
    t[0] = t[1]

def p_CondFactor_not(t):
    r'CondFactor : NOT CondFactor'
    t[0] = t[2] + ['not\n']

def p_CondFactor_rel(t):
    r'CondFactor : Exp RelOp Exp'
    t[0] = t[1] + t[3] + [t[2]]

def p_CondFactor_paren(t):
    r'CondFactor : "(" Condition ")"'
    t[0] = t[2]

def p_CondFactor_bool_true(t):
    r'CondFactor : TRUE'
    t[0] = ['pushi 1\n']

def p_CondFactor_bool_false(t):
    r'CondFactor : FALSE'
    t[0] = ['pushi 0\n']

# OPERADORES RELACIONAIS
def p_RelOp_eq(t):
    r'RelOp : "="'
    t[0] = 'equal\n'

def p_RelOp_ne(t):
    r'RelOp : NE'
    t[0] = 'equal\nnot\n'

def p_RelOp_lt(t):
    r'RelOp : "<"'
    t[0] = 'inf\n'

def p_RelOp_le(t):
    r'RelOp : LE'
    t[0] = 'infeq\n'

def p_RelOp_gt(t):
    r'RelOp : ">"'
    t[0] = 'sup\n'

def p_RelOp_ge(t):
    r'RelOp : GE'
    t[0] = 'supeq\n'

# EXPRESSÕES
def p_Exp_sum(t):
    r'Exp : Exp "+" Term'
    t[0] = t[1] + t[3] + ['add\n']

def p_Exp_sub(t):
    r'Exp : Exp "-" Term'
    t[0] = t[1] + t[3] + ['sub\n']

def p_Exp_term(t):
    r'Exp : Term'
    t[0] = t[1]

def p_Term_mul(t):
    r'Term : Term "*" Factor'
    t[0] = t[1] + t[3] + ['mul\n']

def p_Term_div(t):
    r'Term : Term "/" Factor'
    t[0] = t[1] + t[3] + ['div\n']

def p_Term_div_int(t):
    r'Term : Term DIV Factor'
    t[0] = t[1] + t[3] + ['div\n']

def p_Term_mod(t):
    r'Term : Term MOD Factor'
    t[0] = t[1] + t[3] + ['mod\n']

def p_Term_factor(t):
    r'Term : Factor'
    t[0] = t[1]

# FATORES
def p_Factor_integer(t):
    r'Factor : INT'
    t[0] = [f'pushi {t[1]}\n']

def p_Factor_real(t):
    r'Factor : REAL'
    t[0] = [f'pushf {t[1]}\n']

def p_Factor_string(t):
    r'Factor : STR'
    t[0] = [f'pushs "{t[1]}"\n']

def p_Factor_true(t):
    r'Factor : TRUE'
    t[0] = ['pushi 1\n']

def p_Factor_false(t):
    r'Factor : FALSE'
    t[0] = ['pushi 0\n']

def p_Factor_id(t):
    r'Factor : ID'
    # Verifica se é constante
    if t[1] in parser.constants:
        const_type, const_value = parser.constants[t[1]]
        if const_type == 'int':
            t[0] = [f'pushi {const_value}\n']
        elif const_type == 'real':
            t[0] = [f'pushf {const_value}\n']
        elif const_type == 'str':
            t[0] = [f'pushs "{const_value}"\n']
        elif const_type == 'bool':
            t[0] = [f'pushi {1 if const_value else 0}\n']
    # Verifica se é variável
    elif t[1] in parser.var:
        var_offset = parser.var[t[1]]
        t[0] = [f'pushg {var_offset}\n']
    else:
        print(f"Erro: '{t[1]}' não declarado")
        t[0] = []

def p_Factor_array_access(t):
    r'Factor : ID "[" Exp "]"'
    if t[1] in parser.var:
        var_offset = parser.var[t[1]]
        t[0] = t[3] + [f'pushg {var_offset}\npadd\nloadn\n']
    else:
        t[0] = []

def p_Factor_function_call(t):
    r'Factor : ID "(" ArgumentList ")"'
    if t[1] == 'length':
        t[0] = t[3] + ['strlen\n']
    elif t[1] in parser.functions:
        func_label = parser.functions[t[1]]
        t[0] = t[3] + [f'pusha {func_label}\ncall\n']
    else:
        print(f"Erro: função '{t[1]}' não declarada")
        t[0] = []

def p_Factor_paren(t):
    r'Factor : "(" Exp ")"'
    t[0] = t[2]

def p_ArgumentList(t):
    r'ArgumentList : Exp MoreArguments'
    t[0] = t[1] + t[2]

def p_ArgumentList_vazio(t):
    r'ArgumentList : '
    t[0] = []

def p_MoreArguments(t):
    r'MoreArguments : "," Exp MoreArguments'
    t[0] = t[2] + t[3]

def p_MoreArguments_vazio(t):
    r'MoreArguments : '
    t[0] = []

def p_error(t):
    if t:
        print(f"Erro de sintaxe em '{t.value}' na linha {t.lineno}")
    else:
        print("Erro de sintaxe: fim de arquivo inesperado")

# ========================================
# INICIALIZAÇÃO
# ========================================
lexer.lineno = 1
parser = yacc.yacc()

# Dicionários simples (SEM escopos)
parser.var = {}           # {nome: offset}
parser.vartype = {}       # {nome: tipo}
parser.constants = {}     # {nome: (tipo, valor)}
parser.functions = {}     # {nome: label}
parser.varcount = 0       # Contador de variáveis
parser.labelcount = 0     # Contador de labels

parser.pushdict = {
    'integer': 'pushi 0\n',
    'real': 'pushf 0.0\n',
    'string': 'pushs ""\n',
    'boolean': 'pushi 0\n',
    'char': 'pushi 0\n'
}

# TESTES
ex1 = """
program Hello;
begin
    writeln('Hello, world!');
end.
"""

ex2 = """
program Fatorial;
var
    n, i, fat: integer;
begin
    writeln('Introduza um número inteiro positivo:');
    readln(n);
    fat := 1;
    for i := 1 to n do
        fat := fat * i;
    writeln('Fatorial de ', n, ': ', fat);
end.
"""

ex3 = """
program TesteConst;
const
    max = 100;
var
    x: integer;
begin
    x := max;
    writeln('Max: ', x);
end.
"""

if __name__ == "__main__":
    print("=== Teste Exemplo 1 ===")
    result = parser.parse(ex1)
    if result:
        print("".join(result))
    
    print("\n=== Teste Exemplo 2 ===")
    parser.var = {}
    parser.vartype = {}
    parser.constants = {}
    parser.varcount = 0
    parser.labelcount = 0
    result = parser.parse(ex2)
    if result:
        print("".join(result))
    
    print("\n=== Teste Exemplo 3 (Constantes) ===")
    parser.var = {}
    parser.vartype = {}
    parser.constants = {}
    parser.varcount = 0
    parser.labelcount = 0
    result = parser.parse(ex3)
    if result:
        print("".join(result))