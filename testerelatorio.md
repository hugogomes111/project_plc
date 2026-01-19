
# Processamento de Linguagens e Compiladores

## Construção de um compilador em Pascal
### Grupo 13
* Vitor Pereira - A102515
* Hugo Gomes - A100056
* Gustavo Gomes - A101777

## Introdução

Este trabalho consiste no desenvolvimento de um compilador para a linguagem Pascal Standard, no âmbito da disciplina de Processamento de Linguagens e Compiladores. O objetivo é criar um compilador funcional capaz de processar código Pascal e convertê-lo para código executável numa máquina virtual. 

O desenvolvimento do compilador segue as seguintes etapas: análise léxica, análise sintática, análise semântica e geração de código. O compilador final processa código Pascal e gera código executável para uma máquina virtual. 

Este relatório apresenta a implementação realizada e os testes efetuados. 

## 1. Análise Léxica
O analisador léxico (lexer) é então a primeira etapa do processo de compilação. 

O lexer desenvolvido neste projeto foi implementado em Python, recorrendo à biblioteca PLY (Python Lex-Yacc). 

O objetivo deste componente é converter o texto bruto do programa numa sequência de símbolos terminais (tokens) compreensíveis pelo parser. São identificados elementos como palavras reservadas, identificadores, operadores, literais e símbolos especiais, enquanto são descartados comentários e espaços em branco. 
### 1.1. Tokens
Começamos por definir todos os tokens relevantes, incluindo palavras reservadas, identificadores, literais numéricos (inteiros e reais), valores booleanos, strings, caracteres, operadores relacionais, símbolos especiais (como atribuição e intervalos) e comentários. 

```python
tokens = (
    # palavras reservadas
    'AND', 'ARRAY', 'BEGIN', 'CASE', 'CONST', 'DIV', 'DO', 'DOWNTO', 'ELSE', 'END',
    'FILE', 'FOR', 'FUNCTION', 'GOTO', 'IF', 'IN', 'LABEL', 'MOD', 'NIL', 'NOT',
    'OF', 'OR', 'PACKED', 'PROCEDURE', 'PROGRAM', 'RECORD', 'REPEAT', 'SET', 'STRING', 'THEN',
    'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE', 'WITH', 'FORWARD', 'INTEGER', 'WRITELN', 'READLN',
    'LENGTH', 'TRUE', 'FALSE', 'CHAR', 'REALTYPE', 'WRITE', 'READ',
    # simbolos
    'NE', 'LE', 'GE', 'ASSIGN', 'DOTDOT',
    # tipos de dados
    'ID', 'INT', 'BOOLEAN', 'REAL', 'STR', 'COMMENT'
)
```
