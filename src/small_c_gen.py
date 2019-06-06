# -----------------------------------------------------------------------------
# 203 Project
#
# A simple C lexer, parser, and generator
#
# Chao Zhao (czhao45@ucsc.edu)
# -----------------------------------------------------------------------------

from ply import lex
import ply.yacc as yacc

############################################################## LEXER ###################################################

# Reserved keywords

reserved = {
    "void": "VOID",
    "if": 'IF',
    "else": "ELSE",
    "while": "WHILE",
    "for": "FOR",
    "break": "BREAK",
    "continue": "CONTINUE",
    "return": "RETURN",
    "char": "CHAR",
    "int": "INT",
    "float": "FLOAT"
}

# List of token names.   This is always required
tokens = ('ID',
          'NUMBER',
          'LT', 'GT', 'LE', 'GE', 'EQ', 'NE',
          'AND', 'OR'
          ) + tuple(reserved.values())

literals = ['=', '+', '-', '*', '/', '(', ')', '[', ']', '{', '}', ',', '.', ';', ':']


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Check for reserved words
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_LT = r'<'
t_GT = r'>'
t_LE = r'<='
t_GE = r'>='
t_EQ = r'=='
t_NE = r'!='
t_AND = r'&'
t_OR = r'\|'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


t_ignore = ' \t'
t_ignore_COMMENT = r'//.*'


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


def find_column(input, token):
    last_cr = input.rfind('\n', 0, token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column


lexer = lex.lex()

############################################################## AST ###################################################

from anytree import AnyNode
from anytree import RenderTree


class Node(AnyNode):

    def __init__(self, type, children=None, leaf=None):
        AnyNode.__init__(self)
        self.type = type
        if children:
            self.children = children
        else:
            self.children = []
        self.leaf = leaf


class NoteVisitor:
    def __init__(self):
        self.indent_level = 0

    def make_indent(self):
        return '    ' * self.indent_level

    def visit(self, n):
        method = getattr(self, 'visit_{}'.format(n.type))
        return method(n)

    def visit_symbol(self, n):
        return n.leaf

    def visit_type(self, n):
        return n.leaf

    def visit_assign_op(self, n):
        return "{} = {}".format(self.visit(n.children[0]), self.visit(n.children[1]))

    def visit_bin_op(self, n):
        return "{} {} {}".format(self.visit(n.children[0]), n.leaf, self.visit(n.children[1]))

    def visit_neg_op(self, n):
        return "-{}".format(self.visit(n.children[0]))

    def visit_number(self, n):
        return str(n.leaf)

    def visit_statement(self, n):
        return self.make_indent() + self.visit(n) + ';\n'

    def visit_decl_statement(self, n):
        if len(n.children) == 2:
            return self.make_indent() + "{} {}".format(self.visit(n.children[0]),
                                                       self.visit(n.children[1])) + ';\n'
        else:
            return self.make_indent() + "{} {} = {}".format(self.visit(n.children[0]),
                                                            self.visit(n.children[1]),
                                                            self.visit(n.children[2])) + ';\n'

    def visit_control_statement(self, n):
        return self.make_indent() + n.leaf + ';\n'

    def visit_exp_statement(self, n):
        return self.make_indent() + self.visit(n.children[0]) + ';\n'

    def visit_statement_list(self, n):
        if len(n.children) == 1:
            return self.visit(n.children[0])
        else:
            return self.visit(n.children[0]) \
                   + self.visit(n.children[1])

    def visit_compound_statement(self, n):
        return self.visit(n.children[0])

    def visit_if_statement1(self, n):
        s = self.make_indent() + 'if ({}) {{\n'.format(self.visit(n.children[0]))
        self.indent_level += 1
        s += self.visit(n.children[1])
        self.indent_level -= 1
        s += self.make_indent() + '} else {\n'
        self.indent_level += 1
        s += self.visit(n.children[2])
        self.indent_level -= 1
        s += self.make_indent() + '}\n'
        return s

    def visit_while_statement(self, n):
        s = self.make_indent() + 'while ({}) {{\n'.format(self.visit(n.children[0]))
        self.indent_level += 1
        s += self.visit(n.children[1])
        self.indent_level -= 1
        s += self.make_indent() + '}\n'
        return s

    def visit_for_statement(self, n):
        s = self.make_indent() + 'for ({}; {}; {}) {{\n'.format(self.visit(n.children[0]),
                                                                self.visit(n.children[1]),
                                                                self.visit(n.children[2]))
        self.indent_level += 1
        s += self.visit(n.children[3])
        self.indent_level -= 1
        s += self.make_indent() + '}\n'
        return s

    def visit_function_statement(self, n):
        s = self.make_indent() + '{} {}({}) {{\n'.format(self.visit(n.children[0]),
                                                         self.visit(n.children[1]),
                                                         self.visit(n.children[2]))
        self.indent_level += 1
        s += self.visit(n.children[3])
        self.indent_level -= 1
        s += self.make_indent() + '}\n'
        return s

    def visit_return_statement(self, n):
        s = self.make_indent() + "return {}".format(self.visit(n.children[0])) + ';\n'
        return s

    def visit_parameter(self, n):
        return "{} {}".format(self.visit(n.children[0]), self.visit(n.children[1]))

    def visit_param_list(self, n):
        if len(n.children) == 1:
            return self.visit(n.children[0])
        else:
            return self.visit(n.children[0]) + ', ' \
                   + self.visit(n.children[1])


############################################################## PARSER ###################################################


precedence = (
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('right', 'UMINUS'),
)

start = 'statement_list'


def p_statement_list(p):
    """ statement_list : statement
                   | statement statement_list
    """
    if len(p) == 2:
        p[0] = Node('statement_list', children=[p[1]])
    else:
        p[0] = Node('statement_list', children=[p[1], p[2]])


def p_statement(p):
    """ statement : decl_statement
                  | exp_statement
                  | compound_statement
                  | if_statement1
                  | while_statement
                  | for_statement
                  | function_statement
                  | control_statement
                  | return_statement
    """
    p[0] = p[1]


def p_type_basetype(p):
    """ type : INT
             | CHAR
             | FLOAT
             | VOID """
    p[0] = Node('type', leaf=p[1])


def p_symbol(p):  # var, name
    "symbol : ID"
    p[0] = Node('symbol', leaf=p[1])


def p_decl_statement(p):
    """decl_statement : type symbol ";"
                      | type symbol "=" expression ";" """
    if len(p) == 4:
        p[0] = Node('decl_statement', children=[p[1], p[2]])
    else:
        p[0] = Node('decl_statement', children=[p[1], p[2], p[4]])


def p_expression_statement(p):
    """ exp_statement : expression ";" """
    p[0] = Node('exp_statement', children=[p[1]])


def p_compound_statement(p):
    """ compound_statement : "{" statement_list "}" """
    p[0] = Node('compound_statement', children=[p[2]])


def p_if_statement1(p):
    """if_statement1 : IF "(" expression ")" compound_statement ELSE compound_statement"""
    p[0] = Node('if_statement1', children=[p[3], p[5], p[7]])


def p_while_statement(p):
    """while_statement : WHILE "(" expression ")" compound_statement"""
    p[0] = Node('while_statement1', children=[p[3], p[5]])


def p_for_statement(p):
    """ for_statement : FOR "(" expression ";" expression ";" expression ")" compound_statement """
    p[0] = Node('for_statement', children=[p[3], p[5], p[7], p[9]])


def p_control_statement(p):
    """ control_statement  : BREAK ";"
                           | CONTINUE ";" """
    p[0] = Node("control_statement", leaf=p[1])


def p_function_statement(p):
    """function_statement : type symbol "(" param_list ")" compound_statement """
    p[0] = Node('function_statement', children=[p[1], p[2], p[4], p[6]])


def p_return_statement(p):
    """return_statement : RETURN expression ";" """
    p[0] = Node('return_statement', children=[p[2]])


def p_expression_assign(p):
    'expression : symbol "=" expression'

    p[0] = Node('assign_op', [p[1], p[3]], p[2])


def p_expression_symbol(p):
    'expression : symbol'
    p[0] = p[1]


def p_expression_binop(p):
    """expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression
                  | expression LT expression
                  | expression LE expression
                  | expression GE expression
                  | expression GT expression
                  | expression EQ expression
                  | expression NE expression
                  | expression AND expression
                  | expression OR expression
                  """
    p[0] = Node("bin_op", [p[1], p[3]], p[2])


def p_expression_uminus(p):
    """expression : '-' expression %prec UMINUS"""
    p[0] = Node("neg_op", [p[2]])


def p_expression_number(p):
    "expression : NUMBER"
    p[0] = Node('number', leaf=p[1])


def p_param(p):
    "parameter : type symbol"
    p[0] = Node('parameter', children=[p[1], p[2]])


def p_param_list(p):
    """param_list : parameter
                  | parameter "," param_list"""
    if len(p) == 2:
        p[0] = Node('param_list', children=[p[1]])
    else:
        p[0] = Node('param_list', children=[p[1], p[3]])


# statement


def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()

s = """
int a = 5;
int max(int num1, int num2)
{
int i;
if( num1 < 20 ) {
      for( i = 0; i <= num2; i=i-1 ) { num1 = num1-1;break;}
   } else {
      return num1;
   }
   return num1;}
    """

# Give the lexer some input
# lexer.input(s)

# Tokenize
# while True:
#     tok = lexer.token()
#     if not tok: break  # No more input
#     print(tok)

ast_tree = parser.parse(s, lexer=lexer)
visitor = NoteVisitor()

print(visitor.visit(ast_tree))
print(RenderTree(ast_tree))
