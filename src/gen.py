import ast


import inspect

def lineno():
    """Returns the current line number in our program."""
    return inspect.currentframe().f_back.f_lineno



class ExampleCStyle(object):
    def __init__(self):
        self.indent_characters = "    "
        self.indent_size = 1

        self.has_newline_in_sue = True
        self.has_newline_after_FuncDef = True

        self.has_newline_after_If = True
        # always use {} for each statement, even only one line
        self.always_bracket_for_statement = True
        self.has_newline_else = True

        self.has_newline_after_For = True
        self.has_newline_after_DoWhile_Do = True
        self.has_newline_after_DoWhile_While = True
        self.has_newline_after_Switch = True
        self.has_newline_after_While = True
        self.has_newline_after_Case = True


        self.fmt_ArrayRef = "%s[%s]"
        self.fmt_FuncCall = "%s(%s)"
        self.fmt_SelfInc = '%s++'
        self.fmt_SelfDec = '%s--'
        self.fmt_SizeOf = 'sizeof(%s)'
        self.fmt_BinaryOp = '%s %s %s'
        self.fmt_Assignment = '%s %s %s'
        self.fmt_ForLoop = 'for (%s; %s; %s)'


class ExampleCStyle2(ExampleCStyle):
    def __init__(self):
        super(ExampleCStyle2, self).__init__()

        self.has_newline_in_sue = True
        self.has_newline_after_FuncDef = False

        self.has_newline_after_If = False
        # always use {} for each statement, even only one line
        self.always_bracket_for_statement = False
        self.has_newline_else = False

        self.has_newline_after_For = False
        self.has_newline_after_DoWhile_Do = False
        self.has_newline_after_DoWhile_While = False
        self.has_newline_after_Switch = False
        self.has_newline_after_While = False
        self.has_newline_after_Case = False

class CodeGenerator:
    def __init__(self, style = ExampleCStyle2()):
        self.indent = style.indent_size
        self.indent_char = style.indent_characters

        self.current_indent = 0
        self.style = style

    def _gen_indent(self):
        return self.indent_char * self.current_indent

    def visit(self, node):
        call = '_gen_' + node.__class__.__name__
        s = getattr(self, call, self._gen_Generic)(node)

        return s

    def _gen_Generic(self, node):
        if node:
            return ''.join(self.visit(child) for child_name, child in node.children())
        else:
            return ''



    def _gen_expr(self, node):
        # TODO:  move to style
        if isinstance(node, ast.InitList):
            return '{' + self.visit(node) + '}'
        elif isinstance(node, ast.ExprList):
            return '(' + self.visit(node) + ')'
        else:
            return self.visit(node)



    def _generate_type(self, n, modifiers=[], emit_declname = True):
        """ Recursive generation from a type node. n is the type node.
            modifiers collects the PtrDecl, ArrayDecl and FuncDecl modifiers
            encountered on the way down to a TypeDecl, to allow proper
            generation from it.
        """
        typ = type(n)
        #~ print(n, modifiers)

        if typ == ast.TypeDecl:
            s = ''
            if n.quals: s += ' '.join(n.quals) + ' '
            s += self.visit(n.type)

            nstr = n.declname if n.declname and emit_declname else ''
            # Resolve modifiers.
            # Wrap in parens to distinguish pointer to array and pointer to
            # function syntax.
            #
            for i, modifier in enumerate(modifiers):
                if isinstance(modifier, ast.ArrayDecl):
                    if (i != 0 and
                        isinstance(modifiers[i - 1], ast.PtrDecl)):
                            nstr = '(' + nstr + ')'
                    nstr += '['
                    if modifier.dim_quals:
                        nstr += ' '.join(modifier.dim_quals) + ' '
                    nstr += self.visit(modifier.dim) + ']'
                elif isinstance(modifier, ast.FuncDecl):
                    if (i != 0 and
                        isinstance(modifiers[i - 1], ast.PtrDecl)):
                            nstr = '(' + nstr + ')'
                    nstr += '(' + self.visit(modifier.args) + ')'
                elif isinstance(modifier, ast.PtrDecl):
                    if modifier.quals:
                        nstr = '* %s%s' % (' '.join(modifier.quals),
                                           ' ' + nstr if nstr else '')
                    else:
                        nstr = '*' + nstr
            # if nstr: s += ' ' + nstr

            s += (' ' + nstr) if nstr else ''
            return s
        elif typ == ast.Decl:
            return self._generate_decl(n.type)
        elif typ == ast.Typename:
            return self._generate_type(n.type, emit_declname = emit_declname)
        elif typ == ast.IdentifierType:
            return ' '.join(n.names) + ' '
        elif typ in (ast.ArrayDecl, ast.PtrDecl, ast.FuncDecl):
            return self._generate_type(n.type, modifiers + [n],
                                       emit_declname = emit_declname)
        else:
            return self.visit(n)



    def _generate_decl(self, n):
        """ Generation from a Decl node.
        """
        s = ''
        if n.funcspec: s = ' '.join(n.funcspec) + ' '
        if n.storage: s += ' '.join(n.storage) + ' '
        s += self._generate_type(n.type)
        return s





    def _generate_struct_union_body(self, members):
        return ''.join(self._generate_stmt(decl) for decl in members)

    def _generate_enum_body(self, members):
        # `[:-2] + '\n'` removes the final `,` from the enumerator list
        return ''.join(self.visit(value) for value in members)[:-2] + '\n'

    def _generate_stmt(self, n, add_indent=False):
        """ Generation from a statement node. This method exists as a wrapper
            for individual _gen_* methods to handle different treatment of
            some statements in this context.
        """
        typ = type(n)
        if add_indent: self.current_indent += self.indent
        indent = self._gen_indent()
        if add_indent: self.current_indent -= self.indent

        if typ in (
                ast.Decl, ast.Assignment, ast.Cast, ast.UnaryOp,
                ast.BinaryOp, ast.TernaryOp, ast.FuncCall, ast.ArrayRef,
                ast.StructRef, ast.Constant, ast.ID, ast.Typedef,
                ast.ExprList):
            # These can also appear in an expression context so no semicolon
            # is added to them automatically
            #
            return indent + self.visit(n) + ';\n'
        elif typ in (ast.Compound,):
            # No extra indentation required before the opening brace of a
            # compound - because it consists of multiple lines it has to
            # compute its own indentation.
            #
            return self.visit(n)
        else:
            return indent + self.visit(n) + '\n'




    def _generate_sue(self, node, name):
        """ Generates code for structs, unions, and enums. """

        if name in ('struct', 'union'):
            members = node.decls
            body_function = self._generate_struct_union_body
        else:
            # name == 'enum'
            members = None if node.values is None else node.values.enumerators
            body_function = self._generate_enum_body
        s = name + ' ' + (node.name or '')
        if members is not None:
            # None means no members
            # Empty sequence means an empty list of members

            if self.style.has_newline_in_sue:
                s += '\n' + self._gen_indent() + '{\n'
                self.current_indent += self.indent
            else:
                s += ' {\n'
            s += body_function(members)
            self.current_indent -= self.indent
            s += self._gen_indent() + '}'
        return s




    def _is_simple_node(self, node):
        """ nodes that always have higher precedence than operators. """
        return isinstance(node, (ast.Constant, ast.ID, ast.ArrayRef,
                              ast.StructRef, ast.FuncCall))


    def _parenthesize_if(self, node, condition):
        """ Visits node and returns its string representation, parenthesized
            if the condition function applied to the node returns True.
        """
        s = self._gen_expr(node)
        if condition(node):
            return '(' + s + ')'
        else:
            return s

    def _parenthesize_unless_simple(self, node):
        """ Common use case for _parenthesize_if
        """
        return self._parenthesize_if(node, lambda n: not self._is_simple_node(n))


    def _gen_Constant(self, node):
        return node.value

    def _gen_ID(self, node):
        return node.name

    def _gen_Pragma(self, node):
        if node.string:
            return '#pragma ' + node.string
        else:
            return '#pragma'

    def _gen_ArrayRef(self, node):
        arrref = self._parenthesize_unless_simple(node.name)
        return self.style.fmt_ArrayRef % (arrref, self.visit(node.subscript))


    def _gen_StructRef(self, node):
        sref = self._parenthesize_unless_simple(node.name)
        return sref + node.type + self.visit(node.field)

    def _gen_FuncCall(self, node):
        fcall = self._parenthesize_unless_simple(node.name)
        return self.style.fmt_FuncCall % (fcall, self.visit(node.args))

    def _gen_UnaryOp(self, node):
        operand = self._parenthesize_unless_simple(node.expr)
        if node.op == 'p++':
            return self.style.fmt_SelfInc % operand
        elif node.op == 'p--':
            return self.style.fmt_SelfInc % operand
        elif node.op == 'sizeof':
            return self.style.fmt_SizeOf % self.visit(node.expr)
        else:
            return '%s%s' % (node.op, operand)

    def _gen_BinaryOp(self, node):
        lval = self._parenthesize_if(node.left, lambda n : not self._is_simple_node(n))
        rval = self._parenthesize_if(node.right, lambda n : not self._is_simple_node(n))
        return self.style.fmt_BinaryOp % (lval, node.op, rval)

    def _gen_Assignment(self, node):
        # should here be always true?
        rval = self._parenthesize_if(node.rvalue, lambda n : isinstance(n, ast.Assignment))
        return self.style.fmt_Assignment % (self.visit(node.lvalue), node.op, rval)

    def _gen_IdentifierType(self, node):
        # TODO: is it possible here be more than one names?
        return ' '.join(node.names)









    def _gen_Decl(self, node, typ = True):
        s = self._generate_decl(node) if typ else node.name
        s += (" : %s" % self.visit(node.bitsize)) if node.bitsize else ''
        s += (" = %s" % self._gen_expr(node.init)) if node.init else ''

        return s

    def _gen_DeclList(self, node):
        s = self.visit(node.decls[0])
        if len(node.decls) > 1:
            for eachDecl in node.decls[1:]:
                s += ', %s' % self._gen_Decl(eachDecl, typ = False)
        return s

    def _gen_Typedef(self, node):
        s = (' '.join(node.storage) + ' ') if node.storage else ''
        s += self._generate_type(node.type)
        return s

    # def _gen_Case(self, node):
    #     to = '(%s)' % self._generate_type(node.to_type)
    #     s = '%s%s' % (to, self._parenthesize_unless_simple(node.expr))
    #
    #     return s

    def _gen_ExprList(self, node):
        s = ', '.join(self._gen_expr(eachExpr) for eachExpr in node.exprs)
        return s

    def _gen_InitList(self, node):
        return self._gen_ExprList(self, node)





    def _gen_Enum(self, node):
        return self._generate_sue(node, name='enum')


    def _gen_Struct(self, node):
        return self._generate_sue(node, 'struct')

    def _gen_Union(self, node):
        return self._generate_sue(node, 'union')

    def _gen_Enumerator(self, node):
        if not node.value:
            return '{indent}{name},\n'.format(
                indent=self._gen_indent(),
                name=node.name,
            )
        else:
            return '{indent}{name} = {value},\n'.format(
                indent=self._gen_indent(),
                name=node.name,
                value=self.visit(node.value),
            )

    def _gen_Typename(self, node):
        return self._generate_type(node.type)














    def _gen_FuncDef(self, node):
        # Reset indent
        self.current_indent = 0


        decl = self.visit(node.decl)
        body = self.visit(node.body)

        # TODO: move to coding style
        if node.param_decls:
            knrdecls = ';\n'.join(self.visit(p) for p in node.param_decls)
            # print("Line %s, %s" % (lineno(), knrdecls))
            return decl + '\n' + knrdecls + ';\n' + body + '\n'
        else:
            seperator = '\n' if self.style.has_newline_after_FuncDef else ' '
            return decl + seperator + body + '\n'

    def _gen_FileAST(self, node):
        s = ''
        for ext in node.ext:
            if isinstance(ext, ast.FuncDef):
                s += self.visit(ext)
            elif isinstance(ext, ast.Pragma):
                s += self.visit(ext) + '\n'
            else:
                s += self.visit(ext) + ';\n\n'
        return s

    def _gen_Compound(self, node, first_line_indent = True):

        # TODO: move to coding GoogleCStyle
        s = (self._gen_indent() if first_line_indent else '') + '{\n'
        self.current_indent += self.indent
        if node.block_items:
            s += ''.join(self._generate_stmt(stmt) for stmt in node.block_items)
        self.current_indent -= self.indent
        s += self._gen_indent() + '}\n'
        return s

    def _gen_CompoundLiteral(self, node):
        return '(%s){%s}' % (self.visit(node.type), self.visit(node.init))


    def _gen_EmptyStatement(self, node):
        return ';'

    def _gen_ParamList(self, node):
        return ', '.join(self.visit(param) for param in node.params)

    def _gen_Return(self, node):
        s = (' %s' % self.visit(node.expr)) if node.expr else ''
        return 'return%s;' % s

    def _gen_Break(self, node):
        return 'break;'

    def _gen_Continue(self, node):
        return 'continue;'

    def _gen_TernaryOp(self, node):
        # TODO:  move to style
        s  = '(' + self._gen_expr(node.cond) + ') ? '
        s += '(' + self._gen_expr(node.iftrue) + ') : '
        s += '(' + self._gen_expr(node.iffalse) + ')'
        return s


    def _generate_bracket_stmt(self, node, newline):

        if not isinstance(node, ast.Compound):
            if self.style.always_bracket_for_statement:
                stmt = self._generate_stmt(node, add_indent = True)
                s = self._gen_indent() if newline else ''
                s += '{\n%s' % stmt
                s += self._gen_indent()
                s += '}'
            else:
                indent = self.indent * self.current_indent * len(self.indent_char)
                s = ' ' + self._generate_stmt(node, add_indent = False)[indent: -1]

            return s
        else:
            return self._gen_Compound(node, newline)

    def _gen_If(self, node):
        s = 'if ('
        if node.cond: s += self.visit(node.cond)
        s += ')\n' if self.style.has_newline_after_If else ') '

        # indent if has newline
        # s += self._generate_stmt(node.iftrue, add_indent=True)
        s += self._generate_bracket_stmt(node.iftrue, self.style.has_newline_after_If)
        if node.iffalse:
            if self.style.has_newline_else:
                s += '\n' + self._gen_indent() + 'else\n'
            else:
                s += ' else '
            # s += self._generate_stmt(node.iffalse, add_indent=True)
            s += self._generate_bracket_stmt(node.iffalse, self.style.has_newline_else)
        else:
            s += '\n'
        return s

    def _gen_For(self, node):
        for_init = self.visit(node.init) if node.init else ' '
        for_cond = self.visit(node.cond) if node.cond else ' '
        for_next = self.visit(node.next) if node.next else ' '

        s = self.style.fmt_ForLoop % (for_init, for_cond, for_next)

        s += '\n' if self.style.has_newline_after_For else ' '

        # s += self._generate_stmt(node.stmt, add_indent=True)
        s += self._generate_bracket_stmt(node.stmt, self.style.has_newline_after_For)
        return s

    def _gen_While(self, node):
        # TODO:  move to style
        s = 'while ('
        if node.cond: s += self.visit(node.cond)
        s += ')\n' if self.style.has_newline_after_While else ') '
        # s += self._generate_stmt(node.stmt, add_indent=True)
        s += self._generate_bracket_stmt(node.stmt, self.style.has_newline_after_While)
        return s

    def _gen_DoWhile(self, node):
        # TODO:  move to style
        s = 'do\n' if self.style.has_newline_after_DoWhile_Do else 'do '
        # s += self._generate_stmt(node.stmt, add_indent=True)
        s +=  self._generate_bracket_stmt(node.stmt, self.style.has_newline_after_DoWhile_Do)

        if self.style.has_newline_after_DoWhile_While:
            s += self._gen_indent() + 'while ('
        else:
            s = s[:-1]
            s += ' while('
        if node.cond: s += self.visit(node.cond)
        s += ');\n'
        return s

    def _gen_Switch(self, node):
        # TODO:  move to style
        s = 'switch (' + self.visit(node.cond) + ')'
        s += '\n'if self.style.has_newline_after_Switch else ' '
        # s += self._generate_stmt(node.stmt, add_indent=True)
        s +=  self._generate_bracket_stmt(node.stmt, self.style.has_newline_after_Switch)
        return s

    def _gen_Case(self, node):
        # TODO:  move to style
        s = 'case ' + self.visit(node.expr) + ':\n'
        for stmt in node.stmts:
            s += self._generate_stmt(stmt, add_indent=True)

        s = s if self.style.has_newline_after_Case else s[:-1]
        return s

    def _gen_Default(self, node):
        # TODO:  move to style
        s = 'default:\n'
        for stmt in node.stmts:
            s += self._generate_stmt(stmt, add_indent=True)
        s = s if self.style.has_newline_after_Case else s[:-1]
        return s

    def _gen_Label(self, node):
        # TODO:  move to style
        return node.name + ':\n' + self._generate_stmt(node.stmt)

    def _gen_Goto(self, node):
        return 'goto ' + node.name + ';'

    def _gen_EllipsisParam(self, node):
        return '...'

    def _gen_NamedInitializer(self, node):
        s = ''
        for name in node.name:
            if isinstance(name, ast.ID):
                s += '.' + name.name
            else:
                s += '[' + self.visit(name) + ']'
        s += ' = ' + self._gen_expr(node.expr)
        return s

    def _gen_FuncDecl(self, node):
        return self._generate_type(n)

    def _gen_ArrayDecl(self, node):
        return self._generate_type(n, emit_declname=False)

    def _gen_TypeDecl(self, node):
        return self._generate_type(n, emit_declname=False)

    def _gen_PtrDecl(self, node):
        return self._generate_type(n, emit_declname=False)
