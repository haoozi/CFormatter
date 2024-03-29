#------------------------------------------------------------------------------
# pycparser: c_parser.py
#
# CParser class: Parser and AST builder for the C language
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#------------------------------------------------------------------------------
import re

from ply import yacc

import ast
from lexer import CLexer
from plyparser import PLYParser, Coord, ParseError, parameterized, template





def fix_switch_cases(switch_node):
    """ The 'case' statements in a 'switch' come out of parsing with one
        child node, so subsequent statements are just tucked to the parent
        Compound. Additionally, consecutive (fall-through) case statements
        come out messy. This is a peculiarity of the C grammar. The following:

            switch (myvar) {
                case 10:
                    k = 10;
                    p = k + 1;
                    return 10;
                case 20:
                case 30:
                    return 20;
                default:
                    break;
            }

        Creates this tree (pseudo-dump):

            Switch
                ID: myvar
                Compound:
                    Case 10:
                        k = 10
                        p = k + 1
                        return 10
                    Case 20:
                        Case 30:
                            return 20
                    Default:
                        break

        The goal of this transform is to fix this mess, turning it into the
        following:

            Switch
                ID: myvar
                Compound:
                    Case 10:
                        k = 10
                        p = k + 1
                        return 10
                    Case 20:
                    Case 30:
                        return 20
                    Default:
                        break

        A fixed AST node is returned. The argument may be modified.
    """
    assert isinstance(switch_node, ast.Switch)
    if not isinstance(switch_node.stmt, ast.Compound):
        return switch_node

    # The new Compound child for the Switch, which will collect children in the
    # correct order
    new_compound = ast.Compound([], switch_node.stmt.coord)

    # The last Case/Default node
    last_case = None

    # Goes over the children of the Compound below the Switch, adding them
    # either directly below new_compound or below the last Case as appropriate
    for child in switch_node.stmt.block_items:
        if isinstance(child, (ast.Case, ast.Default)):
            # If it's a Case/Default:
            # 1. Add it to the Compound and mark as "last case"
            # 2. If its immediate child is also a Case or Default, promote it
            #    to a sibling.
            new_compound.block_items.append(child)
            _extract_nested_case(child, new_compound.block_items)
            last_case = new_compound.block_items[-1]
        else:
            # Other statements are added as children to the last case, if it
            # exists.
            if last_case is None:
                new_compound.block_items.append(child)
            else:
                last_case.stmts.append(child)

    switch_node.stmt = new_compound
    return switch_node


def _extract_nested_case(case_node, stmts_list):
    """ Recursively extract consecutive Case statements that are made nested
        by the parser and add them to the stmts_list.
    """
    if isinstance(case_node.stmts[0], (ast.Case, ast.Default)):
        stmts_list.append(case_node.stmts.pop())
        _extract_nested_case(stmts_list[-1], stmts_list)






@template
class CParser(PLYParser):
    def __init__(self):
        """ Create a new CParser."""
        self.clex = CLexer(
            error_func=self._lex_error_func,
            on_lbrace_func=self._lex_on_lbrace_func,
            on_rbrace_func=self._lex_on_rbrace_func,
            type_lookup_func=self._lex_type_lookup_func)

        self.clex.build()

        self.tokens = self.clex.tokens

        rules_with_opt = [
            'abstract_declarator',
            'assignment_expression',
            'declaration_list',
            'declaration_specifiers_no_type',
            'designation',
            'expression',
            'identifier_list',
            'init_declarator_list',
            'id_init_declarator_list',
            'initializer_list',
            'parameter_type_list',
            'block_item_list',
            'type_qualifier_list',
            'struct_declarator_list'
        ]

        for rule in rules_with_opt:
            self._create_opt_rule(rule)

        self.cparser = yacc.yacc(
            module=self,
            start='translation_unit_or_empty')

        # Stack of scopes for keeping track of symbols. _scope_stack[-1] is
        # the current (topmost) scope. Each scope is a dictionary that
        # specifies whether a name is a type. If _scope_stack[n][name] is
        # True, 'name' is currently a type in the scope. If it's False,
        # 'name' is used in the scope but not as a type (for instance, if we
        # saw: int name;
        # If 'name' is not a key in _scope_stack[n] then 'name' was not defined
        # in this scope at all.
        self._scope_stack = [dict()]

        # Keeps track of the last token given to yacc (the lookahead token)
        self._last_yielded_token = None

    def parse(self, text, filename='', debuglevel=0):
        """ Parses C code and returns an AST.
        """
        self.clex.filename = filename
        self.clex.reset_lineno()
        self._scope_stack = [dict()]
        self._last_yielded_token = None
        return self.cparser.parse(
                input=text,
                lexer=self.clex,
                debug=debuglevel)






# privates

    def _push_scope(self):
        self._scope_stack.append(dict())

    def _pop_scope(self):
        assert len(self._scope_stack) > 1
        self._scope_stack.pop()

    def _add_typedef_name(self, name, coord):
        """ Add a new typedef name (ie a TYPEID) to the current scope
        """
        if not self._scope_stack[-1].get(name, True):
            self._parse_error(
                "Typedef %r previously declared as non-typedef "
                "in this scope" % name, coord)
        self._scope_stack[-1][name] = True

    def _add_identifier(self, name, coord):
        """ Add a new object, function, or enum member name (ie an ID) to the
            current scope
        """
        if self._scope_stack[-1].get(name, False):
            self._parse_error(
                "Non-typedef %r previously declared as typedef "
                "in this scope" % name, coord)
        self._scope_stack[-1][name] = False

    def _is_type_in_scope(self, name):
        """ Is *name* a typedef-name in the current scope?
        """
        for scope in reversed(self._scope_stack):
            # If name is an identifier in this scope it shadows typedefs in
            # higher scopes.
            in_scope = scope.get(name)
            if in_scope is not None: return in_scope
        return False

    def _lex_error_func(self, msg, line, column):
        self._parse_error(msg, self._coord(line, column))

    def _lex_on_lbrace_func(self):
        self._push_scope()

    def _lex_on_rbrace_func(self):
        self._pop_scope()

    def _lex_type_lookup_func(self, name):
        """ Looks up types that were previously defined with
            typedef.
            Passed to the lexer for recognizing identifiers that
            are types.
        """
        is_type = self._is_type_in_scope(name)
        return is_type

    def _get_yacc_lookahead_token(self):
        """ We need access to yacc's lookahead token in certain cases.
            This is the last token yacc requested from the lexer, so we
            ask the lexer.
        """
        return self.clex.last_token

    # To understand what's going on here, read sections A.8.5 and
    # A.8.6 of K&R2 very carefully.
    #
    # A C type consists of a basic type declaration, with a list
    # of modifiers. For example:
    #
    # int *c[5];
    #
    # The basic declaration here is 'int c', and the pointer and
    # the array are the modifiers.
    #
    # Basic declarations are represented by TypeDecl (from module ast) and the
    # modifiers are FuncDecl, PtrDecl and ArrayDecl.
    #
    # The standard states that whenever a new modifier is parsed, it should be
    # added to the end of the list of modifiers. For example:
    #
    # K&R2 A.8.6.2: Array Declarators
    #
    # In a declaration T D where D has the form
    #   D1 [constant-expression-opt]
    # and the type of the identifier in the declaration T D1 is
    # "type-modifier T", the type of the
    # identifier of D is "type-modifier array of T"
    #
    # This is what this method does. The declarator it receives
    # can be a list of declarators ending with TypeDecl. It
    # tacks the modifier to the end of this list, just before
    # the TypeDecl.
    #
    # Additionally, the modifier may be a list itself. This is
    # useful for pointers, that can come as a chain from the rule
    # p_pointer. In this case, the whole modifier list is spliced
    # into the new location.
    def _type_modify_decl(self, decl, modifier):
        """ Tacks a type modifier on a declarator, and returns
            the modified declarator.

            Note: the declarator and modifier may be modified
        """
        #~ print '****'
        #~ decl.show(offset=3)
        #~ modifier.show(offset=3)
        #~ print '****'

        modifier_head = modifier
        modifier_tail = modifier

        # The modifier may be a nested list. Reach its tail.
        #
        while modifier_tail.type:
            modifier_tail = modifier_tail.type

        # If the decl is a basic type, just tack the modifier onto
        # it
        #
        if isinstance(decl, ast.TypeDecl):
            modifier_tail.type = decl
            return modifier
        else:
            # Otherwise, the decl is a list of modifiers. Reach
            # its tail and splice the modifier onto the tail,
            # pointing to the underlying basic type.
            #
            decl_tail = decl

            while not isinstance(decl_tail.type, ast.TypeDecl):
                decl_tail = decl_tail.type

            modifier_tail.type = decl_tail.type
            decl_tail.type = modifier_head
            return decl

    # Due to the order in which declarators are constructed,
    # they have to be fixed in order to look like a normal AST.
    #
    # When a declaration arrives from syntax construction, it has
    # these problems:
    # * The innermost TypeDecl has no type (because the basic
    #   type is only known at the uppermost declaration level)
    # * The declaration has no variable name, since that is saved
    #   in the innermost TypeDecl
    # * The typename of the declaration is a list of type
    #   specifiers, and not a node. Here, basic identifier types
    #   should be separated from more complex types like enums
    #   and structs.
    #
    # This method fixes these problems.
    #
    def _fix_decl_name_type(self, decl, typename):
        """ Fixes a declaration. Modifies decl.
        """
        # Reach the underlying basic type
        #
        type = decl
        while not isinstance(type, ast.TypeDecl):
            type = type.type

        decl.name = type.declname
        type.quals = decl.quals

        # The typename is a list of types. If any type in this
        # list isn't an IdentifierType, it must be the only
        # type in the list (it's illegal to declare "int enum ..")
        # If all the types are basic, they're collected in the
        # IdentifierType holder.
        #
        for tn in typename:
            if not isinstance(tn, ast.IdentifierType):
                if len(typename) > 1:
                    self._parse_error(
                        "Invalid multiple types specified", tn.coord)
                else:
                    type.type = tn
                    return decl

        if not typename:
            # Functions default to returning int
            #
            if not isinstance(decl.type, ast.FuncDecl):
                self._parse_error(
                        "Missing type in declaration", decl.coord)
            type.type = ast.IdentifierType(
                    ['int'],
                    coord=decl.coord)
        else:
            # At this point, we know that typename is a list of IdentifierType
            # nodes. Concatenate all the names into a single list.
            #
            type.type = ast.IdentifierType(
                [name for id in typename for name in id.names],
                coord=typename[0].coord)
        return decl

    def _add_declaration_specifier(self, declspec, newspec, kind, append=False):
        """ Declaration specifiers are represented by a dictionary
            with the entries:
            * qual: a list of type qualifiers
            * storage: a list of storage type qualifiers
            * type: a list of type specifiers
            * function: a list of function specifiers

            This method is given a declaration specifier, and a
            new specifier of a given kind.
            If `append` is True, the new specifier is added to the end of
            the specifiers list, otherwise it's added at the beginning.
            Returns the declaration specifier, with the new
            specifier incorporated.
        """
        spec = declspec or dict(qual=[], storage=[], type=[], function=[])

        if append:
            spec[kind].append(newspec)
        else:
            spec[kind].insert(0, newspec)

        return spec

    def _build_declarations(self, spec, decls, typedef_namespace=False):
        """ Builds a list of declarations all sharing the given specifiers.
            If typedef_namespace is true, each declared name is added
            to the "typedef namespace", which also includes objects,
            functions, and enum constants.
        """
        is_typedef = 'typedef' in spec['storage']
        declarations = []

        # Bit-fields are allowed to be unnamed.
        #
        if decls[0].get('bitsize') is not None:
            pass

        # When redeclaring typedef names as identifiers in inner scopes, a
        # problem can occur where the identifier gets grouped into
        # spec['type'], leaving decl as None.  This can only occur for the
        # first declarator.
        #
        elif decls[0]['decl'] is None:
            if len(spec['type']) < 2 or len(spec['type'][-1].names) != 1 or \
                    not self._is_type_in_scope(spec['type'][-1].names[0]):
                coord = '?'
                for t in spec['type']:
                    if hasattr(t, 'coord'):
                        coord = t.coord
                        break
                self._parse_error('Invalid declaration', coord)

            # Make this look as if it came from "direct_declarator:ID"
            decls[0]['decl'] = ast.TypeDecl(
                declname=spec['type'][-1].names[0],
                type=None,
                quals=None,
                coord=spec['type'][-1].coord)
            # Remove the "new" type's name from the end of spec['type']
            del spec['type'][-1]

        # A similar problem can occur where the declaration ends up looking
        # like an abstract declarator.  Give it a name if this is the case.
        #
        elif not isinstance(decls[0]['decl'],
                (ast.Struct, ast.Union, ast.IdentifierType)):
            decls_0_tail = decls[0]['decl']
            while not isinstance(decls_0_tail, ast.TypeDecl):
                decls_0_tail = decls_0_tail.type
            if decls_0_tail.declname is None:
                decls_0_tail.declname = spec['type'][-1].names[0]
                del spec['type'][-1]

        for decl in decls:
            assert decl['decl'] is not None
            if is_typedef:
                declaration = ast.Typedef(
                    name=None,
                    quals=spec['qual'],
                    storage=spec['storage'],
                    type=decl['decl'],
                    coord=decl['decl'].coord)
            else:
                declaration = ast.Decl(
                    name=None,
                    quals=spec['qual'],
                    storage=spec['storage'],
                    funcspec=spec['function'],
                    type=decl['decl'],
                    init=decl.get('init'),
                    bitsize=decl.get('bitsize'),
                    coord=decl['decl'].coord)

            if isinstance(declaration.type,
                    (ast.Struct, ast.Union, ast.IdentifierType)):
                fixed_decl = declaration
            else:
                fixed_decl = self._fix_decl_name_type(declaration, spec['type'])

            # Add the type name defined by typedef to a
            # symbol table (for usage in the lexer)
            #
            if typedef_namespace:
                if is_typedef:
                    self._add_typedef_name(fixed_decl.name, fixed_decl.coord)
                else:
                    self._add_identifier(fixed_decl.name, fixed_decl.coord)

            declarations.append(fixed_decl)

        return declarations

    def _build_function_definition(self, spec, decl, param_decls, body):
        """ Builds a function definition.
        """
        assert 'typedef' not in spec['storage']

        declaration = self._build_declarations(
            spec=spec,
            decls=[dict(decl=decl, init=None)],
            typedef_namespace=True)[0]

        return ast.FuncDef(
            decl=declaration,
            param_decls=param_decls,
            body=body,
            coord=decl.coord)

    def _select_struct_union_class(self, token):
        """ Given a token (either STRUCT or UNION), selects the
            appropriate AST class.
        """
        if token == 'struct':
            return ast.Struct
        else:
            return ast.Union

    ##
    ## Precedence and associativity of operators
    ##
    precedence = (
        ('left', 'LOR'),
        ('left', 'LAND'),
        ('left', 'OR'),
        ('left', 'XOR'),
        ('left', 'AND'),
        ('left', 'EQ', 'NE'),
        ('left', 'GT', 'GE', 'LT', 'LE'),
        ('left', 'RSHIFT', 'LSHIFT'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE', 'MOD')
    )

    ##
    ## Grammar productions
    ## Implementation of the BNF defined in K&R2 A.13
    ##

    # Wrapper around a translation unit, to allow for empty input.
    # Not strictly part of the C99 Grammar, but useful in practice.
    #
    def p_translation_unit_or_empty(self, p):
        """ translation_unit_or_empty   : translation_unit
                                        | empty
        """
        if p[1] is None:
            p[0] = ast.FileAST([])
        else:
            p[0] = ast.FileAST(p[1])

    def p_translation_unit_1(self, p):
        """ translation_unit    : external_declaration
        """
        # Note: external_declaration is already a list
        #
        p[0] = p[1]

    def p_translation_unit_2(self, p):
        """ translation_unit    : translation_unit external_declaration
        """
        p[1].extend(p[2])
        p[0] = p[1]

    # Declarations always come as lists (because they can be
    # several in one line), so we wrap the function definition
    # into a list as well, to make the return value of
    # external_declaration homogenous.
    #
    def p_external_declaration_1(self, p):
        """ external_declaration    : function_definition
        """
        p[0] = [p[1]]

    def p_external_declaration_2(self, p):
        """ external_declaration    : declaration
        """
        p[0] = p[1]

    def p_external_declaration_3(self, p):
        """ external_declaration    : pp_directive
                                    | pppragma_directive
        """
        p[0] = [p[1]]

    def p_external_declaration_4(self, p):
        """ external_declaration    : SEMI
        """
        p[0] = []

    def p_pp_directive(self, p):
        """ pp_directive  : PPHASH
        """
        self._parse_error('Directives not supported yet',
                          self._token_coord(p, 1))

    def p_pppragma_directive(self, p):
        """ pppragma_directive      : PPPRAGMA
                                    | PPPRAGMA PPPRAGMASTR
        """
        if len(p) == 3:
            p[0] = ast.Pragma(p[2], self._token_coord(p, 2))
        else:
            p[0] = ast.Pragma("", self._token_coord(p, 1))

    # In function definitions, the declarator can be followed by
    # a declaration list, for old "K&R style" function definitios.
    #
    def p_function_definition_1(self, p):
        """ function_definition : id_declarator declaration_list_opt compound_statement
        """
        # no declaration specifiers - 'int' becomes the default type
        spec = dict(
            qual=[],
            storage=[],
            type=[ast.IdentifierType(['int'],
                                       coord=self._token_coord(p, 1))],
            function=[])

        p[0] = self._build_function_definition(
            spec=spec,
            decl=p[1],
            param_decls=p[2],
            body=p[3])

    def p_function_definition_2(self, p):
        """ function_definition : declaration_specifiers id_declarator declaration_list_opt compound_statement
        """
        spec = p[1]

        p[0] = self._build_function_definition(
            spec=spec,
            decl=p[2],
            param_decls=p[3],
            body=p[4])

    def p_statement(self, p):
        """ statement   : labeled_statement
                        | expression_statement
                        | compound_statement
                        | selection_statement
                        | iteration_statement
                        | jump_statement
                        | pppragma_directive
        """
        p[0] = p[1]

    # A pragma is generally considered a decorator rather than an actual statement.
    # Still, for the purposes of analyzing an abstract syntax tree of C code,
    # pragma's should not be ignored and were previously treated as a statement.
    # This presents a problem for constructs that take a statement such as labeled_statements,
    # selection_statements, and iteration_statements, causing a misleading structure
    # in the AST. For example, consider the following C code.
    #
    #   for (int i = 0; i < 3; i++)
    #       #pragma omp critical
    #       sum += 1;
    #
    # This code will compile and execute "sum += 1;" as the body of the for loop.
    # Previous implementations of PyCParser would render the AST for this
    # block of code as follows:
    #
    #   For:
    #     DeclList:
    #       Decl: i, [], [], []
    #         TypeDecl: i, []
    #           IdentifierType: ['int']
    #         Constant: int, 0
    #     BinaryOp: <
    #       ID: i
    #       Constant: int, 3
    #     UnaryOp: p++
    #       ID: i
    #     Pragma: omp critical
    #   Assignment: +=
    #     ID: sum
    #     Constant: int, 1
    #
    # This AST misleadingly takes the Pragma as the body of the loop and the
    # assignment then becomes a sibling of the loop.
    #
    # To solve edge cases like these, the pragmacomp_or_statement rule groups
    # a pragma and its following statement (which would otherwise be orphaned)
    # using a compound block, effectively turning the above code into:
    #
    #   for (int i = 0; i < 3; i++) {
    #       #pragma omp critical
    #       sum += 1;
    #   }
    def p_pragmacomp_or_statement(self, p):
        """ pragmacomp_or_statement     : pppragma_directive statement
                                        | statement
        """
        if isinstance(p[1], ast.Pragma) and len(p) == 3:
            p[0] = ast.Compound(
                block_items=[p[1], p[2]],
                coord=self._token_coord(p, 1))
        else:
            p[0] = p[1]

    # In C, declarations can come several in a line:
    #   int x, *px, romulo = 5;
    #
    # However, for the AST, we will split them to separate Decl
    # nodes.
    #
    # This rule splits its declarations and always returns a list
    # of Decl nodes, even if it's one element long.
    #
    def p_decl_body(self, p):
        """ decl_body : declaration_specifiers init_declarator_list_opt
                      | declaration_specifiers_no_type id_init_declarator_list_opt
        """
        spec = p[1]

        # p[2] (init_declarator_list_opt) is either a list or None
        #
        if p[2] is None:
            # By the standard, you must have at least one declarator unless
            # declaring a structure tag, a union tag, or the members of an
            # enumeration.
            #
            ty = spec['type']
            s_u_or_e = (ast.Struct, ast.Union, ast.Enum)
            if len(ty) == 1 and isinstance(ty[0], s_u_or_e):
                decls = [ast.Decl(
                    name=None,
                    quals=spec['qual'],
                    storage=spec['storage'],
                    funcspec=spec['function'],
                    type=ty[0],
                    init=None,
                    bitsize=None,
                    coord=ty[0].coord)]

            # However, this case can also occur on redeclared identifiers in
            # an inner scope.  The trouble is that the redeclared type's name
            # gets grouped into declaration_specifiers; _build_declarations
            # compensates for this.
            #
            else:
                decls = self._build_declarations(
                    spec=spec,
                    decls=[dict(decl=None, init=None)],
                    typedef_namespace=True)

        else:
            decls = self._build_declarations(
                spec=spec,
                decls=p[2],
                typedef_namespace=True)

        p[0] = decls

    # The declaration has been split to a decl_body sub-rule and
    # SEMI, because having them in a single rule created a problem
    # for defining typedefs.
    #
    # If a typedef line was directly followed by a line using the
    # type defined with the typedef, the type would not be
    # recognized. This is because to reduce the declaration rule,
    # the parser's lookahead asked for the token after SEMI, which
    # was the type from the next line, and the lexer had no chance
    # to see the updated type symbol table.
    #
    # Splitting solves this problem, because after seeing SEMI,
    # the parser reduces decl_body, which actually adds the new
    # type into the table to be seen by the lexer before the next
    # line is reached.
    def p_declaration(self, p):
        """ declaration : decl_body SEMI
        """
        p[0] = p[1]

    # Since each declaration is a list of declarations, this
    # rule will combine all the declarations and return a single
    # list
    #
    def p_declaration_list(self, p):
        """ declaration_list    : declaration
                                | declaration_list declaration
        """
        p[0] = p[1] if len(p) == 2 else p[1] + p[2]

    # To know when declaration-specifiers end and declarators begin,
    # we require declaration-specifiers to have at least one
    # type-specifier, and disallow typedef-names after we've seen any
    # type-specifier. These are both required by the spec.
    #
    def p_declaration_specifiers_no_type_1(self, p):
        """ declaration_specifiers_no_type  : type_qualifier declaration_specifiers_no_type_opt
        """
        p[0] = self._add_declaration_specifier(p[2], p[1], 'qual')

    def p_declaration_specifiers_no_type_2(self, p):
        """ declaration_specifiers_no_type  : storage_class_specifier declaration_specifiers_no_type_opt
        """
        p[0] = self._add_declaration_specifier(p[2], p[1], 'storage')

    def p_declaration_specifiers_no_type_3(self, p):
        """ declaration_specifiers_no_type  : function_specifier declaration_specifiers_no_type_opt
        """
        p[0] = self._add_declaration_specifier(p[2], p[1], 'function')


    def p_declaration_specifiers_1(self, p):
        """ declaration_specifiers  : declaration_specifiers type_qualifier
        """
        p[0] = self._add_declaration_specifier(p[1], p[2], 'qual', append=True)

    def p_declaration_specifiers_2(self, p):
        """ declaration_specifiers  : declaration_specifiers storage_class_specifier
        """
        p[0] = self._add_declaration_specifier(p[1], p[2], 'storage', append=True)

    def p_declaration_specifiers_3(self, p):
        """ declaration_specifiers  : declaration_specifiers function_specifier
        """
        p[0] = self._add_declaration_specifier(p[1], p[2], 'function', append=True)

    def p_declaration_specifiers_4(self, p):
        """ declaration_specifiers  : declaration_specifiers type_specifier_no_typeid
        """
        p[0] = self._add_declaration_specifier(p[1], p[2], 'type', append=True)

    def p_declaration_specifiers_5(self, p):
        """ declaration_specifiers  : type_specifier
        """
        p[0] = self._add_declaration_specifier(None, p[1], 'type')

    def p_declaration_specifiers_6(self, p):
        """ declaration_specifiers  : declaration_specifiers_no_type type_specifier
        """
        p[0] = self._add_declaration_specifier(p[1], p[2], 'type', append=True)


    def p_storage_class_specifier(self, p):
        """ storage_class_specifier : AUTO
                                    | REGISTER
                                    | STATIC
                                    | EXTERN
                                    | TYPEDEF
        """
        p[0] = p[1]

    def p_function_specifier(self, p):
        """ function_specifier  : INLINE
        """
        p[0] = p[1]

    def p_type_specifier_no_typeid(self, p):
        """ type_specifier_no_typeid  : VOID
                                      | _BOOL
                                      | CHAR
                                      | SHORT
                                      | INT
                                      | LONG
                                      | FLOAT
                                      | DOUBLE
                                      | _COMPLEX
                                      | SIGNED
                                      | UNSIGNED
                                      | __INT128
        """
        p[0] = ast.IdentifierType([p[1]], coord=self._token_coord(p, 1))

    def p_type_specifier(self, p):
        """ type_specifier  : typedef_name
                            | enum_specifier
                            | struct_or_union_specifier
                            | type_specifier_no_typeid
        """
        p[0] = p[1]

    def p_type_qualifier(self, p):
        """ type_qualifier  : CONST
                            | RESTRICT
                            | VOLATILE
        """
        p[0] = p[1]

    def p_init_declarator_list(self, p):
        """ init_declarator_list    : init_declarator
                                    | init_declarator_list COMMA init_declarator
        """
        p[0] = p[1] + [p[3]] if len(p) == 4 else [p[1]]

    # Returns a {decl=<declarator> : init=<initializer>} dictionary
    # If there's no initializer, uses None
    #
    def p_init_declarator(self, p):
        """ init_declarator : declarator
                            | declarator EQUALS initializer
        """
        p[0] = dict(decl=p[1], init=(p[3] if len(p) > 2 else None))

    def p_id_init_declarator_list(self, p):
        """ id_init_declarator_list    : id_init_declarator
                                       | id_init_declarator_list COMMA init_declarator
        """
        p[0] = p[1] + [p[3]] if len(p) == 4 else [p[1]]

    def p_id_init_declarator(self, p):
        """ id_init_declarator : id_declarator
                               | id_declarator EQUALS initializer
        """
        p[0] = dict(decl=p[1], init=(p[3] if len(p) > 2 else None))

    # Require at least one type specifier in a specifier-qualifier-list
    #
    def p_specifier_qualifier_list_1(self, p):
        """ specifier_qualifier_list    : specifier_qualifier_list type_specifier_no_typeid
        """
        p[0] = self._add_declaration_specifier(p[1], p[2], 'type', append=True)

    def p_specifier_qualifier_list_2(self, p):
        """ specifier_qualifier_list    : specifier_qualifier_list type_qualifier
        """
        p[0] = self._add_declaration_specifier(p[1], p[2], 'qual', append=True)

    def p_specifier_qualifier_list_3(self, p):
        """ specifier_qualifier_list  : type_specifier
        """
        p[0] = self._add_declaration_specifier(None, p[1], 'type')

    def p_specifier_qualifier_list_4(self, p):
        """ specifier_qualifier_list  : type_qualifier_list type_specifier
        """
        spec = dict(qual=p[1], storage=[], type=[], function=[])
        p[0] = self._add_declaration_specifier(spec, p[2], 'type', append=True)

    # TYPEID is allowed here (and in other struct/enum related tag names), because
    # struct/enum tags reside in their own namespace and can be named the same as types
    #
    def p_struct_or_union_specifier_1(self, p):
        """ struct_or_union_specifier   : struct_or_union ID
                                        | struct_or_union TYPEID
        """
        klass = self._select_struct_union_class(p[1])
        # None means no list of members
        p[0] = klass(
            name=p[2],
            decls=None,
            coord=self._token_coord(p, 2))

    def p_struct_or_union_specifier_2(self, p):
        """ struct_or_union_specifier : struct_or_union brace_open struct_declaration_list brace_close
                                      | struct_or_union brace_open brace_close
        """
        klass = self._select_struct_union_class(p[1])
        if len(p) == 4:
            # Empty sequence means an empty list of members
            p[0] = klass(
                name=None,
                decls=[],
                coord=self._token_coord(p, 2))
        else:
            p[0] = klass(
                name=None,
                decls=p[3],
                coord=self._token_coord(p, 2))


    def p_struct_or_union_specifier_3(self, p):
        """ struct_or_union_specifier   : struct_or_union ID brace_open struct_declaration_list brace_close
                                        | struct_or_union ID brace_open brace_close
                                        | struct_or_union TYPEID brace_open struct_declaration_list brace_close
                                        | struct_or_union TYPEID brace_open brace_close
        """
        klass = self._select_struct_union_class(p[1])
        if len(p) == 5:
            # Empty sequence means an empty list of members
            p[0] = klass(
                name=p[2],
                decls=[],
                coord=self._token_coord(p, 2))
        else:
            p[0] = klass(
                name=p[2],
                decls=p[4],
                coord=self._token_coord(p, 2))

    def p_struct_or_union(self, p):
        """ struct_or_union : STRUCT
                            | UNION
        """
        p[0] = p[1]

    # Combine all declarations into a single list
    #
    def p_struct_declaration_list(self, p):
        """ struct_declaration_list     : struct_declaration
                                        | struct_declaration_list struct_declaration
        """
        if len(p) == 2:
            p[0] = p[1] or []
        else:
            p[0] = p[1] + (p[2] or [])

    def p_struct_declaration_1(self, p):
        """ struct_declaration : specifier_qualifier_list struct_declarator_list_opt SEMI
        """
        spec = p[1]
        assert 'typedef' not in spec['storage']

        if p[2] is not None:
            decls = self._build_declarations(
                spec=spec,
                decls=p[2])

        elif len(spec['type']) == 1:
            # Anonymous struct/union, gcc extension, C1x feature.
            # Although the standard only allows structs/unions here, I see no
            # reason to disallow other types since some compilers have typedefs
            # here, and pycparser isn't about rejecting all invalid code.
            #
            node = spec['type'][0]
            if isinstance(node, ast.Node):
                decl_type = node
            else:
                decl_type = ast.IdentifierType(node)

            decls = self._build_declarations(
                spec=spec,
                decls=[dict(decl=decl_type)])

        else:
            # Structure/union members can have the same names as typedefs.
            # The trouble is that the member's name gets grouped into
            # specifier_qualifier_list; _build_declarations compensates.
            #
            decls = self._build_declarations(
                spec=spec,
                decls=[dict(decl=None, init=None)])

        p[0] = decls

    def p_struct_declaration_2(self, p):
        """ struct_declaration : SEMI
        """
        p[0] = None

    def p_struct_declaration_3(self, p):
        """ struct_declaration : pppragma_directive
        """
        p[0] = [p[1]]

    def p_struct_declarator_list(self, p):
        """ struct_declarator_list  : struct_declarator
                                    | struct_declarator_list COMMA struct_declarator
        """
        p[0] = p[1] + [p[3]] if len(p) == 4 else [p[1]]

    # struct_declarator passes up a dict with the keys: decl (for
    # the underlying declarator) and bitsize (for the bitsize)
    #
    def p_struct_declarator_1(self, p):
        """ struct_declarator : declarator
        """
        p[0] = {'decl': p[1], 'bitsize': None}

    def p_struct_declarator_2(self, p):
        """ struct_declarator   : declarator COLON constant_expression
                                | COLON constant_expression
        """
        if len(p) > 3:
            p[0] = {'decl': p[1], 'bitsize': p[3]}
        else:
            p[0] = {'decl': ast.TypeDecl(None, None, None), 'bitsize': p[2]}

    def p_enum_specifier_1(self, p):
        """ enum_specifier  : ENUM ID
                            | ENUM TYPEID
        """
        p[0] = ast.Enum(p[2], None, self._token_coord(p, 1))

    def p_enum_specifier_2(self, p):
        """ enum_specifier  : ENUM brace_open enumerator_list brace_close
        """
        p[0] = ast.Enum(None, p[3], self._token_coord(p, 1))

    def p_enum_specifier_3(self, p):
        """ enum_specifier  : ENUM ID brace_open enumerator_list brace_close
                            | ENUM TYPEID brace_open enumerator_list brace_close
        """
        p[0] = ast.Enum(p[2], p[4], self._token_coord(p, 1))

    def p_enumerator_list(self, p):
        """ enumerator_list : enumerator
                            | enumerator_list COMMA
                            | enumerator_list COMMA enumerator
        """
        if len(p) == 2:
            p[0] = ast.EnumeratorList([p[1]], p[1].coord)
        elif len(p) == 3:
            p[0] = p[1]
        else:
            p[1].enumerators.append(p[3])
            p[0] = p[1]

    def p_enumerator(self, p):
        """ enumerator  : ID
                        | ID EQUALS constant_expression
        """
        if len(p) == 2:
            enumerator = ast.Enumerator(
                        p[1], None,
                        self._token_coord(p, 1))
        else:
            enumerator = ast.Enumerator(
                        p[1], p[3],
                        self._token_coord(p, 1))
        self._add_identifier(enumerator.name, enumerator.coord)

        p[0] = enumerator

    def p_declarator(self, p):
        """ declarator  : id_declarator
                        | typeid_declarator
        """
        p[0] = p[1]

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_xxx_declarator_1(self, p):
        """ xxx_declarator  : direct_xxx_declarator
        """
        p[0] = p[1]

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_xxx_declarator_2(self, p):
        """ xxx_declarator  : pointer direct_xxx_declarator
        """
        p[0] = self._type_modify_decl(p[2], p[1])

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_1(self, p):
        """ direct_xxx_declarator   : yyy
        """
        p[0] = ast.TypeDecl(
            declname=p[1],
            type=None,
            quals=None,
            coord=self._token_coord(p, 1))

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'))
    def p_direct_xxx_declarator_2(self, p):
        """ direct_xxx_declarator   : LPAREN xxx_declarator RPAREN
        """
        p[0] = p[2]

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_3(self, p):
        """ direct_xxx_declarator   : direct_xxx_declarator LBRACKET type_qualifier_list_opt assignment_expression_opt RBRACKET
        """
        quals = (p[3] if len(p) > 5 else []) or []
        # Accept dimension qualifiers
        # Per C99 6.7.5.3 p7
        arr = ast.ArrayDecl(
            type=None,
            dim=p[4] if len(p) > 5 else p[3],
            dim_quals=quals,
            coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_4(self, p):
        """ direct_xxx_declarator   : direct_xxx_declarator LBRACKET STATIC type_qualifier_list_opt assignment_expression RBRACKET
                                    | direct_xxx_declarator LBRACKET type_qualifier_list STATIC assignment_expression RBRACKET
        """
        # Using slice notation for PLY objects doesn't work in Python 3 for the
        # version of PLY embedded with pycparser; see PLY Google Code issue 30.
        # Work around that here by listing the two elements separately.
        listed_quals = [item if isinstance(item, list) else [item]
            for item in [p[3],p[4]]]
        dim_quals = [qual for sublist in listed_quals for qual in sublist
            if qual is not None]
        arr = ast.ArrayDecl(
            type=None,
            dim=p[5],
            dim_quals=dim_quals,
            coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    # Special for VLAs
    #
    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_5(self, p):
        """ direct_xxx_declarator   : direct_xxx_declarator LBRACKET type_qualifier_list_opt TIMES RBRACKET
        """
        arr = ast.ArrayDecl(
            type=None,
            dim=ast.ID(p[4], self._token_coord(p, 4)),
            dim_quals=p[3] if p[3] != None else [],
            coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_6(self, p):
        """ direct_xxx_declarator   : direct_xxx_declarator LPAREN parameter_type_list RPAREN
                                    | direct_xxx_declarator LPAREN identifier_list_opt RPAREN
        """
        func = ast.FuncDecl(
            args=p[3],
            type=None,
            coord=p[1].coord)

        # To see why _get_yacc_lookahead_token is needed, consider:
        #   typedef char TT;
        #   void foo(int TT) { TT = 10; }
        # Outside the function, TT is a typedef, but inside (starting and
        # ending with the braces) it's a parameter.  The trouble begins with
        # yacc's lookahead token.  We don't know if we're declaring or
        # defining a function until we see LBRACE, but if we wait for yacc to
        # trigger a rule on that token, then TT will have already been read
        # and incorrectly interpreted as TYPEID.  We need to add the
        # parameters to the scope the moment the lexer sees LBRACE.
        #
        if self._get_yacc_lookahead_token().type == "LBRACE":
            if func.args is not None:
                for param in func.args.params:
                    if isinstance(param, ast.EllipsisParam): break
                    self._add_identifier(param.name, param.coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=func)

    def p_pointer(self, p):
        """ pointer : TIMES type_qualifier_list_opt
                    | TIMES type_qualifier_list_opt pointer
        """
        coord = self._token_coord(p, 1)
        # Pointer decls nest from inside out. This is important when different
        # levels have different qualifiers. For example:
        #
        #  char * const * p;
        #
        # Means "pointer to const pointer to char"
        #
        # While:
        #
        #  char ** const p;
        #
        # Means "const pointer to pointer to char"
        #
        # So when we construct PtrDecl nestings, the leftmost pointer goes in
        # as the most nested type.
        nested_type = ast.PtrDecl(quals=p[2] or [], type=None, coord=coord)
        if len(p) > 3:
            tail_type = p[3]
            while tail_type.type is not None:
                tail_type = tail_type.type
            tail_type.type = nested_type
            p[0] = p[3]
        else:
            p[0] = nested_type

    def p_type_qualifier_list(self, p):
        """ type_qualifier_list : type_qualifier
                                | type_qualifier_list type_qualifier
        """
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

    def p_parameter_type_list(self, p):
        """ parameter_type_list : parameter_list
                                | parameter_list COMMA ELLIPSIS
        """
        if len(p) > 2:
            p[1].params.append(ast.EllipsisParam(self._token_coord(p, 3)))

        p[0] = p[1]

    def p_parameter_list(self, p):
        """ parameter_list  : parameter_declaration
                            | parameter_list COMMA parameter_declaration
        """
        if len(p) == 2: # single parameter
            p[0] = ast.ParamList([p[1]], p[1].coord)
        else:
            p[1].params.append(p[3])
            p[0] = p[1]

    # From ISO/IEC 9899:TC2, 6.7.5.3.11:
    # "If, in a parameter declaration, an identifier can be treated either
    #  as a typedef name or as a parameter name, it shall be taken as a
    #  typedef name."
    #
    # Inside a parameter declaration, once we've reduced declaration specifiers,
    # if we shift in an LPAREN and see a TYPEID, it could be either an abstract
    # declarator or a declarator nested inside parens. This rule tells us to
    # always treat it as an abstract declarator. Therefore, we only accept
    # `id_declarator`s and `typeid_noparen_declarator`s.
    def p_parameter_declaration_1(self, p):
        """ parameter_declaration   : declaration_specifiers id_declarator
                                    | declaration_specifiers typeid_noparen_declarator
        """
        spec = p[1]
        if not spec['type']:
            spec['type'] = [ast.IdentifierType(['int'],
                coord=self._token_coord(p, 1))]
        p[0] = self._build_declarations(
            spec=spec,
            decls=[dict(decl=p[2])])[0]

    def p_parameter_declaration_2(self, p):
        """ parameter_declaration   : declaration_specifiers abstract_declarator_opt
        """
        spec = p[1]
        if not spec['type']:
            spec['type'] = [ast.IdentifierType(['int'],
                coord=self._token_coord(p, 1))]

        # Parameters can have the same names as typedefs.  The trouble is that
        # the parameter's name gets grouped into declaration_specifiers, making
        # it look like an old-style declaration; compensate.
        #
        if len(spec['type']) > 1 and len(spec['type'][-1].names) == 1 and \
                self._is_type_in_scope(spec['type'][-1].names[0]):
            decl = self._build_declarations(
                    spec=spec,
                    decls=[dict(decl=p[2], init=None)])[0]

        # This truly is an old-style parameter declaration
        #
        else:
            decl = ast.Typename(
                name='',
                quals=spec['qual'],
                type=p[2] or ast.TypeDecl(None, None, None),
                coord=self._token_coord(p, 2))
            typename = spec['type']
            decl = self._fix_decl_name_type(decl, typename)

        p[0] = decl

    def p_identifier_list(self, p):
        """ identifier_list : identifier
                            | identifier_list COMMA identifier
        """
        if len(p) == 2: # single parameter
            p[0] = ast.ParamList([p[1]], p[1].coord)
        else:
            p[1].params.append(p[3])
            p[0] = p[1]

    def p_initializer_1(self, p):
        """ initializer : assignment_expression
        """
        p[0] = p[1]

    def p_initializer_2(self, p):
        """ initializer : brace_open initializer_list_opt brace_close
                        | brace_open initializer_list COMMA brace_close
        """
        if p[2] is None:
            p[0] = ast.InitList([], self._token_coord(p, 1))
        else:
            p[0] = p[2]

    def p_initializer_list(self, p):
        """ initializer_list    : designation_opt initializer
                                | initializer_list COMMA designation_opt initializer
        """
        if len(p) == 3: # single initializer
            init = p[2] if p[1] is None else ast.NamedInitializer(p[1], p[2])
            p[0] = ast.InitList([init], p[2].coord)
        else:
            init = p[4] if p[3] is None else ast.NamedInitializer(p[3], p[4])
            p[1].exprs.append(init)
            p[0] = p[1]

    def p_designation(self, p):
        """ designation : designator_list EQUALS
        """
        p[0] = p[1]

    # Designators are represented as a list of nodes, in the order in which
    # they're written in the code.
    #
    def p_designator_list(self, p):
        """ designator_list : designator
                            | designator_list designator
        """
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

    def p_designator(self, p):
        """ designator  : LBRACKET constant_expression RBRACKET
                        | PERIOD identifier
        """
        p[0] = p[2]

    def p_type_name(self, p):
        """ type_name   : specifier_qualifier_list abstract_declarator_opt
        """
        typename = ast.Typename(
            name='',
            quals=p[1]['qual'],
            type=p[2] or ast.TypeDecl(None, None, None),
            coord=self._token_coord(p, 2))

        p[0] = self._fix_decl_name_type(typename, p[1]['type'])

    def p_abstract_declarator_1(self, p):
        """ abstract_declarator     : pointer
        """
        dummytype = ast.TypeDecl(None, None, None)
        p[0] = self._type_modify_decl(
            decl=dummytype,
            modifier=p[1])

    def p_abstract_declarator_2(self, p):
        """ abstract_declarator     : pointer direct_abstract_declarator
        """
        p[0] = self._type_modify_decl(p[2], p[1])

    def p_abstract_declarator_3(self, p):
        """ abstract_declarator     : direct_abstract_declarator
        """
        p[0] = p[1]

    # Creating and using direct_abstract_declarator_opt here
    # instead of listing both direct_abstract_declarator and the
    # lack of it in the beginning of _1 and _2 caused two
    # shift/reduce errors.
    #
    def p_direct_abstract_declarator_1(self, p):
        """ direct_abstract_declarator  : LPAREN abstract_declarator RPAREN """
        p[0] = p[2]

    def p_direct_abstract_declarator_2(self, p):
        """ direct_abstract_declarator  : direct_abstract_declarator LBRACKET assignment_expression_opt RBRACKET
        """
        arr = ast.ArrayDecl(
            type=None,
            dim=p[3],
            dim_quals=[],
            coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    def p_direct_abstract_declarator_3(self, p):
        """ direct_abstract_declarator  : LBRACKET assignment_expression_opt RBRACKET
        """
        p[0] = ast.ArrayDecl(
            type=ast.TypeDecl(None, None, None),
            dim=p[2],
            dim_quals=[],
            coord=self._token_coord(p, 1))

    def p_direct_abstract_declarator_4(self, p):
        """ direct_abstract_declarator  : direct_abstract_declarator LBRACKET TIMES RBRACKET
        """
        arr = ast.ArrayDecl(
            type=None,
            dim=ast.ID(p[3], self._token_coord(p, 3)),
            dim_quals=[],
            coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    def p_direct_abstract_declarator_5(self, p):
        """ direct_abstract_declarator  : LBRACKET TIMES RBRACKET
        """
        p[0] = ast.ArrayDecl(
            type=ast.TypeDecl(None, None, None),
            dim=ast.ID(p[3], self._token_coord(p, 3)),
            dim_quals=[],
            coord=self._token_coord(p, 1))

    def p_direct_abstract_declarator_6(self, p):
        """ direct_abstract_declarator  : direct_abstract_declarator LPAREN parameter_type_list_opt RPAREN
        """
        func = ast.FuncDecl(
            args=p[3],
            type=None,
            coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=func)

    def p_direct_abstract_declarator_7(self, p):
        """ direct_abstract_declarator  : LPAREN parameter_type_list_opt RPAREN
        """
        p[0] = ast.FuncDecl(
            args=p[2],
            type=ast.TypeDecl(None, None, None),
            coord=self._token_coord(p, 1))

    # declaration is a list, statement isn't. To make it consistent, block_item
    # will always be a list
    #
    def p_block_item(self, p):
        """ block_item  : declaration
                        | statement
        """
        p[0] = p[1] if isinstance(p[1], list) else [p[1]]

    # Since we made block_item a list, this just combines lists
    #
    def p_block_item_list(self, p):
        """ block_item_list : block_item
                            | block_item_list block_item
        """
        # Empty block items (plain ';') produce [None], so ignore them
        p[0] = p[1] if (len(p) == 2 or p[2] == [None]) else p[1] + p[2]

    def p_compound_statement_1(self, p):
        """ compound_statement : brace_open block_item_list_opt brace_close """
        p[0] = ast.Compound(
            block_items=p[2],
            coord=self._token_coord(p, 1))

    def p_labeled_statement_1(self, p):
        """ labeled_statement : ID COLON pragmacomp_or_statement """
        p[0] = ast.Label(p[1], p[3], self._token_coord(p, 1))

    def p_labeled_statement_2(self, p):
        """ labeled_statement : CASE constant_expression COLON pragmacomp_or_statement """
        p[0] = ast.Case(p[2], [p[4]], self._token_coord(p, 1))

    def p_labeled_statement_3(self, p):
        """ labeled_statement : DEFAULT COLON pragmacomp_or_statement """
        p[0] = ast.Default([p[3]], self._token_coord(p, 1))

    def p_selection_statement_1(self, p):
        """ selection_statement : IF LPAREN expression RPAREN pragmacomp_or_statement """
        p[0] = ast.If(p[3], p[5], None, self._token_coord(p, 1))

    def p_selection_statement_2(self, p):
        """ selection_statement : IF LPAREN expression RPAREN statement ELSE pragmacomp_or_statement """
        p[0] = ast.If(p[3], p[5], p[7], self._token_coord(p, 1))

    def p_selection_statement_3(self, p):
        """ selection_statement : SWITCH LPAREN expression RPAREN pragmacomp_or_statement """
        p[0] = fix_switch_cases(
                ast.Switch(p[3], p[5], self._token_coord(p, 1)))

    def p_iteration_statement_1(self, p):
        """ iteration_statement : WHILE LPAREN expression RPAREN pragmacomp_or_statement """
        p[0] = ast.While(p[3], p[5], self._token_coord(p, 1))

    def p_iteration_statement_2(self, p):
        """ iteration_statement : DO pragmacomp_or_statement WHILE LPAREN expression RPAREN SEMI """
        p[0] = ast.DoWhile(p[5], p[2], self._token_coord(p, 1))

    def p_iteration_statement_3(self, p):
        """ iteration_statement : FOR LPAREN expression_opt SEMI expression_opt SEMI expression_opt RPAREN pragmacomp_or_statement """
        p[0] = ast.For(p[3], p[5], p[7], p[9], self._token_coord(p, 1))

    def p_iteration_statement_4(self, p):
        """ iteration_statement : FOR LPAREN declaration expression_opt SEMI expression_opt RPAREN pragmacomp_or_statement """
        p[0] = ast.For(ast.DeclList(p[3], self._token_coord(p, 1)),
                         p[4], p[6], p[8], self._token_coord(p, 1))

    def p_jump_statement_1(self, p):
        """ jump_statement  : GOTO ID SEMI """
        p[0] = ast.Goto(p[2], self._token_coord(p, 1))

    def p_jump_statement_2(self, p):
        """ jump_statement  : BREAK SEMI """
        p[0] = ast.Break(self._token_coord(p, 1))

    def p_jump_statement_3(self, p):
        """ jump_statement  : CONTINUE SEMI """
        p[0] = ast.Continue(self._token_coord(p, 1))

    def p_jump_statement_4(self, p):
        """ jump_statement  : RETURN expression SEMI
                            | RETURN SEMI
        """
        p[0] = ast.Return(p[2] if len(p) == 4 else None, self._token_coord(p, 1))

    def p_expression_statement(self, p):
        """ expression_statement : expression_opt SEMI """
        if p[1] is None:
            p[0] = ast.EmptyStatement(self._token_coord(p, 2))
        else:
            p[0] = p[1]

    def p_expression(self, p):
        """ expression  : assignment_expression
                        | expression COMMA assignment_expression
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            if not isinstance(p[1], ast.ExprList):
                p[1] = ast.ExprList([p[1]], p[1].coord)

            p[1].exprs.append(p[3])
            p[0] = p[1]

    def p_typedef_name(self, p):
        """ typedef_name : TYPEID """
        p[0] = ast.IdentifierType([p[1]], coord=self._token_coord(p, 1))

    def p_assignment_expression(self, p):
        """ assignment_expression   : conditional_expression
                                    | unary_expression assignment_operator assignment_expression
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = ast.Assignment(p[2], p[1], p[3], p[1].coord)

    # K&R2 defines these as many separate rules, to encode
    # precedence and associativity. Why work hard ? I'll just use
    # the built in precedence/associativity specification feature
    # of PLY. (see precedence declaration above)
    #
    def p_assignment_operator(self, p):
        """ assignment_operator : EQUALS
                                | XOREQUAL
                                | TIMESEQUAL
                                | DIVEQUAL
                                | MODEQUAL
                                | PLUSEQUAL
                                | MINUSEQUAL
                                | LSHIFTEQUAL
                                | RSHIFTEQUAL
                                | ANDEQUAL
                                | OREQUAL
        """
        p[0] = p[1]

    def p_constant_expression(self, p):
        """ constant_expression : conditional_expression """
        p[0] = p[1]

    def p_conditional_expression(self, p):
        """ conditional_expression  : binary_expression
                                    | binary_expression CONDOP expression COLON conditional_expression
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = ast.TernaryOp(p[1], p[3], p[5], p[1].coord)

    def p_binary_expression(self, p):
        """ binary_expression   : cast_expression
                                | binary_expression TIMES binary_expression
                                | binary_expression DIVIDE binary_expression
                                | binary_expression MOD binary_expression
                                | binary_expression PLUS binary_expression
                                | binary_expression MINUS binary_expression
                                | binary_expression RSHIFT binary_expression
                                | binary_expression LSHIFT binary_expression
                                | binary_expression LT binary_expression
                                | binary_expression LE binary_expression
                                | binary_expression GE binary_expression
                                | binary_expression GT binary_expression
                                | binary_expression EQ binary_expression
                                | binary_expression NE binary_expression
                                | binary_expression AND binary_expression
                                | binary_expression OR binary_expression
                                | binary_expression XOR binary_expression
                                | binary_expression LAND binary_expression
                                | binary_expression LOR binary_expression
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = ast.BinaryOp(p[2], p[1], p[3], p[1].coord)

    def p_cast_expression_1(self, p):
        """ cast_expression : unary_expression """
        p[0] = p[1]

    def p_cast_expression_2(self, p):
        """ cast_expression : LPAREN type_name RPAREN cast_expression """
        p[0] = ast.Cast(p[2], p[4], self._token_coord(p, 1))

    def p_unary_expression_1(self, p):
        """ unary_expression    : postfix_expression """
        p[0] = p[1]

    def p_unary_expression_2(self, p):
        """ unary_expression    : PLUSPLUS unary_expression
                                | MINUSMINUS unary_expression
                                | unary_operator cast_expression
        """
        p[0] = ast.UnaryOp(p[1], p[2], p[2].coord)

    def p_unary_expression_3(self, p):
        """ unary_expression    : SIZEOF unary_expression
                                | SIZEOF LPAREN type_name RPAREN
        """
        p[0] = ast.UnaryOp(
            p[1],
            p[2] if len(p) == 3 else p[3],
            self._token_coord(p, 1))

    def p_unary_operator(self, p):
        """ unary_operator  : AND
                            | TIMES
                            | PLUS
                            | MINUS
                            | NOT
                            | LNOT
        """
        p[0] = p[1]

    def p_postfix_expression_1(self, p):
        """ postfix_expression  : primary_expression """
        p[0] = p[1]

    def p_postfix_expression_2(self, p):
        """ postfix_expression  : postfix_expression LBRACKET expression RBRACKET """
        p[0] = ast.ArrayRef(p[1], p[3], p[1].coord)

    def p_postfix_expression_3(self, p):
        """ postfix_expression  : postfix_expression LPAREN argument_expression_list RPAREN
                                | postfix_expression LPAREN RPAREN
        """
        p[0] = ast.FuncCall(p[1], p[3] if len(p) == 5 else None, p[1].coord)

    def p_postfix_expression_4(self, p):
        """ postfix_expression  : postfix_expression PERIOD ID
                                | postfix_expression PERIOD TYPEID
                                | postfix_expression ARROW ID
                                | postfix_expression ARROW TYPEID
        """
        field = ast.ID(p[3], self._token_coord(p, 3))
        p[0] = ast.StructRef(p[1], p[2], field, p[1].coord)

    def p_postfix_expression_5(self, p):
        """ postfix_expression  : postfix_expression PLUSPLUS
                                | postfix_expression MINUSMINUS
        """
        p[0] = ast.UnaryOp('p' + p[2], p[1], p[1].coord)

    def p_postfix_expression_6(self, p):
        """ postfix_expression  : LPAREN type_name RPAREN brace_open initializer_list brace_close
                                | LPAREN type_name RPAREN brace_open initializer_list COMMA brace_close
        """
        p[0] = ast.CompoundLiteral(p[2], p[5])

    def p_primary_expression_1(self, p):
        """ primary_expression  : identifier """
        p[0] = p[1]

    def p_primary_expression_2(self, p):
        """ primary_expression  : constant """
        p[0] = p[1]

    def p_primary_expression_3(self, p):
        """ primary_expression  : unified_string_literal
                                | unified_wstring_literal
        """
        p[0] = p[1]

    def p_primary_expression_4(self, p):
        """ primary_expression  : LPAREN expression RPAREN """
        p[0] = p[2]

    def p_primary_expression_5(self, p):
        """ primary_expression  : OFFSETOF LPAREN type_name COMMA offsetof_member_designator RPAREN
        """
        coord = self._token_coord(p, 1)
        p[0] = ast.FuncCall(ast.ID(p[1], coord),
                              ast.ExprList([p[3], p[5]], coord),
                              coord)

    def p_offsetof_member_designator(self, p):
        """ offsetof_member_designator : identifier
                                         | offsetof_member_designator PERIOD identifier
                                         | offsetof_member_designator LBRACKET expression RBRACKET
        """
        if len(p) == 2:
            p[0] = p[1]
        elif len(p) == 4:
            field = ast.ID(p[3], self._token_coord(p, 3))
            p[0] = ast.StructRef(p[1], p[2], field, p[1].coord)
        elif len(p) == 5:
            p[0] = ast.ArrayRef(p[1], p[3], p[1].coord)
        else:
            raise NotImplementedError("Unexpected parsing state. len(p): %u" % len(p))

    def p_argument_expression_list(self, p):
        """ argument_expression_list    : assignment_expression
                                        | argument_expression_list COMMA assignment_expression
        """
        if len(p) == 2: # single expr
            p[0] = ast.ExprList([p[1]], p[1].coord)
        else:
            p[1].exprs.append(p[3])
            p[0] = p[1]

    def p_identifier(self, p):
        """ identifier  : ID """
        p[0] = ast.ID(p[1], self._token_coord(p, 1))

    def p_constant_1(self, p):
        """ constant    : INT_CONST_DEC
                        | INT_CONST_OCT
                        | INT_CONST_HEX
                        | INT_CONST_BIN
        """
        uCount = 0
        lCount = 0
        for x in p[1][-3:]:
            if x in ('l', 'L'):
                lCount += 1
            elif x in ('u', 'U'):
                uCount += 1
        t = ''
        if uCount > 1:
             raise ValueError('Constant cannot have more than one u/U suffix.')
        elif lCount > 2:
             raise ValueError('Constant cannot have more than two l/L suffix.')
        prefix = 'unsigned ' * uCount + 'long ' * lCount
        p[0] = ast.Constant(
            prefix + 'int', p[1], self._token_coord(p, 1))

    def p_constant_2(self, p):
        """ constant    : FLOAT_CONST
                        | HEX_FLOAT_CONST
        """
        if 'x' in p[1].lower():
            t = 'float'
        else:
            if p[1][-1] in ('f', 'F'):
                t = 'float'
            elif p[1][-1] in ('l', 'L'):
                t = 'long double'
            else:
                t = 'double'

        p[0] = ast.Constant(
            t, p[1], self._token_coord(p, 1))

    def p_constant_3(self, p):
        """ constant    : CHAR_CONST
                        | WCHAR_CONST
        """
        p[0] = ast.Constant(
            'char', p[1], self._token_coord(p, 1))

    # The "unified" string and wstring literal rules are for supporting
    # concatenation of adjacent string literals.
    # I.e. "hello " "world" is seen by the C compiler as a single string literal
    # with the value "hello world"
    #
    def p_unified_string_literal(self, p):
        """ unified_string_literal  : STRING_LITERAL
                                    | unified_string_literal STRING_LITERAL
        """
        if len(p) == 2: # single literal
            p[0] = ast.Constant(
                'string', p[1], self._token_coord(p, 1))
        else:
            p[1].value = p[1].value[:-1] + p[2][1:]
            p[0] = p[1]

    def p_unified_wstring_literal(self, p):
        """ unified_wstring_literal : WSTRING_LITERAL
                                    | unified_wstring_literal WSTRING_LITERAL
        """
        if len(p) == 2: # single literal
            p[0] = ast.Constant(
                'string', p[1], self._token_coord(p, 1))
        else:
            p[1].value = p[1].value.rstrip()[:-1] + p[2][2:]
            p[0] = p[1]

    def p_brace_open(self, p):
        """ brace_open  :   LBRACE
        """
        p[0] = p[1]
        p.set_lineno(0, p.lineno(1))

    def p_brace_close(self, p):
        """ brace_close :   RBRACE
        """
        p[0] = p[1]
        p.set_lineno(0, p.lineno(1))

    def p_empty(self, p):
        'empty : '
        p[0] = None

    def p_error(self, p):
        # If error recovery is added here in the future, make sure
        # _get_yacc_lookahead_token still works!
        #
        if p:
            self._parse_error(
                'before: %s' % p.value,
                self._coord(lineno=p.lineno,
                            column=self.clex.find_tok_column(p)))
        else:
            self._parse_error('At end of input', self.clex.filename)
