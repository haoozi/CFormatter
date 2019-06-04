#-----------------------------------------------------------------
# pycparser: _build_tables.py
#
# A dummy for generating the lexing/parsing tables and and
# compiling them into .pyc for faster execution in optimized mode.
# Also generates AST code from the configuration file.
# Should be called from the pycparser directory.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------

# Insert '.' and '..' as first entries to the search path for modules.
# Restricted environments like embeddable python do not include the
# current working directory on startup.

# Generate c_ast.py
from ast_generator import ASTCodeGenerator
ast_gen = ASTCodeGenerator('c_ast.cfg')
ast_gen.generate(open('ast.py', 'w'))
