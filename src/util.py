#-----------------------------------------------------------------
# pycparser: __init__.py
#
# This package file exports some convenience functions for
# interacting with pycparser
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------


from parser import CParser




def parse_file(filename):

    with open(filename) as f:
        text = f.read()

    parser = CParser()
    return parser.parse(text, filename)
