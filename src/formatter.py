import sys
import argparse

# This is not required if you've installed pycparser into
# your site-packages/ with setup.py
#
sys.path.extend(['.', '..'])


import util
import parser
import gen






if __name__ == "__main__":
    # if len(sys.argv) > 1:
    #     translate_to_c(sys.argv[1])
    # else:
    #     print("Please provide a filename as argument")


    ast = util.parse_file(sys.argv[1])
    # print(ast)
    generator = gen.CodeGenerator(style = gen_s.ExampleCStyle())
    print(generator.visit(ast))
