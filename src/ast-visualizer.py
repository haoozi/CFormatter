from anytree import Node, RenderTree
from anytree.exporter import DotExporter

import util



class ASTWalker:
    node_dic = {}

    def nodeIDGet(self, name):
        try:
            self.node_dic[name] += 1
        except Exception as e:
            self.node_dic[name] = 0

        return self.node_dic[name]

    def __init__(self):
        pass

    def walk(self, node, parent = None):
        className = node.__class__.__name__
        nodeName = className + '_' + str(self.nodeIDGet(className))

        newNode = Node(nodeName, parent = parent)

        for (child_name, child) in node.children():
            childNode = self.walk(child, newNode)


        return newNode


if __name__ == '__main__':
    t = util.parse_file(sys.argv[1])
    w = ASTWalker().walk(t)
    DotExporter(w).to_picture("./w.png")

#
#
# file = Node("FileAST")
#
# def0 = Node("FuncDef", parent = file)
# Decl0 = Node("Decl", parent = def0)
# FuncDecl0 = Node("FuncDecl", parent = Decl0)
#
# ParamList0 = Node("ParamList", parent = FuncDecl0)
# Decl1 = Node("Decl", )
#
# TypeDecl0 = Node("TypeDecl", parent = FuncDecl0)
