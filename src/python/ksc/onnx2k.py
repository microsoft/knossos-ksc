#
# onnx2ks - convert ONNX to Knossos
#
# References:
# https://github.com/onnx/onnx/blob/master/docs/IR.md
# https://github.com/onnx/onnx/blob/72b701f7a55cafa4b8ab66a21dc22da0905b2f4c/onnx/onnx.in.proto

#%%

from collections import namedtuple
import re
import warnings

import onnx

from ksc.type import Type
from ksc.utils import paren
from ksc.expr import Def, EDef, Rule, Const, Var, Lam, Call, Let, If, Assert

# Pretty printing
# Importing prettyprint to get the decorated printers for Expression and Type
import ksc.prettyprint # pylint: disable=unused-import

# Import the prettyprinter routines we use explicitly in this file
from prettyprinter import cpprint, pprint, pformat

# Needed this in order to see the error messages when pprint fails
import warnings
warnings.filterwarnings("always")

# help(onnx.defs.get_all_schemas()[0])

# Load prelude...


# f = "/home/awf/dev/knossos-ksc/test/onnx2k/onnx-convtranspose.onnx"
# stack overflow.. f = "/home/awf/dev/knossos-ksc/test/onnx2k/bertsquad-10.onnx"
f = "/home/awf/dev/knossos-ksc/test/onnx2k/mnist-7.onnx"

m = onnx.load(f)

# doc(m)
print(m.graph)

def emit_inits(inits, body):
    if len(inits) == 0:
        return body
    init = inits[0]
    return Let(Var(init.name, None, False), Const(f"{init.dims}@{init.data_type}"),
               emit_inits(inits[1:], body))


def emit(nodes):
    node = nodes[0]
    op = node.op_type
    assert len(node.output) == 1
    out = Var(node.output[0], None, False)
    args = [Var(i, None, False) for i in node.input]
    rhs = Call(op, args)
    if len(nodes) > 1:
        return Let(out, rhs, emit(nodes[1:]))
    else:
        return Let(out, rhs, out)

# inputs -> args
# value_infos -> types
body = emit(m.graph.node)
ex = emit_inits(m.graph.initializer, body)
decl = Def(m.graph.name, None, [], ex)
cpprint(decl,width=1200)
