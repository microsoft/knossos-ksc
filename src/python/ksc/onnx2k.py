#
# onnx2ks - convert ONNX to Knossos
#
# References:
# https://github.com/onnx/onnx/blob/master/docs/IR.md
# https://github.com/onnx/onnx/blob/72b701f7a55cafa4b8ab66a21dc22da0905b2f4c/onnx/onnx.in.proto

# TODO:
# - Emit "def" FunctionProtos in prelude
# - Add pre/post comments to expressions, and emit argument names as comments
# - roundtrip test
# - Fix pylint on TensorProto.FLOAT

#%%

from collections import namedtuple
import sys
import inspect
import re
import warnings

import onnx, onnx.numpy_helper

from ksc.utils import paren
from ksc.type import Type
from ksc.expr import Def, EDef, Rule, Const, Var, Lam, Call, Let, If, Assert

# Pretty printing
# Importing prettyprint to get the decorated printers for Expression and Type
import ksc.prettyprint # pylint: disable=unused-import

# Import the prettyprinter routines we use explicitly in this file
from prettyprinter import cpprint, pprint, pformat

# Needed this in order to see the error messages when pprint fails
import warnings
warnings.filterwarnings("always")

############################################################################
# Oops, some of this code was written by functional programmers...
import resource
# resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY,resource.RLIM_INFINITY))
sys.setrecursionlimit(10**6)

############################################################################
def encode_name(name: str):
    if re.match("^[0-9]", name):
        return "%" + name
    else:
        return name

def useVar(name: str):
    return Var(encode_name(name), None, False)

def defVar(name: str, ty : Type):
    return Var(encode_name(name), ty, True)

# See https://github.com/onnx/onnx/blob/master/onnx/mapping.py
from onnx import TensorProto
TENSOR_TYPE_TO_KS_TYPE = {
    int(TensorProto.FLOAT): Type.Float,
    int(TensorProto.UINT8): Type.Integer,
    int(TensorProto.INT8): Type.Integer,
    int(TensorProto.UINT16): Type.Integer,
    int(TensorProto.INT16): Type.Integer,
    int(TensorProto.INT32): Type.Integer,
    int(TensorProto.INT64): Type.Integer,
    int(TensorProto.BOOL): Type.Bool,
    int(TensorProto.FLOAT16): Type.Float,
    int(TensorProto.DOUBLE): Type.Float,
    int(TensorProto.COMPLEX64): Type.Tuple(Type.Float,Type.Float),
    int(TensorProto.COMPLEX128): Type.Tuple(Type.Float,Type.Float),
    int(TensorProto.UINT32): Type.Integer,
    int(TensorProto.UINT64): Type.Integer,
    int(TensorProto.STRING): Type.String
}

def convertElementType(ety):
    ty = TENSOR_TYPE_TO_KS_TYPE.get(ety)
    if ty != None:
        return ty
    raise NotImplementedError(f"type {ety}")

def convertType(proto):
    ety = convertElementType(proto.tensor_type.elem_type)
    return Type.Vec(ety)

def get_value(init):
    """
    Get scalar value from a TensorProto
    """
    # https://github.com/onnx/onnx/blob/72b701f7a55cafa4b8ab66a21dc22da0905b2f4c/onnx/onnx.in.proto#L448
    # Find https://github.com/onnx/onnx/blob/72b701f7a55cafa4b8ab66a21dc22da0905b2f4c/onnx/mapping.py#L49 
    
    a = onnx.numpy_helper.to_array(init)

    ksty = convertElementType(init.data_type)

    if ksty == Type.Bool:
        return Const(bool(a))

    if ksty == Type.Float:
        return Const(float(a))

    if ksty == Type.Integer:
        return Const(int(a))

    field = onnx.mapping.STORAGE_TENSOR_TYPE_TO_FIELD[init.data_type]
    if not field:
        raise NotImplementedError(f"field Type {field} {init.data_type}, {init}")

    value = getattr(init, field)
    if not value:
        raise NotImplementedError(f"value {init.int32_data} {field} {value} {init.data_type}\n{init}")

    return Const(value)

def get_values(init):
    """
    Get values from a TensorProto
    """
    a = onnx.numpy_helper.to_array(init)
    return [Const(v) for v in a]

def emit_inits(inits, body):
    """
    Emit initializers
    """
    for init in reversed(inits):
        var = useVar(init.name)
        if len(init.dims) == 1 and init.dims[0] < 16:
            # Putative vec constructor
            value = Call("vec", get_values(init))

        elif len(init.dims) == 0:
            value = get_value(init)
        else:
            value = Const(f"{init.dims}@{convertElementType(init.data_type)}")

        body = Let(var, value, body)
    return body

def onnx2ks(g):
    """
    Convert ONNX graph g into Knossos Expr.
    """

    # ONNX graph nodes are toplologically sorted, so just run through in reverse order
    # making a chain of lets.
    body = None
    for node in reversed(g.node):
        op = node.op_type
        args = [useVar(i) for i in node.input]
        rhs = Call(op, args)
        # (let ((out1 ... outN) (op args))
        #   body)

        vars = [useVar(o) for o in node.output]
        if len(vars) == 1:
            vars = vars[0]

        if body == None:
            # Innermost body: just reference the output vars
            body = vars 

        # Wrap previous body in a let
        body = Let(vars, rhs, body)

    inputs = set([i.name for i in g.input])

    inits = {init.name:init for init in g.initializer}

    internal_inits = []
    for init in g.initializer:
        if init.name not in inputs:
            internal_inits.append(init)

    # value_infos -> types
    args = [defVar(i.name, convertType(i.type)) for i in g.input]
    ex = emit_inits(internal_inits, body)

    # (def GraphName None args body)
    decl = Def(g.name, None, args, ex)

    # (def main None () 
    #     (GraphName initializers.. ) )
    main_body = Call(g.name, [useVar(i.name) for i in g.input])
    input_inits = [inits[i.name] for i in g.input if i.name in inits]
    main_args = [arg for arg in args if arg.name not in inits]
    main = Def("main", None, main_args, emit_inits(input_inits, main_body))

    return (decl,main)

if __name__ == "__main__":
    argv = sys.argv

    if len(argv) == 1:
        argv = ['*EXE*', 'test/onnx2k/mnist-7.onnx', 'obj/']

    nargs = len(argv) - 1
    if nargs == 0:
        filename = "/dev/stdin"
    else:
        filename = argv[1]

    ofn = None
    if nargs == 2:
        ofn = argv[2]
        if ofn.endswith("/"):
            ofn = ofn + re.sub(r'\.onnx$','.ks',filename)

    print(f"onnx2ks: Reading from {filename}")
    model = onnx.load(filename)
    #try:
    obj = onnx2ks(model.graph)
    # except:
    #     print(model)
    #     print("***ERROR***")
    #     onnx2ks(model.graph)

    if ofn == None:
        for o in obj:
            cpprint(obj)
            print('')
    else:
        print(f"onnx2ks: Writing to {ofn}")
        with open(ofn, "w") as out:
            out.write(f";; AUTOGENERATED FROM {filename}\n")
            for o in obj:
                pprint(o,width=1200,stream=out)
                out.write("\n")
