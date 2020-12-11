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
from itertools import chain
from typing import List

import sys
import inspect
import re
import warnings

import numpy as np

import onnx, onnx.numpy_helper, onnx.helper
from onnx import TensorProto, AttributeProto

import onnxruntime.capi.onnxruntime_pybind11_state as ort
from   onnxruntime.capi.onnxruntime_pybind11_state.schemadef import OpSchema

from ksc.utils import paren
from ksc.type import Type
from ksc.expr import Expr, Def, EDef, Rule, Const, Var, Lam, Call, Let, If, Assert
from ksc.expr import pystr

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
ATTR_TYPE_TO_KS_TYPE = {
    OpSchema.AttrType.FLOAT: Type.Float,
    OpSchema.AttrType.INT: Type.Integer,
    OpSchema.AttrType.STRING: Type.String,
    OpSchema.AttrType.TENSOR: Type.Vec(Type.Float),
    OpSchema.AttrType.FLOATS: Type.Vec(Type.Float),
    OpSchema.AttrType.INTS: Type.Vec(Type.Integer),
    OpSchema.AttrType.STRINGS: Type.Vec(Type.String),
    OpSchema.AttrType.TENSORS: Type.Vec(Type.Vec(Type.Float)),
    OpSchema.AttrType.GRAPH: Type.Lam(None, None)
}

def onnxAttrType_to_Type(ty):
    """
    Convert ONNX AttrType to KS Type.
    # TODOS:
    #     ty.SPARSE_TENSOR
    #     ty.SPARSE_TENSORS
    #     ty.GRAPHS
    """
    assert isinstance(ty, OpSchema.AttrType)
    kstype = ATTR_TYPE_TO_KS_TYPE[ty]

    return kstype


# See https://github.com/onnx/onnx/blob/master/onnx/mapping.py
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

def onnxTensorType_to_Type(ety):
    ty = TENSOR_TYPE_TO_KS_TYPE.get(ety)
    if ty != None:
        return ty
    raise NotImplementedError(f"type {ety}")

def onnxTensorElementType_to_Type(proto):
    ety = onnxTensorType_to_Type(proto.tensor_type.elem_type)
    return Type.Vec(ety)

def get_value(init):
    """
    Get scalar value from a TensorProto
    """
    # https://github.com/onnx/onnx/blob/72b701f7a55cafa4b8ab66a21dc22da0905b2f4c/onnx/onnx.in.proto#L448
    # Find https://github.com/onnx/onnx/blob/72b701f7a55cafa4b8ab66a21dc22da0905b2f4c/onnx/mapping.py#L49 
    
    a = onnx.numpy_helper.to_array(init)

    ksty = onnxTensorType_to_Type(init.data_type)

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

def mkVec(val):
    if isinstance(val, (list, np.ndarray)):
        return Call("vec", [Const(v) for v in val])
    else:
        return Call("vec", [Const(val)])

def exprFromTensorProto(val, name):
    """
    Make KS values from a TensorProto
    """
    if len(val.dims) == 1 and val.dims[0] < 16:
        # vec constructor
        return mkVec(onnx.numpy_helper.to_array(val))

    if len(val.dims) == 0:
        return get_value(val)

    nptype = onnx.mapping.TENSOR_TYPE_TO_NP_TYPE[val.data_type] 
    return Call(f"load-from-onnx-{nptype}", [*(Const(x) for x in val.dims), Const(name)])

def exprFromAttrVal(val):
    if isinstance(val, onnx.TensorProto):
        return exprFromTensorProto(val, "?exprFromAttrVal?")

    if isinstance(val, list):
        return mkVec(val)

    if isinstance(val, bytes):
        val = val.decode("ascii")
    return Const(val)

def exprFromAttr(attr : AttributeProto):
    # ty = onnxAttrType_to_Type(attr.type)
    a = onnx.helper.get_attribute_value(attr)
    return exprFromAttrVal(a)


def emit_inits(inits, body):
    """
    Emit initializers
    """
    for init in reversed(inits):
        var = useVar(init.name)

        value = exprFromTensorProto(init, var.name)

        body = Let(var, value, body)
    return body

def get_attribute_default_value(attr):
    if not hasattr(attr, "_default_value") or not attr._default_value:
        return None

    val = onnx.AttributeProto()
    _nbytes = val.ParseFromString(attr._default_value)
    val = onnx.helper.get_attribute_value(val)

    ty = onnxAttrType_to_Type(attr.type)
    if ty.is_scalar:
        return exprFromAttrVal(val)
    
    if ty.is_vec:
        return mkVec(val)
 
    return val

def get_default_value(schema, attr):
    val = get_attribute_default_value(attr)
    if val != None:
        return val

    print(f"** Attribute {attr.name} of op {schema.name} has no default value -- special casing")

    # TODO: Formalize this, at least into prelude
    if schema.name == "MaxPool":
        if attr.name == "dilations":
            return exprFromAttrVal([1, 1])

    if schema.name == "Conv" or schema.name == "ConvTranspose":
        if attr.name == "pads":
            return exprFromAttrVal([-1, -1]) # TODO: this should either match dims, or maybe be empty
        if attr.name == "output_shape":
            return exprFromAttrVal([-1, -1])

    print(schema.doc)

    raise NotImplementedError()       

def get_all_schemas():
    # Form schema group, take latest version
    schemas = dict()
    for s in ort.get_all_operator_schema():
        name = s.name if s.domain == "" else s.domain + "." + s.name
        if name in schemas and s.since_version < schemas[name].since_version:
            pass
        schemas[name] = s
    return schemas

def encode_name(name: str):
    if re.match("^[0-9]", name):
        return "%" + name
    else:
        return name

def useVar(name: str):
    return Var(encode_name(name), None, False)

def defVar(name: str, ty : Type):
    return Var(encode_name(name), ty, True)

def onnx2ks(g):
    """
    Convert ONNX graph g into Knossos Expr.
    """

    schemas = get_all_schemas()

    # ONNX graph nodes are topologically sorted, so just run through in reverse order
    # making a chain of lets.
    body = None
    for node in reversed(g.node):
        opname = node.op_type if not node.domain else node.domain + "." + node.op_type 
        s = schemas[opname]
        name = s.name

        # Collect args from input
        args = [useVar(i) for i in node.input]

        # Special cases
        if opname == "Constant":
            # Constant: exactly one of the optionals should be set
            assert len(node.attribute) == 1
            n = node.attribute[0]
            args = [exprFromAttr(n)]

        elif opname == "Cast":
            # Cast: output type depends on input value.  We postpend the type to the name
            assert len(node.attribute) == 1
            n = node.attribute[0]
            assert n.type == OpSchema.AttrType.INT
            out_type = OpSchema.AttrType(n.i)
            out_type = str(out_type)
            assert out_type.startswith("AttrType.")
            out_type = out_type[9:]
            name = "Cast_" + out_type

        else:
            # Collect attributes. 
            # - Some are set on the node.
            node_attrs = dict()
            for n in node.attribute:
                val = exprFromAttr(n)
                node_attrs[n.name] = val
                if n.name not in s.attributes:
                    warnings.warn(f"Attribute {n.name} not in schema for {opname} -- adding arg anyway")
                    args += [val]

            # - The rest are defaulted
            for attrname,attr in s.attributes.items():
                if attrname in node_attrs:
                    args += [node_attrs[attrname]]
                else:
                    args += [get_default_value(s, attr)]

        # Are we dropping optional outputs?   
        # This is typically only known at the call site.
        # Change the function name to reflect that, 
        # otherwise type annotation would have to happen 
        # at more than one place 
        n_outputs = len(node.output)
        if n_outputs < len(s.outputs):
            name = f"take{n_outputs}${name}"

        # Generate the call node
        rhs = Call(name, args) 

        # Now bind multiple outputs 
        # (let ((out1 ... outN) (op args))
        #   body)
        vars = [useVar(o) for o in node.output]
        if len(vars) == 1:
            vars = vars[0]

        # First time round, this is the innermost body (or the last node in the graph): just reference the output vars
        if body == None:
            if isinstance(vars, Var):
                body = vars
            else:
                body = Call("tuple", vars)

        # Rest of the time, wrap previous body in a Let
        body = Let(vars, rhs, body)

    body = Let(Var("$end_of_inits #|End of initializers|# "), Const(99999), body)

    # And now gather the initializers, and place in Lets
    inputs = set([i.name for i in g.input])

    inits = {init.name:init for init in g.initializer}

    internal_inits = []
    for init in g.initializer:
        if init.name not in inputs:
            internal_inits.append(init)

    # value_infos -> types
    args = [defVar(i.name, onnxTensorElementType_to_Type(i.type)) for i in g.input]

    body = Let(Var("$beg_of_internal_inits #|Begin of internal initializers|# "), Const(99999), body)
    ex = emit_inits(internal_inits, body)

    # Emit a def for the whole graph
    #    (def GraphName None args body)
    decl = Def(g.name, None, args, ex)

    # And emit a "main" to call it with initializers
    # (def main None () 
    #     (GraphName initializers.. ) )
    main_body = Call(g.name, [useVar(i.name) for i in g.input])
    input_inits = [inits[i.name] for i in g.input if i.name in inits]
    main_args = [arg for arg in args if arg.name not in inits]
    main = Def("main", None, main_args, emit_inits(input_inits, main_body))

    # Collect the two decls 
    return [decl, main]

if __name__ == "__main__":
    import os
    from ksc.parse_ks import parse_ks_filename
    from ksc.type_propagate import type_propagate_decls

    argv = sys.argv

    if len(argv) == 1:
        os.makedirs('obj/test/onnx2ks', exist_ok=True)
        argv = ['*EXE*', 'test/onnx2ks/mnist-7.onnx', 'obj/']

    nargs = len(argv) - 1
    if nargs == 0:
        filename = "/dev/stdin"
    else:
        filename = argv[1]

    outbase = None
    if nargs == 2:
        outbase = argv[2]
        if outbase.endswith("/"):
            outbase = outbase + re.sub(r'\.onnx$','',filename)

    type_propagate = True

    def save(decls, ofn, msg):
        print(f"onnx2ks: Writing to {ofn}")
        os.makedirs(os.path.dirname(ofn), exist_ok=True)
        with open(ofn, "w") as out:
            out.write(f";; {msg}\n")
            for decl in decls:
                pprint(decl,width=256,ribbon_width=256,stream=out)
                out.write("\n")

    # Load our file
    print(f"onnx2ks: Reading from {filename}")
    model = onnx.load(filename)

    print(f"onnx2ks: Writing graph to {outbase}.onnx.txt")
    with open(outbase + ".onnx.txt", "w") as out:
        out.write(onnx.helper.printable_graph(model.graph))

    # Convert to untyped KS
    decls = onnx2ks(model.graph)

    if outbase:
        save(decls, outbase + ".untyped.ks", f" untyped KS from {filename}")

    # Check model -- do ORT models need to be toposorted?
    # onnx.checker.check_model(model)

    if type_propagate:
        # Load preludes
        print(f"onnx2ks: Reading prelude")
        symtab = dict()

        prelude_decls = parse_ks_filename("etc/onnx-prelude.ks")
        type_propagate_decls(prelude_decls, symtab)

        prelude_decls = parse_ks_filename("etc/onnx-prelude-autogen.ks")
        type_propagate_decls(prelude_decls, symtab)

        # Apply type annotations
        type_propagate_decls(decls, symtab)

    if outbase:
        # And save
        save(decls, outbase + ".ks", f"AUTOGENERATED FROM {filename}")

        # And load again...
        decls = parse_ks_filename(outbase + ".ks")

    if False:
        # Pretty print
        for decl in decls:
            cpprint(decl, width=132, ribbon_width=132)
            print('')



# %%
