#
# onnx-build-prelude - Convert ONNX Schemas to Knossos prelude
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

# %%

#%%
def onnxAttrType_to_Type(ty):
    assert isinstance(ty, onnx.defs.OpSchema.AttrType)
    if ty == ty.FLOAT:
        return Type.Float
    if ty == ty.INT:
        return Type.Integer
    if ty == ty.STRING:
        return Type.String
    if ty == ty.TENSOR:
        return Type.Vec(Type.Float)

    if ty == ty.FLOATS:
        return Type.Vec(Type.Float)
    if ty == ty.INTS:
        return Type.Vec(Type.Integer)
    if ty == ty.STRINGS:
        return Type.Vec(Type.String)
    if ty == ty.TENSORS:
        return Type.Vec(Type.Vec(Type.Float))

    if ty == ty.GRAPH:
        return Type.Lam(None, None)

    #   class AttrType(object):
    #     FLOAT: 'OpSchema.AttrType' = ...
    #     INT: 'OpSchema.AttrType' = ...
    #     STRING: 'OpSchema.AttrType' = ...
    #     TENSOR: 'OpSchema.AttrType' = ...
    #     GRAPH: 'OpSchema.AttrType' = ...
    #     SPARSE_TENSOR: 'OpSchema.AttrType' = ...
    #     FLOATS: 'OpSchema.AttrType' = ...
    #     INTS: 'OpSchema.AttrType' = ...
    #     STRINGS: 'OpSchema.AttrType' = ...
    #     TENSORS: 'OpSchema.AttrType' = ...
    #     GRAPHS: 'OpSchema.AttrType' = ...
    #     SPARSE_TENSORS: 'OpSchema.AttrType' = ...

    print(ty)
    raise NotImplementedError(f"didn't handle {ty}")

def onnxType_to_Type(ty : str):
    if ty.startswith('tensor('):
        assert ty.endswith(')')
        return Type.Vec(onnxType_to_Type(ty[7:-1]))

    if ty.startswith('seq('):
        assert ty.endswith(')')
        return Type.Vec(onnxType_to_Type(ty[4:-1]))

    if ty.startswith('map('):
        assert ty.endswith(')')
        return Type.Vec(onnxType_to_Type("tuple(" + ty[4:-1] + ")"))

    if ty.startswith('tuple('):
        assert ty.endswith(')')
        args = ty[6:-1]
        tys = [onnxType_to_Type(s) for s in re.split(', *', args)]
        return Type.Tuple(*tys)

    if ty in ['double', 'float', 'float16']:
        return Type.Float

    if ty in ['complex64', 'complex128']:
        # TODO: functions taking these types will need some mangling
        print(f"Note: complex args converted to tuple")
        return Type.Tuple(Type.Float,Type.Float)

    if ty in ['int64', 'int32', 'int16', 'int8', 
              'uint64', 'uint32', 'uint16', 'uint8']:
        return Type.Integer

    if ty == 'string':
        return Type.String

    if ty == 'bool':
        return Type.Bool

    print(ty)
    raise NotImplementedError(f"didn't convert '{ty}''")

TypeConstraintParam = namedtuple('TypeConstraintParam', ['name', 'ty'])

def all_combinations_type_constraints(type_constraints):
    """
    Take list of type constraints of the form
        [ ("T1", [ty11, ty12, ty13]), ("T2", [ty21, ty22])]
    and generate the list
        [ [("T1", ty11), ("T2", ty21)]
          [("T1", ty11), ("T2", ty22)]
          [("T1", ty12), ("T2", ty21)]
          [("T1", ty12), ("T2", ty22)]
          [("T1", ty13), ("T2", ty21)]
          [("T1", ty13), ("T2", ty22)]]
    """
    if len(type_constraints) == 0:
        return [[]]

    tc = type_constraints[0]  # Generate all combinations from head and tail
    tys = tc.allowed_type_strs # Get all ONNX types
    tys = set([onnxType_to_Type(ty) for ty in tys]) # Convert to Knossos and uniquify
    tcs = [ TypeConstraintParam(tc.type_param_str, ty) for ty in tys ]
    tails = all_combinations_type_constraints(type_constraints[1:])
    return [ [x] + tail for x in tcs for tail in tails ]

def test_all_combinations_type_constraints():
    print("Test test_all_combinations_type_constraints")
    for s in onnx.defs.get_all_schemas():
        if s.name == "CastMap":
            tcs = s.type_constraints
            all = all_combinations_type_constraints(tcs)
            assert len(all) == 6

test_all_combinations_type_constraints()

prelude = open("onnx-prelude.ks", "w")

out = []
for s in onnx.defs.get_all_schemas():
    print(f";; Doing {s.name} # line \"{s.file}\" {s.line}")
    prelude.write(f";; Doing {s.name} # line \"{s.file}\" {s.line}\n")

    # 0: Some special-cases
    if s.name == "Constant":
        out.append(";; Constant")
        continue

    # Gather type constraints
    all_signatures = all_combinations_type_constraints(s.type_constraints)

    if len(s.type_constraints) > 1:
        print(f"Found {len(all_signatures)} signatures")
        for tc in s.type_constraints:
            print("TC:", tc.type_param_str, "|", tc.allowed_type_strs, "|", tc.description)    

    for sig in all_signatures:
        # Gather (Tn, Type) pairs into dict
        typeStrDict = {tc.name:tc.ty for tc in sig}

        # 1: Assemble arguments.  Knossos treats "inputs" and "attributes" uniformly.
        args = []
        # 1.1: Inputs
        for i in s.inputs:
            ty = typeStrDict.get(i.typeStr)
            if ty == None:
                tys = set([onnxType_to_Type(ty) for ty in i.types])
                if len(tys) > 1:
                    print(f"multiple types but no type constraints at {s.name}, input {i.name}: {tys}")
                    ty = tys.pop()
            arg = Var(i.name, ty, True)
            args.append(arg)

        # 1.2: Attributes
        for key in s.attributes:
            a = s.attributes[key]
            arg = Var(a.name, onnxAttrType_to_Type(a.type), True)
            args.append(arg)

        # 1.1: Outputs
        return_type = Type.Tuple()
        if len(s.outputs) != 1:
            warnings.warn("TODO: multiple return types")
    
        for o in s.outputs:
            # FormalParameter(pybind11_builtins.pybind11_object)
            # |  description
            # |  isHomogeneous
            # |  name
            # |  option
            # |  typeStr   # Alias for the type, referred to in type constraints
            # |  types
            
            ty = typeStrDict.get(o.typeStr)
            if ty == None:
                tys = set([onnxType_to_Type(ty) for ty in o.types])
                if len(tys) > 1:
                    print(f"multiple return types but no type constraints at {s.name}, output {o.name}: {tys}")
                    ty = tys.pop()
            return_type = ty

        # 2: Def vs edef
        if s.has_function:
            print(f";; has body: {type(s.function_body)}", file=prelude)

        obj = EDef(s.name, return_type, args)

        pprint(obj, stream=prelude, width=120, indent=2)

    if s.has_type_and_shape_inference_function:
        pass # out.append(EDef("shape$" + s.name, Type.Float, [Type.Float, Type.Vec(Type.Float)]))
        
prelude.close()
