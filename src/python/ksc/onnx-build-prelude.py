#
# onnx-build-prelude - Convert ONNX Schemas to Knossos prelude
#
# References:
# https://github.com/onnx/onnx/blob/master/docs/IR.md
# https://github.com/onnx/onnx/blob/72b701f7a55cafa4b8ab66a21dc22da0905b2f4c/onnx/onnx.in.proto

#%%

from typing import TextIO
from collections import namedtuple

import warnings
import sys
import re

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

#%%
def comment(s : str):
    """
    Make a LISP inline comment from str
    """
    return f"#|{s}|#"

def onnxAttrType_to_Type(ty):
    """
    Convert ONNX AttrType to KS Type.
    Currently collapses 
    """
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

def onnxType_to_Type_with_mangler(ty : str):
    """
    Convert ty to KS type, and record any transformations (E.g. Complex->(Tuple Float Float)) in "mangler"
    """
    if ty.startswith('tensor('):
        # tensor -> Vec for now, but see great renaming
        assert ty.endswith(')')
        mangler,elty = onnxType_to_Type_with_mangler(ty[7:-1])
        return mangler, Type.Vec(elty)

    if ty.startswith('seq('):
        # seq -> Vec
        assert ty.endswith(')')
        mangler,elty = onnxType_to_Type_with_mangler(ty[4:-1])
        return "seq$"+mangler, Type.Vec(elty)

    if ty.startswith('map('):
        # map -> Vec Tuple
        assert ty.endswith(')')
        mangler,elty = onnxType_to_Type_with_mangler("tuple(" + ty[4:-1] + ")")
        return "map$"+mangler, Type.Vec(elty)

    if ty.startswith('tuple('):
        assert ty.endswith(')')
        args = ty[6:-1]
        manglers_and_tys = [onnxType_to_Type_with_mangler(s) for s in re.split(', *', args)]
        manglers = "".join([x[0] for x in manglers_and_tys])
        tys = [x[1] for x in manglers_and_tys]
        return f"{manglers}",Type.Tuple(*tys)

    if ty in ['double', 'float', 'float16']:
        return "",Type.Float

    if ty in ['complex64', 'complex128']:
        return "cplx$",Type.Tuple(Type.Float,Type.Float)

    if ty in ['int64', 'int32', 'int16', 'int8', 
              'uint64', 'uint32', 'uint16', 'uint8']:
        return "",Type.Integer

    if ty == 'string':
        return "",Type.String

    if ty == 'bool':
        return "",Type.Bool

    raise NotImplementedError(f"didn't convert '{ty}''")

def onnxType_to_Type(ty : str):
    return onnxType_to_Type_with_mangler(ty)[1]

TypeConstraintParam = namedtuple('TypeConstraintParam', ['name', 'ty'])

def TCPs_from_tc(type_constraint):
    """
    Take type_constraint(type_param_str, allowed_type_strs) and return list of TypeConstraintParam
    """
    tys = type_constraint.allowed_type_strs # Get all ONNX types
    tys = set([onnxType_to_Type_with_mangler(ty) for ty in tys]) # Convert to Knossos and uniquify
    return [ TypeConstraintParam(type_constraint.type_param_str, ty) for ty in tys ] # make list

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

    # Generate all combinations from head and tail
    tcps = TCPs_from_tc(type_constraints[0])
    tails = all_combinations_type_constraints(type_constraints[1:])
    return [ [x] + tail 
               for x in tcps
                 for tail in tails ]

def test_all_combinations_type_constraints():
    print("Test test_all_combinations_type_constraints")
    for s in onnx.defs.get_all_schemas():
        if s.name == "CastMap":
            tcs = s.type_constraints
            all = all_combinations_type_constraints(tcs)
            assert len(all) == 6

test_all_combinations_type_constraints()

def onnx_schemas_to_prelude(prelude : TextIO):
    def writeln(line):
        prelude.write(line + "\n")

    writeln(';; THIS FILE IS AUTOGENERATED.  See onnx-build-prelude.py.')

    print("Processing:", end='')
    for s in onnx.defs.get_all_schemas():
        print(f" {s.name}", end='')

        # 0. Send the doc
        writeln(f";; Doing {s.name} # line \"{s.file}\" {s.line}")

        for doc in s.doc.splitlines():
            writeln(f";; {doc}")

        writeln(f";; Type constraints:")
        for tc in s.type_constraints:
            writeln(f";; {tc.type_param_str} | {tc.allowed_type_strs} | {tc.description}")

        # 0.1 Some special-cases, which are assumed "hand-written" in the output,
        # e.g. output type depends on some runtime value.
        # We can relax this if we assume the runtime values are always constants -- e.g. some attrs
        if s.name in ["Constant", "ZipMap", 
                      "SequenceEmpty", "Cast", "CastMap", 
                      # "EyeLike", 
                      "TreeEnsembleClassifier", "LinearClassifier", "SVMClassifier",
                      "LabelEncoder", "CategoryMapper", 
                      "If"]:
            print("!", end='')
            writeln(f";; SPECIAL: {s.name}")
            continue

        out_type_from_sig = None
        if s.name == "ConcatFromSequence":
            # Onnx type constraints can't express S: seq<'t> -> T: 't
            def ConcatFromSequence_type(tys):
                return Type.Index(tys[0])
            out_type_from_sig = lambda tys: ConcatFromSequence_type(tys)

        if s.name == "DictVectorizer":
            def DictVectorizer_type(tys):
                # Vec (Tuple key value)
                kv = Type.Index(tys[0])
                value_ty = kv.children[1]
                return Type.Vec(value_ty)
            out_type_from_sig = lambda tys: DictVectorizer_type(tys)

        if s.name == "SplitToSequence":
            out_type_from_sig = lambda tys: Type.Vec(tys[0])

        if s.name == "SequenceAt":
            out_type_from_sig = lambda tys: Type.Index(tys[0])

        if s.name == "SequenceConstruct":
            out_type_from_sig = lambda tys: Type.Vec(tys[0])

        # SequenceAt, SequenceConstruct, SequenceEmpty, NonMaxSuppression, TreeEnsembleRegressor

        # Gather type constraints
        output_typeStrs = set([o.typeStr for o in s.outputs])
        input_typeStrs = set([o.typeStr for o in s.inputs])
        input_type_constraints = list(filter(lambda tc: tc.type_param_str in input_typeStrs, s.type_constraints))
        
        all_signatures = all_combinations_type_constraints(input_type_constraints)

        if len(all_signatures) > 1:
            print(f"[x{len(all_signatures)}]", end='')

        for sig in all_signatures:
            # Gather (Tn, Type) pairs into dict
            typeStrDict = {tc.name:tc.ty for tc in sig}

            # 1: Assemble arguments.  Knossos treats "inputs" and "attributes" uniformly.
            args = []

            def onnxTypes_to_mangler_and_Type(typeStr, types):
                mangler_and_ty = typeStrDict.get(typeStr)
                if mangler_and_ty != None:
                    return mangler_and_ty
                tys = set([onnxType_to_Type_with_mangler(ty) for ty in types])
                if len(tys) > 1:
                    writeln(f";; WARN: multiple types but no type constraints at {s.name}: {tys}")
                mangler_and_ty = tys.pop()
                assert mangler_and_ty != None
                return mangler_and_ty

            # 1.1: Inputs
            for i in s.inputs:
                mangler,ty = onnxTypes_to_mangler_and_Type(i.typeStr, i.types)
                annot = comment(mangler) if mangler != '' else ''
                arg = Var(i.name + annot, ty, True)
                args.append(arg)

            # 1.2: Attributes
            for key in s.attributes:
                a = s.attributes[key]
                arg = Var(a.name + comment("attr"), onnxAttrType_to_Type(a.type), True)
                args.append(arg)

            # 1.1: Outputs
            return_types = []
            if out_type_from_sig == None:
                for o in s.outputs:
                    mangler,ty = onnxTypes_to_mangler_and_Type(o.typeStr, o.types)
                    if mangler != '':
                        writeln(f";; NOTE: output mangler {mangler}")
                    return_types.append(ty)
            else:
                return_types = [out_type_from_sig([v.type for v in args])]

            # Return multiple outputs as a tuple
            if len(return_types) > 1:
                return_type = Type.Tuple(*return_types)
                writeln(f";; NOTE: multiple outputs as tuple")
            elif len(return_types) == 1:
                return_type = return_types[0]
            else:
                return_type = Type.Tuple()  # Empty tuple is Knossos "unit" type

            # Grab any manglers
            manglers = set([onnxType_to_Type_with_mangler(ty)[0] for ty in i.types for i in s.inputs + s.outputs])
            if '' in manglers:
                manglers.remove('')
            if len(manglers) > 0:
                writeln(f";; NOTE: manglers {manglers}")

            # 2: Def vs edef
            if s.has_function:
                writeln(f";; has body: {type(s.function_body)}")

            obj = EDef(s.name, return_type, args)

            pprint(obj, stream=prelude, width=180, indent=2)

        if s.has_type_and_shape_inference_function:
            pass # out.append(EDef("shape$" + s.name, Type.Float, [Type.Float, Type.Vec(Type.Float)]))

    print("... done")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        filename = "etc/onnx-prelude-autogen.ks"
    else:
        filename = sys.argv[1]
    with open(filename, "w") as prelude:
        onnx_schemas_to_prelude(prelude)

# %%
