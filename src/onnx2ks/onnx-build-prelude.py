#
# onnx-build-prelude - Convert ONNX Schemas to Knossos prelude
#
# References:
# https://github.com/onnx/onnx/blob/master/docs/IR.md
# https://github.com/onnx/onnx/blob/72b701f7a55cafa4b8ab66a21dc22da0905b2f4c/onnx/onnx.in.proto
#
# This is largely a run-once script.  Its main task is to produce a consistent set of Knossos
# edefs for the current onnx schemas found in "onnx.defs.get_all_schemas()", and place them
# in "etc/onnx-prelude-autogen.ks".   It needs to handle a few concepts that don't translate
# well, such as:
#
#  - Attributes and inputs
#    KS has only inputs, constant parameters can be optimized by OTP
#
#  - Type constraints
#    ONNX has some slightly tricky rules to declare the same op over multiple types.
#    Knossos has ad-hoc overloading, so we just emit the same name with each acceptable type combo.
#    This leads to 25 decls for SequenceInsert, but it's seriously not a biggie in the scheme 
#    of things.  Think of it as C++ templates.
#
#  - Optional inputs
#    Emits one type signature for each callable version of the function
#
#  - Optional outputs
#    Emits versions of the function of the form "take1$foo" to auto-discard the outputs.
#
# Because it's a "run rarely" script, there are various manual hints to generate the
# right thing, and anthing "too hard" has been manually handled in "etc/onnx-prelude.ks"
#


#%%

from typing import TextIO
from collections import namedtuple

import warnings
import sys
import re

import onnx

import onnxruntime.capi.onnxruntime_pybind11_state as ort
from   onnxruntime.capi.onnxruntime_pybind11_state.schemadef import OpSchema

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

from onnx2ks import get_all_schemas, get_attribute_default_value, onnxAttrType_to_Type

#%%
def comment(s : str):
    """
    Make a LISP inline comment from str
    """
    return f"#|{s}|#"

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

    if ty in ['double', 'float', 'float16', 'bfloat16']:
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

    print(f"Processing schemas:", end='')
    schemas = get_all_schemas()
    for s in schemas.values():
        domain_prefix = "" if s.domain == "" else s.domain + "."
        print(f" {domain_prefix}{s.name}", end='')

        # 0. Send the doc
        writeln(f"\n;; Doing {domain_prefix}{s.name} # line \"{s.file}\" {s.line}")
        writeln(f"; since_version {s.since_version}")

        if s.deprecated:
            writeln(";; Deprecated")
            continue

        if s.doc:
            for doc in s.doc.splitlines():
                writeln(f";; {doc}")
        else:
            writeln(";; no doc")

        writeln(f";; Type constraints:")
        for tc in s.type_constraints:
            writeln(f";; {tc.type_param_str} | {tc.allowed_type_strs} | {tc.description}")

        # 0.1 Some special-cases, which are assumed "hand-written" in the output,
        # e.g. output type depends on some runtime value.
        # We can relax this if we assume the runtime values are always constants -- e.g. some attrs
        if s.name in ["Constant", "If", "Loop", "Scan", 
                      "ZipMap", 
                      "SequenceEmpty", "Cast", "CastMap", 
                      # "EyeLike", 
                      "TreeEnsembleClassifier", "LinearClassifier", "SVMClassifier",
                      "LabelEncoder", "CategoryMapper",
                      "LambOptimizer" # 1024 optional input groups
                      ]:
            print("!", end='')
            writeln(f";; SPECIAL: {s.name}")
            continue

        # 0.2 Further special cases, which just need some help in inferring the output type
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
            input_arg_types = []
            input_arg_names = []
            arg_comments = ""

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
            n_optional = 0
            n_variadic = 0
            for i in s.inputs:
                input_arg_names.append(i.name)

                mangler,ty = onnxTypes_to_mangler_and_Type(i.typeStr, i.types)
                input_arg_types.append(ty)

                if i.option == OpSchema.FormalParameterOption.Single:
                    opt = "single"
                if i.option == OpSchema.FormalParameterOption.Variadic:
                    opt = "variadic"
                    n_variadic += 1
                if i.option == OpSchema.FormalParameterOption.Optional:
                    n_optional += 1
                    opt = "optional"

                arg_comments += f"Arg<{i.name},{mangler},{opt}> "

            # 1.2: Attributes
            attr_arg_names = []
            attr_arg_types = []
            for key in s.attributes:
                a = s.attributes[key]
                attr_arg_names.append(a.name)

                attr_arg_types.append(onnxAttrType_to_Type(a.type))

                if a.required:
                    arg_comments += f"Attr<{a.name}> "
                else:
                    val = get_attribute_default_value(a)
                    if val == None:
                        warnings.warn(f"Optional attribute without default value {a.name}")
                        val = "**MISSING**"
    
                    arg_comments += f"Attr<Optional,{a.name},{val}> "

            # 1.3: Outputs
            return_types = []
            if out_type_from_sig == None:
                for o in s.outputs:
                    mangler,ty = onnxTypes_to_mangler_and_Type(o.typeStr, o.types)
                    if mangler != '':
                        writeln(f";; NOTE: output mangler {mangler}")
                    return_types.append(ty)
            else:
                return_types = [out_type_from_sig(input_arg_types)]

            n_outputs = len(s.outputs)
            n_optional_outputs = 0
            n_variadic_outputs = 0
            for o in s.outputs:
                if o.option == OpSchema.FormalParameterOption.Single:
                    opt = "single"
                if o.option == OpSchema.FormalParameterOption.Variadic:
                    opt = "variadic"
                    n_variadic_outputs += 1
                if o.option == OpSchema.FormalParameterOption.Optional:
                    n_optional_outputs += 1
                    opt = "optional"

            # Grab any manglers
            manglers = set([onnxType_to_Type_with_mangler(ty)[0] for ty in i.types for i in s.inputs + s.outputs])
            if '' in manglers:
                manglers.remove('')
            if len(manglers) > 0:
                writeln(f";; NOTE: manglers {manglers}")

            # 2: Def vs edef -- not in ort 1.5.2
            # if s.has_function:
            #     writeln(f";; has body: {type(s.function_body)}")

            # Emit actual signatures, one per optional arg, one per optional output
            for k_out in range(n_optional_outputs+1):
                # k_out = n_optional_outputs -- normal case, unmodified name
                take_n = n_outputs - n_optional_outputs + k_out
                if k_out == n_optional_outputs:
                    name = domain_prefix + s.name
                else:
                    name = f"take{take_n}$" + domain_prefix + s.name

                # Return multiple outputs as a tuple
                if take_n > 1:
                    return_type = Type.Tuple(*return_types[0:take_n])
                    writeln(f";; NOTE: multiple outputs as tuple")
                elif take_n == 1:
                    return_type = return_types[0]
                else:
                    return_type = Type.Tuple()  # Empty tuple is Knossos "unit" type

                writeln(f"; #out={k_out} ARGS: {arg_comments}")
                n_args = len(input_arg_types)
                for k in range(n_optional+1):
                    arg_types = input_arg_types[:n_args-k] + attr_arg_types
                    obj = EDef(name, return_type, arg_types)
                    pprint(obj, stream=prelude, width=1024, ribbon_width=1024, indent=2)

        if s.has_type_and_shape_inference_function:
            # out.append(EDef("shape$" + s.name, Type.Float, [Type.Float, Type.Vec(Type.Float)]))
            pass
        else:
            writeln(f"; No shape function for {s.name}\n")

    print("... done")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        filename = "etc/onnx-prelude-autogen.ks"
    else:
        filename = sys.argv[1]
    with open(filename, "w") as prelude:
        onnx_schemas_to_prelude(prelude)

# %%
