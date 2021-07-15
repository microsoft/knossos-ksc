"""
type_propagate: Type propagation for Knossos IR
"""

from typing import List
from ksc.type import (
    Type,
    shape_type,
    tangent_type,
    make_tuple_if_many,
    KSTypeError,
)

from ksc.expr import (
    ASTNode,
    Expr,
    Def,
    EDef,
    GDef,
    Rule,
    Var,
    Lam,
    Call,
    Let,
    If,
    Assert,
)
from ksc.expr import pystr, StructuredName
from ksc.prim import prim_lookup

import editdistance

# Pretty printing
# Importing prettyprint to get the decorated printers for Expression and Type
import ksc.prettyprint  # pylint: disable=unused-import

# Import the prettyprinter routine we use explicitly in this file
from prettyprinter import pformat

# Needed this in order to see the error messages when pprint fails
import warnings

warnings.filterwarnings("always")

############################################################################
# Oops, some of this code was written by functional programmers...
import sys

# import resource
# resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY,resource.RLIM_INFINITY))
sys.setrecursionlimit(10 ** 6)

from functools import singledispatch


def type_propagate(ex, symtab, respect_existing=False):
    """
    Fill "type" field(s) of "ex", propagating from and updating incoming dict "symtab"

    Keys of the symtab are names, either names of variables or StructuredNames
    Values in the symtab are types, the return types in the case of StructuredNames

    if respect_existing is True, and "ex" is an Expr whose type_ is already set (not None),
    then the existing type_ is left untouched and sub-Exprs are not examined.
    """
    return (
        ex
        if respect_existing and isinstance(ex, Expr) and ex.type_ is not None
        else _type_propagate_helper(ex, symtab, respect_existing)
    )


@singledispatch
def _type_propagate_helper(ex: ASTNode, symtab, respect_existing: bool):
    # Default implementation, for Expr subclasses not specialized below
    assert ex.type_ is not None
    return ex


# f : S -> T
# [rev f] : (S, dT) -> dS
# [fwd f] : (S, dS) -> dT
# [rev [fwd f]] : ((S, dS), dT) -> (dS, dS)


def infer_fn_type_from_derived_fn_args(sname: StructuredName, argtype: Type) -> Type:
    if sname.is_derivation():
        derivation = sname.se[0]

        # f : S-> T
        # [rev f] : (S, dT) -> dS
        if derivation == "rev":
            S = argtype.tuple_elem(0)
            return infer_fn_type_from_derived_fn_args(sname.se[1], S)

        # [fwd f] : (S, dS) -> dT
        if derivation == "fwd":
            S = argtype.tuple_elem(0)
            return infer_fn_type_from_derived_fn_args(sname.se[1], S)

        # [D f] : S -> LM dS dT
        if derivation == "D":
            S = argtype
            return infer_fn_type_from_derived_fn_args(sname.se[1], S)

        # [Dt f] : S -> LM dT dS
        if derivation == "Dt":
            S = argtype
            return infer_fn_type_from_derived_fn_args(sname.se[1], S)

        # [shape f] : S -> shapeType(S)
        if derivation == "shape":
            S = argtype
            return infer_fn_type_from_derived_fn_args(sname.se[1], S)

        # [suffwdpass f] : S -> (T, BOG)
        if derivation == "suffwdpass":
            S = argtype
            return infer_fn_type_from_derived_fn_args(sname.se[1], S)

        # If we can't deduce the base function argument type,
        # it must be specified in the structured name.
        return sname.get_type()

    if sname.has_type():
        return sname.se[1]
    else:
        return argtype


def add_type_to_sname(sname: StructuredName, argtype: Type) -> StructuredName:
    if sname.is_derivation():
        fn_type = infer_fn_type_from_derived_fn_args(sname, argtype)
    else:
        fn_type = argtype

    if fn_type is None:
        raise KSTypeError(f"Names should be fully specified in defs for {sname}")

    sname, old_ty = sname.add_type(fn_type)

    if old_ty is not None and old_ty != fn_type:
        raise KSTypeError(
            f"In definition of '{sname}', explicit type in structured name\n"
            + f"does not match argument type {fn_type} inferred from {argtype}"
        )
    return sname


@_type_propagate_helper.register(Def)
def _(ex, symtab, respect_existing):
    # (def name return_type args body)
    declared_return_type = ex.return_type != None

    # Name to look up is function "signature": name and argtypes
    argtypes = tuple(a.type_ for a in ex.args)
    argtype = make_tuple_if_many(argtypes)
    ex.name = add_type_to_sname(ex.name, argtype)

    # Check for definition in symtab with different return type
    if ex.name in symtab:
        old_type = symtab[ex.name]
        if old_type != ex.return_type:
            raise KSTypeError(
                f"Redefinition of {ex.name} with different return type {old_type} -> {ex.return_type}"
            )

    if declared_return_type:
        # Add to symtab before entering body, allowing for recursive calls
        symtab[ex.name] = ex.return_type

    # local_st: local symbol table to recurse into the body
    # Add args to local_st, after function was added to global st.
    # This defines that
    #   (def f Integer (f : Lam Integer Integer) (f 2))
    # is not a recursive call to f, but a call to the argument f
    local_st = {**symtab, **{a.name: a.type_ for a in ex.args}}

    # Recurse into body
    try:
        ex.body = type_propagate(ex.body, local_st, respect_existing)
    except KSTypeError as e:
        ctx = f"In definition of {ex.name} {pformat(argtypes)}\n"
        raise KSTypeError(ctx + str(e)) from e

    if declared_return_type:
        # Check the inferred type matches the decl
        if ex.return_type != ex.body.type_:
            raise KSTypeError(
                f"In definition of '{ex.name}', inferred return type {ex.body.type_}\n"
                + f"does not match declaration {ex.return_type}"
            )
    else:
        # Add to symtab after inference
        ex.return_type = ex.body.type_
        symtab[ex.name] = ex.return_type

    return ex


@_type_propagate_helper.register(EDef)
def _(ex, symtab, respect_existing):
    # (edef name return_type arg_type)
    ex.name = add_type_to_sname(ex.name, ex.arg_type)

    if ex.name in symtab and symtab[ex.name] != ex.return_type:
        raise KSTypeError(
            f"Double definition: {ex.name}\n -> {symtab[ex.name]}\n vs {ex.return_type}"
        )
    symtab[ex.name] = ex.return_type
    return ex


@_type_propagate_helper.register(GDef)
def _(ex, symtab, respect_existing):
    # (gdef derivation function_name)
    signature = StructuredName((ex.derivation, ex.function_name))
    # TODO: Need to map to return type.
    return_type = None
    if signature in symtab and symtab[signature] != return_type:
        raise KSTypeError(
            f"Double definition: {signature}\n -> {symtab[signature]}\n vs {return_type}"
        )
    symtab[signature] = return_type
    return ex


@_type_propagate_helper.register(Rule)
def _(ex, symtab, respect_existing):
    local_st = {**symtab, **{a.name: a.type_ for a in ex.template_vars}}
    type_propagate(ex.template, local_st, respect_existing)
    type_propagate(ex.replacement, local_st, respect_existing)
    return ex


@_type_propagate_helper.register(Var)
def _(ex, symtab, respect_existing):
    if ex.name not in symtab:
        raise KSTypeError(f"Unknown symbol {ex.name}")
    ex.type_ = symtab[ex.name]
    return ex


@_type_propagate_helper.register(Call)
def _(ex, symtab, respect_existing):
    for a in ex.args:
        type_propagate(a, symtab, respect_existing)
    argtypes = tuple(a.type_ for a in ex.args)
    argtype = make_tuple_if_many(argtypes)

    # Try prim lookup first - prims cannot be overloaded
    prim = prim_lookup(ex.name, argtype)
    if prim != None:
        ex.type_ = prim
        return ex

    # Add the type to the sname
    ex.name = add_type_to_sname(ex.name, argtype)

    # Check symbol table first
    if ex.name in symtab:
        ex.type_ = symtab[ex.name]
        return ex

    # Note: "derived functions", e.g. shape$foo, grad$foo, must be fully specified if, as is common, their signatures don't match foo

    # Not found, show what was found to improve error message
    argtypes_str = ",".join(map(pformat, argtypes))
    print(f"type_propagate: at ", pystr(ex, 2))
    print(
        f"type_propagate: Couldn't find {ex.name} called with types ({argtypes_str}) "
    )
    print(f"type_propagate: Looked up {ex.name}")
    exname = ex.name.mangled()
    near_miss = False
    closest_match = None
    closest_match_distance = float("inf")
    DISTANCE_THRESHOLD = 3
    for key, val in symtab.items():
        if isinstance(key, StructuredName):
            key_str = key.mangle_without_type()
        else:
            key_str = str(key)
        distance = editdistance.eval(key_str, exname)
        if distance < closest_match_distance:
            closest_match = key
            closest_match_distance = distance

        if distance < DISTANCE_THRESHOLD:
            if not near_miss:
                print(f"type_propagate: Near misses:")
                near_miss = True
            print(f"type_propagate:   {key_str} -> {val}")
    if not near_miss:
        print(
            f"type_propagate: No near misses (closest was {closest_match}, dist = {closest_match_distance})"
        )

    print(f"type_propagate: Boilerplate for prelude.ks:")
    argtypes_ks_str = " ".join(map(pformat, argtypes))
    argtype_tuple = Type.Tuple(*argtypes) if len(argtypes) > 1 else argtypes[0]
    argtypes_ks_tuple = pformat(argtype_tuple)
    argtypes_ks_tangent_tuple = pformat(tangent_type(argtype_tuple))

    print(f"(edef {ex.name} RET ({argtypes_ks_str}))")
    print(
        f"(edef D${ex.name} (LM {argtypes_ks_tangent_tuple} dRET) ({argtypes_ks_str}))"
    )
    print(
        f"(def rev${ex.name} {argtypes_ks_tangent_tuple} ((t : {argtypes_ks_tuple}) (dret : dRET))"
    )
    print(f"   )")

    raise KSTypeError(f"Couldn't find {ex.name} applied to ({argtypes_str}) at {ex}")


@_type_propagate_helper.register(Lam)
def _(ex, symtab, respect_existing):
    local_st = {**symtab, ex.arg.name: ex.arg.type_}
    ex.body = type_propagate(ex.body, local_st, respect_existing)
    ex.type_ = Type.Lam(ex.arg.type_, ex.body.type_)
    return ex


@_type_propagate_helper.register(Let)
def _(ex, symtab, respect_existing):
    ex.rhs = type_propagate(ex.rhs, symtab, respect_existing)

    # Single var assignment
    if isinstance(ex.vars, Var):
        ex.vars.type_ = ex.rhs.type_
        local_st = {**symtab, ex.vars.name: ex.vars.type_}
        ex.body = type_propagate(ex.body, local_st, respect_existing)
        ex.type_ = ex.body.type_
        return ex

    # Tuple assignment -- incoming type should be tuple of same size
    if isinstance(ex.vars, list):
        assert len(ex.vars) == ex.rhs.type_.tuple_len if ex.rhs.type_ else True
        local_st = symtab.copy()
        for i, var in enumerate(ex.vars):
            var.type_ = ex.rhs.type_.tuple_elem(i) if ex.rhs.type_ else None
            local_st[var.name] = var.type_

        ex.body = type_propagate(ex.body, local_st, respect_existing)
        ex.type_ = ex.body.type_
        return ex

    assert False  # Bad var


@_type_propagate_helper.register(If)
def _(ex, symtab, respect_existing):
    ex.cond = type_propagate(ex.cond, symtab, respect_existing)
    assert ex.cond.type_ == Type.Bool
    ex.t_body = type_propagate(ex.t_body, symtab, respect_existing)
    ex.f_body = type_propagate(ex.f_body, symtab, respect_existing)
    assert ex.t_body.type_ == ex.f_body.type_
    ex.type_ = ex.t_body.type_
    return ex


@_type_propagate_helper.register(Assert)
def _(ex, symtab, respect_existing):
    ex.cond = type_propagate(ex.cond, symtab, respect_existing)
    assert ex.cond.type_ == Type.Bool
    ex.body = type_propagate(ex.body, symtab, respect_existing)
    ex.type_ = ex.body.type_
    return ex


def type_propagate_decls(decls: List[Expr], symtab):
    for decl in decls:
        type_propagate(decl, symtab)
