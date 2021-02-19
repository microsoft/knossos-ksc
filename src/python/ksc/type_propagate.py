"""
type_propagate: Type propagation for Knossos IR
"""

import itertools
from typing import Union, List
from ksc.type import Type, SizeType, shape_type

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
import sys
import resource
# resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY,resource.RLIM_INFINITY))
sys.setrecursionlimit(10**6)

import re
ks_prim_lookup_re_get = re.compile(r"get\$(\d+)\$(\d+)")

def ks_prim_lookup(name, tys):
    n = len(tys)

    # tuple
    if name == "tuple":
        return Type.Tuple(*tys)

    # get$n$m
    if n == 1 and name.startswith("get$"):
        m = ks_prim_lookup_re_get.fullmatch(name)
        if m:
            n = int(m.group(1))
            max = int(m.group(2))
            assert tys[0].tuple_len == max
            return tys[0].tuple_elem(n-1)

    # vec
    if name == "Vec_init":
        assert all(ty == tys[0] for ty in tys)
        return Type.Tensor(1, tys[0])

    # size : Tensor N T -> Size
    if n == 1 and name == "size":
        return SizeType.from_rank(tys[0].tensor_rank)

    if n == 1 and name == "shape":
        return shape_type(tys[0])

    # index : Size, Tensor N T -> T
    if n == 2 and name == "index":
        assert tys[0] == SizeType.from_rank(tys[1].tensor_rank)
        return tys[1].tensor_elem_type

    # build : Size, Lam IntTuple T -> Tensor T
    if n == 2 and name == "build":
        size_ty = tys[0]
        lam_ty = tys[1]
        assert lam_ty.lam_arg_type == size_ty

        rank = SizeType.get_rank(size_ty)
        assert rank is not None
        
        elem_type = lam_ty.lam_return_type
        return Type.Tensor(rank, elem_type)
        
    # sumbuild : Size, Lam IntTuple T -> T
    if n == 2 and name == "sumbuild":
        size_ty = tys[0]
        lam_ty = tys[1]
        assert lam_ty.lam_arg_type == size_ty

        return lam_ty.lam_return_type
        
    # constVec(n T)
    if n == 2 and name == "constVec":
        size_ty = tys[0]
        elem_ty = tys[1]

        rank = SizeType.get_rank(size_ty)
        assert rank is not None
        
        return Type.Tensor(rank, elem_ty)

    # deltaVec(n i v)
    if n == 3 and name == "deltaVec":
        size_ty = tys[0]
        ind_ty = tys[1]
        elem_ty = tys[2]
        assert size_ty == ind_ty
        rank = SizeType.get_rank(size_ty)
        assert rank is not None
        
        return Type.Tensor(rank, elem_ty)

    # fold : Lam (Tuple State T) State, State, Tensor T -> State
    if n == 3 and name == "fold":
        assert tys[0].is_lam_or_LM
        assert tys[0].lam_arg_type.tuple_len == 2
        tyState = tys[0].lam_arg_type.tuple_elem(0)
        tyT = tys[0].lam_arg_type.tuple_elem(1)
        assert tys[0].lam_return_type == tyState
        assert tys[1] == tyState
        assert tys[2].is_tensor_of(tyT)
        return tyState

    # eq : T, T -> Bool
    if n == 2 and name in ("eq", "lt", "gt", "lte", "gte"):
        assert tys[0] == tys[1] # TODO: MOVEEQ Move eq to prelude
        return Type.Bool

    # Polymorphic arithmetic
    # sum : Tensor T -> T
    if n == 1 and name == "sum" and tys[0].is_tensor:
        return tys[0].tensor_elem_type

    # ts_add : T, dT -> T
    if n == 2 and name == "ts_add":
        # assert tys[0] == tys[1] TODO: check rhs is tangent_type
        return tys[0]

    # ts_dot : T, T -> Float
    if n == 2 and name == "ts_dot":
        assert tys[0] == tys[1]
        return Type.Float

    # ts_scale : Float, dT -> dT
    if n == 2 and name == "ts_scale":
        assert tys[0] == Type.Float
        return tys[1]

    # print : T... -> Int
    if name == "print":
        return Type.Integer

    return None

############################################################################
from functools import singledispatch

class KSTypeError(RuntimeError):
    pass

@singledispatch
def type_propagate(ex, symtab):
    """
    Fill "type" field of expr, propagating from incoming dict
    """
    # Default implementation, for types not specialized below
    assert ex.type_ != None
    return ex

@type_propagate.register(Def)
def _(ex, symtab):
    # (def name return_type args body)
    declared_return_type = ex.return_type != None

    # Name to look up is function "signature": name and argtypes
    argtypes = tuple(a.type_ for a in ex.args)
    signature = (ex.name, argtypes)

    # Check for definition in symtab with different return type 
    if signature in symtab:
        old_type = symtab[signature]
        if old_type != ex.return_type:
            raise KSTypeError(f"Redefinition of {ex.name}({argtypes}) with different return type {old_type} -> {ex.return_type}")
    
    if declared_return_type:
        # Add to symtab before entering body, allowing for recursive calls
        symtab[signature] = ex.return_type

    # local_st: local symbol table to recurse into the body
    # Add args to local_st, after function was added to global st. 
    # This defines that
    #   (def f Integer (f : Lam Integer Integer) (f 2))
    # is not a recursive call to f, but a call to the argument f
    local_st = {**symtab, **{a.name:a.type_ for a in ex.args}}

    # Recurse into body
    try:
        ex.body = type_propagate(ex.body, local_st)
    except KSTypeError as e:
        ctx = f"In definition of {ex.name} {pformat(argtypes)}\n"
        raise KSTypeError(ctx + str(e)) from e
    
    if declared_return_type: 
        # Check the inferred type matches the decl
        if ex.return_type != ex.body.type_:
            raise KSTypeError(f"In definition of '{ex.name}', inferred return type {ex.body.type_}\n" +
                              f"does not match declaration {ex.return_type}")
    else:
        # Add to symtab after inference
        ex.return_type = ex.body.type_
        symtab[signature] = ex.return_type

    return ex

@type_propagate.register(EDef)
def _(ex, symtab):
    # (edef name retun_type arg_types)
    signature = ex.name, tuple(ex.arg_types)
    if signature in symtab and symtab[signature] != ex.return_type:
        raise KSTypeError(f"Double definition: {signature}\n -> {symtab[signature]}\n vs {ex.return_type}")
    symtab[signature] = ex.return_type
    return ex

@type_propagate.register(Rule)
def _(ex, symtab):
    # TODO: Typeannot for rules
    return ex

@type_propagate.register(Var)
def _(ex, symtab):
    if ex.decl:
        assert ex.type_ != None
    else:
        if ex.name not in symtab:
            raise KSTypeError(f"Unknown symbol {ex.name}")
        ex.type_ = symtab[ex.name]
    return ex

@type_propagate.register(Call)
def _(ex, symtab):
    for a in ex.args:
        type_propagate(a, symtab)
    argtypes = tuple(a.type_ for a in ex.args)
    key = ex.name, argtypes

    # Check symbol table first
    if key in symtab:
        ex.type_ = symtab[key]
        return ex

    # TODO: do "derived functions", e.g. shape$foo, grad$foo? 

    # Try prim lookup
    prim = ks_prim_lookup(ex.name, argtypes)
    if prim != None:
        ex.type_ = prim
        return ex

    # Not found, show what was found to improve error message
    argtypes_str = ",".join(map(pformat, argtypes))
    print(f"type_propagate: at ", pystr(ex, 2))
    print(f"type_propagate: Couldn't find {ex.name}({argtypes_str}) ")
    print(f"type_propagate: Near misses:")
    for key,val in symtab.items():
        if isinstance(key, tuple):
            key=key[0]
        if key.startswith(ex.name): # TODO: soundex match here?
            print(f"type_propagate:   {key}({val})")

    raise KSTypeError(f"Couldn't find {ex.name}({argtypes_str}) at ", pystr(ex, 2))

@type_propagate.register(Lam)
def _(ex, symtab):
    local_st = {**symtab, ex.arg.name: ex.arg.type_}
    ex.body = type_propagate(ex.body, local_st)
    ex.type_ = Type.Lam(ex.arg.type_, ex.body.type_)
    return ex

@type_propagate.register(Let)
def _(ex, symtab):
    ex.rhs = type_propagate(ex.rhs, symtab)

    # Single var assignment
    if isinstance(ex.vars, Var):
        ex.vars.type_ = ex.rhs.type_
        local_st = {**symtab, ex.vars.name: ex.vars.type_}
        ex.body = type_propagate(ex.body, local_st)
        ex.type_ = ex.body.type_
        return ex

    # Tuple assignment -- incoming type should be tuple of same size
    if isinstance(ex.vars, list):
        assert len(ex.vars) == ex.rhs.type_.tuple_len if ex.rhs.type_ else True
        local_st = symtab.copy()
        for i,var in enumerate(ex.vars):
            var.type_ = ex.rhs.type_.tuple_elem(i) if ex.rhs.type_ else None
            local_st[var.name] = var.type_
        
        ex.body = type_propagate(ex.body, local_st)
        ex.type_ = ex.body.type_
        return ex

    assert False # Bad var   

@type_propagate.register(If)
def _(ex, symtab):
    ex.cond = type_propagate(ex.cond, symtab)
    assert ex.cond.type_ == Type.Bool
    ex.t_body = type_propagate(ex.t_body, symtab)
    ex.f_body = type_propagate(ex.f_body, symtab)
    assert ex.t_body.type_ == ex.f_body.type_
    ex.type_ = ex.t_body.type_
    return ex

@type_propagate.register(Assert)
def _(ex, symtab):
    ex.cond = type_propagate(ex.cond, symtab)
    assert ex.cond.type_ == Type.Bool
    ex.body = type_propagate(ex.body, symtab)
    ex.type_ = ex.body.type_
    return ex

def type_propagate_decls(decls : List[Expr], symtab):
    for decl in decls:
        type_propagate(decl, symtab)
