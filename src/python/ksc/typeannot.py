"""
typeannot: Type annotation for Knossos IR
"""

import itertools
from typing import Union, List
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
            assert len(tys[0]) == max
            return tys[0].tuple_elem(n-1)

    # vec
    if name == "vec":
        assert all(ty == tys[0] for ty in tys)
        return Type.Vec(tys[0])

    if n == 1 and name == "size":
        assert tys[0].kind == "Vec"
        return Type.Integer

    # index : Int, Vec T -> T
    if n == 2 and name == "index":
        assert tys[0].kind == "Integer"
        return Type.Index(tys[1])

    # build : Int, Lam Int T -> Vec T
    if n == 2 and name == "build":
        assert tys[0].kind == "Integer"
        assert tys[1].kind == "Lam"
        return Type.Vec(tys[1].lam_return_type)

    # fold : Lam (Tuple State T) State, State, Vec T -> State
    if n == 3 and name == "fold":
        assert tys[0].kind == "Lam"
        assert tys[0].lam_arg_type.tuple_len() == 2
        tyState = tys[0].lam_arg_type.tuple_elem(0)
        tyT = tys[0].lam_arg_type.tuple_elem(1)
        assert tys[0].lam_return_type == tyState
        assert tys[1] == tyState
        assert tys[2].is_vec_of(tyT)
        return tyState

    # eq : T, T -> Bool
    if n == 2 and name == "eq":
        assert tys[0] == tys[1] # TODO: MOVEEQ Move eq to prelude
        return Type.Bool

    # Polymorphic arithmetic
    # sum : Vec T -> T
    if n == 1 and name == "sum" and tys[0].is_vec:
        return Type.Index(tys[0])

    # ts_add : T, dT -> T
    if n == 2 and name == "ts_add":
        # assert tys[0] == tys[1] TODO: check rhs is tangent_type
        return tys[0]

    # ts_add : Float, dT -> dT
    if n == 2 and name == "ts_scale":
        assert tys[0].kind == "Float"
        return tys[1]

    # print : T... -> Int
    if name == "print":
        return Type.Integer

    return None

############################################################################
from functools import singledispatch

class TypeError(RuntimeError):
    pass

@singledispatch
def typeannot(ex, symtab):
    """
    Fill "type" field of expr, propagating from incoming dict
    """
    # Default implementation, for types not specialized below
    assert ex.type_ != None
    return ex

@typeannot.register(Def)
def _(ex, symtab):
    # (def name retun_type args body)
    assert ex.return_type != None

    # Add this function's name to symtab
    # Do it before entering body, allowing for recursive calls
    argtypes = tuple(a.type_ for a in ex.args)
    
    # Check for redefinition with different return type 
    signature = (ex.name, argtypes)
    if signature in symtab:
        old_type = symtab[signature]
        if old_type != ex.return_type:
            raise TypeError(f"Redefinition of {ex.name}({argtypes}) with different return type {old_type} -> {ex.return_type}")
    
    symtab[signature] = ex.return_type

    # local_st: local symbol table to recurse into the body
    local_st = symtab.copy()
    # Add args to local_st, after function was added to global st. 
    # This defines that
    #   (def f Integer (f : Lam Integer Integer) (f 2))
    # is not a recursive call to f, but a call to the argument f
    for a in ex.args:
        local_st[a.name] = a.type_

    # Recurse into body
    ex.body = typeannot(ex.body, local_st)

    # And check the inferred type matches the decl
    if ex.return_type != ex.body.type_:
        raise TypeError(f"In definition of '{ex.name}', inferred return type {ex.body.type_}\n" +
                        f"does not match declaration {ex.return_type}")
    
    return ex

@typeannot.register(EDef)
def _(ex, symtab):
    # (edef name retun_type arg_types)
    signature = ex.name, tuple(ex.arg_types)
    if signature in symtab and symtab[signature] != ex.return_type:
        raise NotImplementedError(f"Double edef: {key}\n -> {symtab[key]}\n vs {ex.return_type}")
    symtab[signature] = ex.return_type
    return ex

@typeannot.register(Rule)
def _(ex, symtab):
    # TODO: Typeannot for rules
    return ex

@typeannot.register(Var)
def _(ex, symtab):
    if ex.decl:
        assert ex.type_ != None
    else:
        ex.type_ = symtab[ex.name]
    return ex

@typeannot.register(Call)
def _(ex, symtab):
    for a in ex.args:
        typeannot(a, symtab)
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
    print(f"typeannot: at ", pystr(ex, 2))
    print(f"typeannot: Couldn't find {ex.name}({argtypes_str}) ")
    print(f"typeannot: Near misses:")
    for key,val in symtab.items():
        if isinstance(key, tuple):
            key=key[0]
        if key.startswith(ex.name): # TODO: soundex match here?
            print(f"typeannot:   {key}({val})")

    raise TypeError(f"Couldn't find {ex.name}({argtypes_str}) at ", pystr(ex, 2))

@typeannot.register(Lam)
def _(ex, symtab):
    local_st = symtab.copy()
    local_st[ex.arg.name] = ex.arg.type_
    ex.body = typeannot(ex.body, local_st)
    ex.type_ = Type.Lam(ex.arg.type_, ex.body.type_)
    return ex

@typeannot.register(Let)
def _(ex, symtab):
    ex.rhs = typeannot(ex.rhs, symtab)

    # Single var assignment
    if isinstance(ex.vars, Var):
        ex.vars.type_ = ex.rhs.type_
        local_st = symtab.copy()
        local_st[ex.vars.name] = ex.vars.type_
        ex.body = typeannot(ex.body, local_st)
        ex.type_ = ex.body.type_
        return ex

    # Tuple assignment -- incoming type should be tuple of same size
    if isinstance(ex.vars, list):
        assert len(ex.vars) == ex.rhs.type_.tuple_len() if ex.rhs.type_ else True
        local_st = symtab.copy()
        for i,var in enumerate(ex.vars):
            var.type_ = ex.rhs.type_.tuple_elem(i) if ex.rhs.type_ else None
            local_st[var.name] = var.type_
        
        ex.body = typeannot(ex.body, local_st)
        ex.type_ = ex.body.type_
        return ex

    assert False # Bad var   

@typeannot.register(If)
def _(ex, symtab):
    ex.cond = typeannot(ex.cond, symtab)
    assert ex.cond.type_ == Type.Bool
    ex.t_body = typeannot(ex.t_body, symtab)
    ex.f_body = typeannot(ex.f_body, symtab)
    assert ex.t_body.type_ == ex.f_body.type_
    ex.type_ = ex.t_body.type_
    return ex

@typeannot.register(Assert)
def _(ex, symtab):
    ex.cond = typeannot(ex.cond, symtab)
    assert ex.cond.type_ == Type.Bool
    ex.body = typeannot(ex.body, symtab)
    ex.type_ = ex.body.type_
    return ex

def typeannot_decls(decls : List[Expr], symtab = dict()):
    for decl in decls:
        typeannot(decl, symtab)
    return symtab
