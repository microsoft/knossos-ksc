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

def warn(msg):
    print("ksc: warning: ", msg)

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
            return tys[0].Child(n-1)

    # vec
    if name == "vec":
        assert all(ty == tys[0] for ty in tys)
        return Type.Vec(tys[0])

    if n == 1 and name == "size":
        assert tys[0].kind == "Vec"
        return Type.Integer

    if n == 2 and name == "index":
        assert tys[0].kind == "Integer"
        return Type.Index(tys[1])

    if n == 2 and name == "build":
        assert tys[0].kind == "Integer"
        assert tys[1].kind == "Lam"
        return Type.Vec(tys[1].return_type)

    # Polymorphic arithmetic
    if n == 1 and name == "sum":
        return Type.Index(tys[0])

    if n == 2 and name == "ts_add":
        # assert tys[0] == tys[1] TODO: check rhs is tangent_type
        return tys[0]

    if n == 2 and name == "ts_scale":
        assert tys[0].kind == "Float"
        # assert tys[0] == tys[1] TODO: check rhs is tangent_type
        return tys[1]

    # Print
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
    assert ex.type != None
    return ex

@typeannot.register(Def)
def _(ex, symtab):
    # name args body
    local_st = symtab.copy()
    for a in ex.args:
        local_st[a.name] = a.type
    ex.body = typeannot(ex.body, local_st)
    if ex.type == None:
        ex.type = ex.body.type
    else:
        assert ex.type == ex.body.type
    
    argtypes = tuple(a.type for a in ex.args)
    symtab[ex.name, argtypes] = ex.type
    return ex

@typeannot.register(EDef)
def _(ex, symtab):
    key = ex.name, tuple(ex.arg_types)
    if key in symtab and symtab[key] != ex.type:
        raise NotImplementedError(f"Double edef: {key}\n -> {symtab[key]}\n vs {ex.type}")
    symtab[key] = ex.type
    return ex

@typeannot.register(Rule)
def _(ex, symtab):
    return ex

@typeannot.register(Var)
def _(ex, symtab):
    if ex.decl:
        if not ex.type:
            warn(f"Variable decl {ex.name} without type")
        return ex

    # Use, not declaration -- should find it in the symbol table
    if ex.name not in symtab:
        warn(f"Variable not found {ex.name}")
        return ex
    
    ex.type = symtab[ex.name]

    if ex.type == None:
        # Don't warn, this is because of an earlier error
        #  warn(f"Variable found, but type {ex.name}")
        pass

    return ex

@typeannot.register(Call)
def _(ex, symtab):
    for a in ex.args:
        typeannot(a, symtab)
    argtypes = tuple(a.type for a in ex.args)

    if None in argtypes:
        # Annotation failed further down, just silently exit
        ex.type = None
        return ex

    key = ex.name, argtypes

    # Check symbol table first
    if key in symtab:
        ex.type = symtab[key]
        return ex

    # TODO: do "derived functions", e.g. shape$foo, grad$foo? 

    # Try prim lookup
    prim = ks_prim_lookup(ex.name, argtypes)
    if prim != None:
        ex.type = prim
        return ex

    argtypes_str = " ".join(map(pformat, argtypes))
    warn(f"Couldn't find operator: ({ex.name} {argtypes_str})")
    # Print near misses
    for key,val in symtab.items():
        if isinstance(key, tuple):
            name = key[0]
            found_args = " ".join(map(pformat, key[1]))
        else:
            name = key
            found_args = ""

        if name.startswith(ex.name):
            warn(f"   Found ({name} {found_args}) -> {val}")

    ex.type = None
    return ex

@typeannot.register(Lam)
def _(ex, symtab):
    local_st = symtab.copy()
    local_st[ex.arg.name] = ex.arg.type
    ex.body = typeannot(ex.body, local_st)
    ex.type = Type.Lam(ex.arg.type, ex.body.type)
    return ex

@typeannot.register(Let)
def _(ex, symtab):
    ex.rhs = typeannot(ex.rhs, symtab)
    assert isinstance(ex.vars, Var)
    ex.vars.type = ex.rhs.type
    local_st = symtab.copy()
    local_st[ex.vars.name] = ex.vars.type
    ex.body = typeannot(ex.body, local_st)
    ex.type = ex.body.type
    return ex

@typeannot.register(If)
def _(ex, symtab):
    ex.cond = typeannot(ex.cond, symtab)
    assert ex.cond.type == Type.Bool
    ex.t_body = typeannot(ex.t_body, symtab)
    ex.f_body = typeannot(ex.f_body, symtab)
    assert ex.t_body.type == ex.f_body.type
    ex.type = ex.t_body.type
    return ex

@typeannot.register(Assert)
def _(ex, symtab):
    ex.cond = typeannot(ex.cond, symtab)
    assert ex.cond.type == Type.Bool
    ex.body = typeannot(ex.body, symtab)
    ex.type = ex.body.type
    return ex

def typeannot_decls(decls : List[Expr], symtab = dict()):
    for decl in decls:
        typeannot(decl, symtab)
    return symtab

if __name__ == "__main__":
    from ksc.parse_ks import parse_ks_file
    symtab = dict()
    decls_prelude = parse_ks_file("src/runtime/prelude.ks")
    typeannot_decls(decls_prelude, symtab)
    decls_file = list(parse_ks_file("test/ksc/gmm-obj.ks"))
    typeannot_decls(decls_file, symtab)
    for decl in decls_file:
        cpprint(decl)
