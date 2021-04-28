from functools import singledispatch
from hashlib import sha224
from typing import Mapping

from ksc.cav_subst import get_children
from ksc.expr import  Expr, Var, Let, Lam
from ksc.match_filter import match_filter

def are_alpha_equivalent(exp1, exp2):
    return _alpha_equivalence_helper(exp1, exp2, {}, {})

# Implementation uses two maps to ensure a bijection between bound variables in 'left' and 'right'.

def _alpha_equivalence_helper(left : Expr, right : Expr, l_to_r_bound_vars: Mapping[str, str], r_to_l_bound_vars: Mapping[str, str]) -> bool:
    return (left is right) or _alpha_equivalence_traversal(left, right, l_to_r_bound_vars, r_to_l_bound_vars)

@singledispatch
def _alpha_equivalence_traversal(left : Expr, right : Expr, l_to_r_bound_vars: Mapping[str, str], r_to_l_bound_vars: Mapping[str, str]) -> bool:
    if match_filter(left) != match_filter(right):
        return False
    l_children, r_children = get_children(left), get_children(right)
    return len(l_children) == len(r_children) and all(
        _alpha_equivalence_helper(l_child, r_child, l_to_r_bound_vars, r_to_l_bound_vars)
        for l_child, r_child in zip(l_children, r_children)
    )

@_alpha_equivalence_traversal.register
def _alpha_equiv_var(left: Var, right: Expr, l_to_r_bound_vars: Mapping[str, str], r_to_l_bound_vars: Mapping[str, str]) -> bool:
    return (isinstance(right, Var) and
        # The two checks ensure the bound variables are used 1:1 - see test_alpha_equivalence_shadows_free
        # and the defaults passed to get() allow free variables of the same name.
        l_to_r_bound_vars.get(left.name, left.name) == right.name and
        r_to_l_bound_vars.get(right.name, right.name) == left.name and
        left.type_ == right.type_)

@_alpha_equivalence_traversal.register
def _alpha_equiv_let(left: Let, right: Expr, l_to_r_bound_vars: Mapping[str, str], r_to_l_bound_vars: Mapping[str, str]) -> bool:
    if not isinstance(right, Let):
        return False
    assert isinstance(left.vars, Var), "Tupled-lets are not supported: call untuple_lets first"
    assert isinstance(right.vars, Var), "Tupled-lets are not supported: call untuple_lets first"
    return (_alpha_equivalence_helper(left.rhs, right.rhs, l_to_r_bound_vars, r_to_l_bound_vars) and
        _alpha_equivalence_helper(left.body, right.body,
            {**l_to_r_bound_vars, left.vars.name: right.vars.name},
            {**r_to_l_bound_vars, right.vars.name: left.vars.name})
    )

@_alpha_equivalence_traversal.register
def _alpha_equiv_lam(left: Lam, right: Expr, l_to_r_bound_vars: Mapping[str, str], r_to_l_bound_vars: Mapping[str, str]) -> bool:
    return _alpha_equivalence_helper(left.body, right.body,
        {**l_to_r_bound_vars, left.arg.name: right.arg.name},
        {**r_to_l_bound_vars, right.arg.name: left.arg.name})

###############################################################################
# Hash modulo alpha, i.e. respecting the above:
#    are_alpha_equivalent(a,b) ==> alpha_hash(a) == alpha_hash(b)

def _hash_str(s: str) -> int:
    # Use a hash that's repeatable across python VM invocations:
    # python's hash() for strings depends upon a seed randomly chosen at startup
    # (unless overridden with PYTHONHASHSEED environment variable).
    hex: str = sha224(s.encode()).hexdigest()
    return int(hex[:16], base=16)

# The hashing routines below use deBruijn indices stored in a map whose values are reversed:
# the outermost (non-shadowed) binder has stored value 0,
# the innermost (closest) binder has stored value (the number of keys in the map, minus 1).
# Thus, the deBruijn number (closest-bound variable = 1, then increasing) is (size of map) - (stored value).
# This allows adding variables without changing values existing in the map.
def _add_var(reverse_debruijn_vars: Mapping[str, int], varname: str) -> Mapping[str, int]:
    # TODO: python3.9 brings 'frozenmap' which would have better performance characteristics here.
    return {**reverse_debruijn_vars, varname: len(reverse_debruijn_vars)}

@singledispatch
def _alpha_hash_helper(e: Expr, reverse_debruijn_vars: Mapping[str, int]) -> int:
    # Default case. The match_filter and the children suffices for anything that isn't or doesn't bind a variable.
    # However, the match_filter may be a class, the hash of which seems to be different each run
    # (even if PYTHONHASHSEED is specified!), so use repeatable string hashing above.
    node_type_hash = _hash_str(str(match_filter(e)))
    return hash((node_type_hash, *(_alpha_hash_helper(ch, reverse_debruijn_vars) for ch in get_children(e))))

@_alpha_hash_helper.register
def _alpha_hash_var(v: Var, reverse_debruijn_vars: Mapping[str, int]) -> int:
    reverse_debruijn_idx = reverse_debruijn_vars.get(v.name)
    if reverse_debruijn_idx is not None:
        return len(reverse_debruijn_vars) - reverse_debruijn_idx
    # Free variable.
    return _hash_str(v.name)

@_alpha_hash_helper.register
def _alpha_hash_let(l: Let, reverse_debruijn_vars: Mapping[str, int]) -> int:
    return hash(("let",
        _alpha_hash_helper(l.rhs, reverse_debruijn_vars),
        _alpha_hash_helper(l.body, _add_var(reverse_debruijn_vars, l.vars.name))))

@_alpha_hash_helper.register
def _alpha_hash_lam(l: Lam, reverse_debruijn_vars: Mapping[str, int]) -> int:
    return hash(("lam", _alpha_hash_helper(l.body, _add_var(reverse_debruijn_vars, l.arg.name))))

def alpha_hash(e: Expr):
    return _alpha_hash_helper(e, {})
