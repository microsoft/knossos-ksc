from functools import singledispatch
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
