from functools import singledispatch
from hashlib import sha224
from typing import NamedTuple, Type

from pyrsistent import pmap
from pyrsistent.typing import PMap

from ksc.cav_subst import get_children
from ksc.expr import Expr, Var, Let, Lam, Call, If, Assert, Const
from ksc.filter_term import get_filter_term


def are_alpha_equivalent(exp1, exp2):
    return _alpha_equivalence_helper(exp1, exp2, BoundVarBijection(pmap(), pmap()))


# We use this ensure a bijection between bound variables in 'left' and 'right' exprs.
class BoundVarBijection(NamedTuple):
    left_to_right: PMap[str, str]
    right_to_left: PMap[str, str]

    def vars_equal(self, left: str, right: str) -> bool:
        # Allow left == right IF neither is in the bijection
        return self.left_to_right.get(left, left) == right and self.right_to_left.get(right, right) == left

    def add_var(self, left, right):
        return BoundVarBijection(self.left_to_right.set(left, right), self.right_to_left.set(right, left))


def _alpha_equivalence_helper(left: Expr, right: Expr, var_map: BoundVarBijection) -> bool:
    return (left is right) or _alpha_equivalence_traversal(left, right, var_map)


@singledispatch
def _alpha_equivalence_traversal(left: Expr, right: Expr, var_map: BoundVarBijection) -> bool:
    if get_filter_term(left) != get_filter_term(right):
        return False
    l_children, r_children = get_children(left), get_children(right)
    return len(l_children) == len(r_children) and all(
        _alpha_equivalence_helper(l_ch, r_ch, var_map) for l_ch, r_ch in zip(l_children, r_children)
    )


@_alpha_equivalence_traversal.register
def _alpha_equiv_var(left: Var, right: Expr, var_map: BoundVarBijection) -> bool:
    # The check ensures the bound variables are used 1:1 - see test_alpha_equivalence_shadows_free
    return isinstance(right, Var) and var_map.vars_equal(left.name, right.name)
    # Do not check type, consistent with ==


@_alpha_equivalence_traversal.register
def _alpha_equiv_let(left: Let, right: Expr, var_map: BoundVarBijection) -> bool:
    if not isinstance(right, Let):
        return False
    assert isinstance(left.vars, Var), "Tupled-lets are not supported: call untuple_lets first"
    assert isinstance(right.vars, Var), "Tupled-lets are not supported: call untuple_lets first"
    return _alpha_equivalence_helper(left.rhs, right.rhs, var_map) and _alpha_equivalence_helper(
        left.body, right.body, var_map.add_var(left.vars.name, right.vars.name)
    )


@_alpha_equivalence_traversal.register
def _alpha_equiv_lam(left: Lam, right: Expr, var_map: BoundVarBijection) -> bool:
    return _alpha_equivalence_helper(left.body, right.body, var_map.add_var(left.arg.name, right.arg.name))


###############################################################################
# Hash modulo alpha, i.e. respecting the above:
#    are_alpha_equivalent(a,b) ==> alpha_hash(a) == alpha_hash(b)


def _hash_str(s: str) -> int:
    # Use a hash that's repeatable across python VM invocations:
    # python's hash() for strings depends upon a seed randomly chosen at startup
    # (unless overridden with PYTHONHASHSEED environment variable).
    hex: str = sha224(s.encode()).hexdigest()
    return int(hex[:16], base=16)


# For most Expr-subclasses we use a repeatable hash of the class name.
# (Directly hashing the class is not repeatable across python VM invocations even if PYTHONHASHSEED is fixed.)
# Cache that here.
_class_hashes: PMap[Type[Expr], int] = pmap({clazz: _hash_str(clazz.__name__) for clazz in [If, Assert, Let, Lam]})

# The hashing routines below use deBruijn indices stored in a map whose values are reversed:
# the outermost (non-shadowed) binder has stored value 0,
# the innermost (closest) binder has stored value (the number of keys in the map, minus 1).
# Thus, the deBruijn number (closest-bound variable = 1, then increasing) is (size of map) - (stored value).
# This allows adding variables without changing values existing in the map.
def _add_var(reverse_debruijn_vars: PMap[str, int], varname: str) -> PMap[str, int]:
    return reverse_debruijn_vars.set(varname, len(reverse_debruijn_vars))


def _hash_children(node_type_hash: int, e: Expr, reverse_debruijn_vars: PMap[str, int]) -> int:
    return hash((node_type_hash, *(_alpha_hash_helper(ch, reverse_debruijn_vars) for ch in get_children(e))))


@singledispatch
def _alpha_hash_helper(e: Expr, reverse_debruijn_vars: PMap[str, int]) -> int:
    # Default case.
    return _hash_children(_class_hashes[e.__class__], e, reverse_debruijn_vars)


@_alpha_hash_helper.register
def _alpha_hash_const(c: Const, reverse_debruijn_vars: PMap[str, int]) -> int:
    # System hash() is repeatable for int/float but not str.
    return _hash_str(c.value) if isinstance(c.value, str) else hash(c.value)


@_alpha_hash_helper.register
def _alpha_hash_call(c: Call, reverse_debruijn_vars: PMap[str, int]) -> int:
    # StructuredName can be hash()'d directly but again this depends on PYTHONHASHSEED.
    return _hash_children(_hash_str(str(c.name)), c, reverse_debruijn_vars)


@_alpha_hash_helper.register
def _alpha_hash_var(v: Var, reverse_debruijn_vars: PMap[str, int]) -> int:
    reverse_debruijn_idx = reverse_debruijn_vars.get(v.name)
    if reverse_debruijn_idx is not None:
        return len(reverse_debruijn_vars) - reverse_debruijn_idx
    # Free variable.
    return _hash_str(v.name)


@_alpha_hash_helper.register
def _alpha_hash_let(l: Let, reverse_debruijn_vars: PMap[str, int]) -> int:
    return hash(
        (
            _class_hashes[Let],
            _alpha_hash_helper(l.rhs, reverse_debruijn_vars),
            _alpha_hash_helper(l.body, _add_var(reverse_debruijn_vars, l.vars.name)),
        )
    )


@_alpha_hash_helper.register
def _alpha_hash_lam(l: Lam, reverse_debruijn_vars: PMap[str, int]) -> int:
    return hash((_class_hashes[Lam], _alpha_hash_helper(l.body, _add_var(reverse_debruijn_vars, l.arg.name))))


def alpha_hash(e: Expr):
    return _alpha_hash_helper(e, pmap())
