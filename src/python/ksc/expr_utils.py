from functools import singledispatch
from typing import FrozenSet, Mapping, Sequence, TypeVar

from ksc.expr import Expr, Assert, Call, Const, If, Let, Lam, Var


@singledispatch
def subexps_no_binds(e: Expr) -> Sequence[Expr]:
    # The rewritable children of an Expr
    raise ValueError("Must be overridden for every Expr subclass")


@subexps_no_binds.register
def _subexps_no_binds_var(e: Var):
    return []


@subexps_no_binds.register
def _subexps_no_binds_const(e: Const):
    return []


@subexps_no_binds.register
def _subexps_no_binds_let(e: Let):
    return [e.rhs, e.body]


@subexps_no_binds.register
def _subexps_no_binds_lam(e: Lam):
    return [e.body]


@subexps_no_binds.register
def _subexps_no_binds_call(e: Call):
    return e.args


@subexps_no_binds.register
def _subexps_no_binds_if(e: If):
    return [e.cond, e.t_body, e.f_body]


@subexps_no_binds.register
def _subexps_no_binds_assert(e: Assert):
    return [e.cond, e.body]


@singledispatch
def list_binders(e: Expr) -> Sequence[str]:
    """ Make a list that for each Let/Lam binder, contains the bound name.
       (If multiple Let/Lam's bind the same name, the name will be repeated.) """
    return _list_subexp_binders(e)


def _list_subexp_binders(e: Expr) -> Sequence[str]:
    return sum([list_binders(ch) for ch in subexps_no_binds(e)], [])


@list_binders.register
def _list_binders_let(e: Let):
    assert isinstance(e.vars, Var), "Tupled Lets not supported - use untuple_lets first"
    return [e.vars.name] + _list_subexp_binders(e)


@list_binders.register
def _list_binders_lam(e: Lam):
    return [e.arg.name] + _list_subexp_binders(e)


def binder_sets_per_free_var(e: Expr) -> Mapping[str, Sequence[FrozenSet[str]]]:
    """ For each variable free in `e`, return a sequence containing,
        for each occurrence of that variable, the set of binders within e enclosing that occurrence.
    """
    return _binder_sets_helper(e, frozenset())


@singledispatch
def _binder_sets_helper(
    e: Expr, binders_in_scope: FrozenSet[str]
) -> Mapping[str, Sequence[FrozenSet[str]]]:
    return _concat_map_values(
        [_binder_sets_helper(ch, binders_in_scope) for ch in subexps_no_binds(e)]
    )


@_binder_sets_helper.register
def _binder_sets_var(
    v: Var, binders_in_scope: FrozenSet[str]
) -> Mapping[str, Sequence[FrozenSet[str]]]:
    return {} if v.name in binders_in_scope else {v.name: [binders_in_scope]}


T = TypeVar("T")


def _concat_map_values(
    maps: Sequence[Mapping[str, Sequence[T]]]
) -> Mapping[str, Sequence[T]]:
    res = {}
    for m in maps:
        for key, value_list in m.items():
            res.setdefault(key, []).extend(value_list)
    return res


@_binder_sets_helper.register
def _binder_sets_lam(e: Lam, binders_in_scope: FrozenSet[str]):
    return _binder_sets_helper(e.body, binders_in_scope.union([e.arg.name]))


@_binder_sets_helper.register
def _binder_sets_let(e: Let, binders_in_scope: FrozenSet[str]):
    assert isinstance(e.vars, Var), "Tupled Lets not supported - use untuple_lets first"
    return _concat_map_values(
        [
            _binder_sets_helper(e.rhs, binders_in_scope),
            _binder_sets_helper(e.body, binders_in_scope.union([e.vars.name])),
        ]
    )
