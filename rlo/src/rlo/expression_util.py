from dataclasses import dataclass, field
from functools import cached_property
from typing import Dict, Generator, List, Optional, Mapping, NamedTuple

from pyrsistent import pmap

from rlo import costs
from rlo.expression import Expression, TypePropagationError
from ksc.type import Type


def get_node(expr: Expression, node_id: int, expected_op: Optional[str] = None):
    node = expr.nodes[node_id]
    if expected_op is not None:
        assert node.op == expected_op
    return node


def find_users(let_node: Expression) -> Generator[int, None, None]:
    """ Computes users of the let-bound variable.

    Args:
        let_node: a let node

    Returns:
        the list of node indices corresponding to distinct uses
        of the bound variable
    """
    assert let_node.op == "let"
    for node_id, node, env in let_node.node_ids_with_environment():
        if node.op == "variable":
            binder_idx, _val = env.get(node.name, (-1, None))
            if binder_idx == 0:
                yield node_id


def first_non_let_ancestor(expr: Expression, node_id: int):
    path = list(expr.get_path(node_id))
    return next(n for _idx, n, _which_c in reversed(path[:-1]) if n.op != "let")


def count_ops_along_path(expr: Expression, node_id: int, ops: List[str]):
    path = expr.get_path(node_id)
    return len([n for _idx, n, _which_c in path if n.op in ops])


@dataclass(frozen=True)
class SymtabAndDefs:
    symtab: Mapping[str, Type] = pmap()
    defs: Mapping[str, Expression] = pmap()
    """ A Lam-expression for each Def """

    def __post_init__(self):
        for name, lam in self.defs.items():
            assert lam.type is not None
            if name in self.symtab:
                assert self.symtab[name] == lam.type
            else:
                assert name.startswith("cost$")

    def make_toplevel(self, expr: Expression) -> "ExprWithEnv":
        expr = deep_copy_with_types(expr, self.symtab)
        return ExprWithEnv(expr, self)


@dataclass(frozen=True)
class ExprWithEnv:
    expr: Expression
    # compare=False means we don't check these in __eq__, - we expect it to be large but change infrequently.
    # Checking object identity via 'is' would be good except for pickle/unpickle. Perhaps something PMap ??
    env: SymtabAndDefs = field(compare=False)

    def __post_init__(self):
        assert self.expr.type is not None, "Toplevel Expressions must be typed"

    @cached_property
    def _cost(self):
        # This uses cached_property so that the cached value is stored, and discarded, with the ExprWithEnv
        # In contrast, functools.lru_cache would maintain a global cache outside any instance of ExprWithEnv.
        return costs.cost(self.expr, self.env.defs)

    def cost(self):
        """ Legacy interface that makes obvious the computation of cost() """
        return self._cost


class NamedExprWithEnv(NamedTuple):
    name: str
    exprenv: ExprWithEnv


def deep_copy_with_types(
    expr: Expression, bound_vars: Mapping[str, Type], respect_existing=False
) -> Expression:
    # First we (must) special-case all nodes binding or dealing with the environment (bound_vars)
    if expr.op == "constant":
        if expr.type is None:
            raise TypePropagationError
        return expr
    if expr.op == "variable":
        var_type = bound_vars.get(expr.name)
        if expr.type is not None:
            if (var_type is not None) and var_type != expr.type:
                raise TypePropagationError(
                    f"Variable {expr.name} has type {expr.type} yet environment says type should be {var_type}"
                )
            return expr
        if var_type is None:
            raise TypePropagationError(
                f"Found free variable {expr.name} during type propagation."
            )
        return Expression.Variable(expr.name, type=var_type)
    if respect_existing and expr.type is not None:
        return expr
    if expr.op == "let":
        bound_expr = deep_copy_with_types(
            expr.second, bound_vars, respect_existing=respect_existing
        )
        bound_var = Expression.Variable(expr.first.name, type=bound_expr.type)
        new_bound_vars = {**bound_vars, expr.first.name: bound_expr.type}
        body = deep_copy_with_types(
            expr.third, new_bound_vars, respect_existing=respect_existing
        )
        if body.type is None:
            raise TypePropagationError(
                f"Cannot determine the type of let body {body} in {expr} with {new_bound_vars}"
            )
        return expr.clone_with_new_children([bound_var, bound_expr, body])
    if expr.op == "lam":
        assert expr.left.op == "variable" and expr.left.type is not None
        new_bound_vars = {**bound_vars, expr.left.name: expr.left.type}
        body = deep_copy_with_types(
            expr.right, new_bound_vars, respect_existing=respect_existing
        )
        if body.type is None:
            raise TypePropagationError(
                f"Cannot determine the type of lambda body {body} in {expr} with {new_bound_vars}"
            )
        return expr.clone_with_new_children([expr.left, body])
    if expr.op in ["build", "sumbuild"]:
        assert expr.second.op == "variable"
        size_expr = deep_copy_with_types(
            expr.first, bound_vars, respect_existing=respect_existing
        )
        body = deep_copy_with_types(
            expr.third,
            {**bound_vars, expr.second.name: Type.Integer},
            respect_existing=respect_existing,
        )
        return expr.clone_with_new_children(
            [size_expr, Expression.Variable(expr.second.name, type=Type.Integer), body,]
        )
    return expr.clone_with_new_children(
        [
            deep_copy_with_types(c, bound_vars, respect_existing=respect_existing)
            for c in expr.children
        ]
    )
