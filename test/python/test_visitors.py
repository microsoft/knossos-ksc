from typing import FrozenSet

from ksc.expr import Expr, Var, Let, Lam, Const
from ksc.visitors import ExprVisitor, ExprTransformer
from ksc.parse_ks import parse_expr_string

# This seems to cover all the cases.
expr = parse_expr_string("(let (a (if p (add b c) (neg b))) (assert (gt a 0) (build a (lam (i : Integer) i))))")


def test_visitor():
    class CountConsts(ExprVisitor):
        def __init__(self):
            super().__init__(False)
            self._count = 0

        def count(self, e: Expr):
            self.visit(e)
            return self._count

        def visit_const(self, c: Const):
            self._count += 1

    assert CountConsts().count(expr) == 1
    assert (
        CountConsts().count(parse_expr_string("(if (gt p 0) x (index a (build 10 (lam (i : Integer) (add i 2)))))"))
        == 3
    )


def test_expr_transformer():
    assert ExprTransformer().visit(expr) == expr

    # A simple, non-semantic-preserving, transformation - this does not rename binders.
    class VarRenamer(ExprTransformer):
        def visit_var(self, v: Var) -> Expr:
            assert not v.decl
            return Var(v.name + "2")

    # Thus, check ExprTransformer traverses to the leaves.
    assert VarRenamer().visit(expr) == parse_expr_string(
        "(let (a (if p2 (add b2 c2) (neg b2))) (assert (gt a2 0) (build a2 (lam (i : Integer) i2))))"
    )


def test_expr_transformer_arg_passing():
    class UnboundVarRenamer(ExprTransformer):
        def visit_var(self, v: Var, bound_vars: FrozenSet[str]) -> Expr:
            return v if v.name in bound_vars else Var(v.name + "_2")

        def visit_let(self, l: Let, bound_vars: FrozenSet[str]) -> Expr:
            bound_here = [l.vars.name] if isinstance(l.vars, Var) else [v.name for v in l.vars]
            return Let(l.vars, self.visit(l.rhs, bound_vars), self.visit(l.body, bound_vars.union(bound_here)))

        def visit_lam(self, l: Lam, bound_vars: FrozenSet[str]) -> Expr:
            return Lam(l.arg, self.visit(l.body, bound_vars.union([l.arg.name])))

    assert UnboundVarRenamer().visit(expr, frozenset()) == parse_expr_string(
        "(let (a (if p_2 (add b_2 c_2) (neg b_2))) (assert (gt a 0) (build a (lam (i : Integer) i))))"
    )
