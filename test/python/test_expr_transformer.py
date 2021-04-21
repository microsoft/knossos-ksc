from typing import FrozenSet

from ksc.expr import Expr, Var, Let, Lam
from ksc.expr_transformer import ExprTransformer
from ksc.parse_ks import parse_expr_string

# This seems to cover all the cases.
expr = parse_expr_string("(let (a (if p (add b c) (neg b))) (assert (gt a 0) (build a (lam (i : Integer) i))))")

def test_expr_transformer():
    assert ExprTransformer().transform(expr) == expr

    # A simple, non-semantic-preserving, transformation - this does not rename binders.
    class VarRenamer(ExprTransformer):

        def transform_var(self, v : Var) -> Expr:
            assert not v.decl
            return Var(v.name + "2")

    # Thus, check ExprTransformer traverses to the leaves.
    assert VarRenamer().transform(expr) == parse_expr_string(
        "(let (a (if p2 (add b2 c2) (neg b2))) (assert (gt a2 0) (build a2 (lam (i : Integer) i2))))"
    )

def test_expr_transformer_arg_passing():
    class UnboundVarRenamer(ExprTransformer):
        def transform_var(self, v : Var, bound_vars: FrozenSet[str]) -> Expr:
            return v if v.name in bound_vars else Var(v.name + "_2")

        def transform_let(self, l : Let, bound_vars: FrozenSet[str]) -> Expr:
            bound_here = [l.vars.name] if isinstance(l.vars, Var) else [v.name for v in l.vars]
            return Let(l.vars,
                self.transform(l.rhs, bound_vars),
                self.transform(l.body, bound_vars.union(bound_here)))

        def transform_lam(self, l : Lam, bound_vars: FrozenSet[str]) -> Expr:
            return Lam(l.arg, self.transform(l.body, bound_vars.union([l.arg.name])))

    assert UnboundVarRenamer().transform(expr, frozenset()) == parse_expr_string(
        "(let (a (if p_2 (add b_2 c_2) (neg b_2))) (assert (gt a 0) (build a (lam (i : Integer) i))))"
    )
