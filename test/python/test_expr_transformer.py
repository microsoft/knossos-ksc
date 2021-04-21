from ksc.expr import Var, Expr
from ksc.expr_transformer import ExprTransformer
from ksc.parse_ks import parse_expr_string

def test_expr_transformer():
    # This seems to cover all the cases.
    expr = parse_expr_string("(let (a (if p (add b c) (neg b))) (assert (gt a 0) (build a (lam (i : Integer) i))))")

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
