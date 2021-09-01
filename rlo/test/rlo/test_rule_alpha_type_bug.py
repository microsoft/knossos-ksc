from rlo.expression import Expression
from rlo.expression_util import deep_copy_with_types
from rlo import rewrites
from rlo import sparser
from ksc.type import Type


def test_rule_breaks_alpha_conversion():
    var0 = Expression.Variable(
        "var0",
        Type.Tuple(Type.Tensor(1, Type.Float), Type.Tensor(1, Type.Float), Type.Float),
    )
    e = sparser.parse_expr(
        "(sumbuild (size (get$1$3 var0)) (lam (i : Integer) (add (sub (mul (sub (get$3$3 var0) (index i (get$2$3 var0))) (get$3$3 var0)) (mul (index i (get$2$3 var0)) (get$3$3 var0))) (mul (index i (get$2$3 var0)) (index i (get$2$3 var0))))))",
    )
    assert e.first.only_child.left == var0
    assert e.first.only_child.left.type is None

    def with_type(expr):
        return deep_copy_with_types(expr, {"var0": var0.type})

    e = with_type(e)
    assert e.first.only_child.left.name == "var0"
    assert e.first.only_child.left.type is var0.type
    assert e.type == Type.Float

    for rewrite in rewrites.get_rules("ml_rules_no_bind").get_all_rewrites_expr(e):
        child = rewrite.apply_expr(e)
        assert e.type == with_type(child).type
