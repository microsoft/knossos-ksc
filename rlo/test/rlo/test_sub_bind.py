# fmt: off
import re

from rlo.expression import Expression, RewriteTarget, EF
from rlo import rewrites

def test_sub_bind():
    a = Expression.Variable("a")
    x = Expression.Variable("x")
    exp = EF.Let(a, 1.0 / x, a / (1.0 + a))
    cands = [rewrite.apply_expr(exp) for rewrite in rewrites.rule("inline_let").get_all_rewrites_expr(exp)]
    assert (len(cands) == 2) and set([str(c) for c in cands]) == set([
        "(let (a (div 1.0 x)) (div a (add 1.0 (div 1.0 x))))",
        "(let (a (div 1.0 x)) (div (div 1.0 x) (add 1.0 a)))"
    ])
    for c in cands:
        res = [rewrite.apply_expr(c) for rewrite in rewrites.rule("inline_let").get_all_rewrites_expr(c)]
        assert len(res) == 1
        exp = res[0]
        assert str(exp) == "(let (a (div 1.0 x)) (div (div 1.0 x) (add 1.0 (div 1.0 x))))"
    exp = RewriteTarget(exp).apply_rule(rewrites.rule("delete_let"))
    assert str(exp) == "(div (div 1.0 x) (add 1.0 (div 1.0 x)))"

def test_inline_delete_let():
    x = Expression.Variable("x")
    a = Expression.Variable("a")
    exp = EF.Let(a, x + 1.0, a * 2.0)
    e2 = RewriteTarget(exp).third.left.apply_rule(rewrites.rule("inline_let"))
    assert e2 == EF.Let(a, x + 1.0, (x + 1.0) * 2.0)
    assert RewriteTarget(e2).apply_rule(rewrites.rule("delete_let")) == (x + 1.0) * 2.0

def test_duplicate_bind():
    x = Expression.Variable("x")
    exp = EF.Let("a", EF.Let("b", x + 1.0, "b"), EF.Add("a", "a"))
    # Sanity check before applying rule
    assert set([n.name for n in exp.nodes if n.op == "variable"]) == set(["a", "x", "b"])
    assert str(exp).count("let") == 2

    while True:
        cands = [rewrite.apply_expr(exp) for rewrite in rewrites.rule("inline_let").get_all_rewrites_expr(exp)]
        if len(cands) == 0:
            break
        exp = cands[0]
    exp = RewriteTarget(exp).apply_rule(rewrites.rule("delete_let"))

    # We should have EF.Let(b, x + 1.0, x + 1.0) + EF.Let(b', x + 1.0, x + 1.0)
    # for some variables b,b' that may be the same
    assert exp.op == "add"
    assert exp.left.op == "let"
    assert exp.left == EF.Let(exp.left.first, x + 1.0, x + 1.0)
    assert exp.right.op == "let"
    assert exp.right == EF.Let(exp.right.first, x + 1.0, x + 1.0)
    assert exp.left == exp.right

    assert len(set([exp.left.first.name, exp.right.first.name, 'x'])) in [2,3]

def test_sub_bind_alpha_converts_inside_bind():
    var0 = Expression.Variable("var0")
    var1 = Expression.Variable("var1")
    x = Expression.Variable("x")

    e = EF.Let(var1, Expression.Constant(1.0), var1 / EF.Let(var0, 1.0/(1.0/x), var0))

    for rewrite in rewrites.get_rules("binding_rules").get_all_rewrites_expr(e):
        s = str(rewrite.apply_expr(e))
        binds = [str(m) for m in re.findall("let var[0-9]", s)]
        # Same variable should not be bound more than once
        assert len(binds) == len(set(binds))
