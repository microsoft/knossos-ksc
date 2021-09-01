# fmt: off
from rlo.expression import Expression, RewriteTarget, EF
from rlo import rewrites
from rlo import utils

def test_cse_bind():
    x = Expression.Variable("x")
    exp = (1.0 / x) / (1.0 + (1.0 / x))
    # Introduce let around first child
    exp = RewriteTarget(exp).left.apply_rule(rewrites.rule("new_bind"))
    assert exp == EF.Let("var0", 1.0/x, "var0") / (1.0 + (1.0 / x))

    tgt = RewriteTarget(exp).left
    assert tgt.is_rule_applicable(rewrites.rule("lift_bind"))
    exp = tgt.apply_rule(rewrites.rule("lift_bind"))
    assert str(exp) == "(let (var0 (div 1.0 x)) (div var0 (add 1.0 (div 1.0 x))))"

    exp = RewriteTarget(exp).third.right.right.apply_rule(rewrites.rule("new_bind"))
    assert str(exp) == "(let ({0} (div 1.0 x)) (div {0} (add 1.0 (let ({1} (div 1.0 x)) {1}))))".format(
        exp.first.name, exp.third.right.right.first.name)

    exp = RewriteTarget(exp).third.right.right.apply_rule(rewrites.rule("lift_bind"))
    assert str(exp) == "(let ({0} (div 1.0 x)) (div {0} (let ({1} (div 1.0 x)) (add 1.0 {1}))))".format(
        exp.first.name, exp.third.right.first.name)
    exp = RewriteTarget(exp).third.right.apply_rule(rewrites.rule("lift_bind"))
    assert str(exp) == "(let ({0} (div 1.0 x)) (let ({1} (div 1.0 x)) (div {0} (add 1.0 {1}))))".format(
        exp.first.name, exp.third.first.name)

    exp = RewriteTarget(exp).apply_rule(rewrites.rule("cse_bind"))
    assert str(exp) == "(let (var0 (div 1.0 x)) (div var0 (add 1.0 var0)))"

def test_cse_bind_alpha():
    a = Expression.Variable("a")
    b = Expression.Variable("b")
    var0 = Expression.Variable("var0")
    var1 = Expression.Variable("var1")
    e1 = EF.Let(var0, 10, var0)
    e2 = EF.Let(var1, 10, var1)
    assert e1 == e2
    assert repr(e1) != repr(e2)

    e = EF.Let(a, e1, EF.Let(b, e2, a + b))
    assert RewriteTarget(e).apply_rule(rewrites.rule("cse_bind")) == EF.Let(a, e1, a + a)

def test_cse_bind_respects_rebinding():
    e = EF.Let("a", "outer", EF.Let("b", "outer", EF.Let("b", "inner", EF.Add("a", "b")))) # The add is of a==outer and b==inner
    rewritten = RewriteTarget(e).apply_rule(rewrites.rule("cse_bind"))
    assert rewritten == EF.Let("a", "outer", EF.Let("b", "inner", EF.Add("a", "b"))) # innermost b not renamed
    assert rewritten != EF.Let("a", "outer", EF.Let("b", "inner", EF.Add("a", "a"))) # if innermost b replaced by a

def test_cse_rebinds_free_var_in_value():
    # Here there are two a's - so while the bound exprs appear identical (a+1), they are not,
    # as inner let's bound expr sees a different "a" to the outer let's (textually-identical) bound expr
    e = EF.Let("a", EF.Add("a", 1), EF.Let("b", EF.Add("a", 1), EF.Mul("b", 2)))
    # So, we should not be able to CSE these.
    assert len(list(rewrites.rule("cse_bind").get_all_rewrites_expr(e))) == 0

def test_cse_bind_tuple():
    x = Expression.Variable("x")
    y = Expression.Variable("y")
    es = [x + 1]*10
    t = EF.Tuple(*es)
    for i in range(len(es)):
        all_newbinds = list(rewrites.rule("new_bind").get_all_rewrites_expr(t))
        assert len(all_newbinds) == len(es)-i
        t = all_newbinds[0].apply_expr(t)
    lets = [EF.Let(y, x + 1, y)] * 10
    assert t == EF.Tuple(*lets) # Lots of alpha-conversion here, all y's will be different
    while True:
        # We use two get_all_rewrites's to ensure we use the cse_bind's first
        all_rws = list(rewrites.rule("cse_bind").get_all_rewrites_expr(t)) + list(rewrites.rule("lift_bind").get_all_rewrites_expr(t))
        if len(all_rws) == 0: break
        # Take first and loop
        t = all_rws[0].apply_expr(t)
    assert t == EF.Let(y, x+1, EF.Tuple(*([y]*10)))

def test_cse_same_var():
    # This was breaking on an assertion that we don't substitute a variable for itself.
    # Now we avoid that substitution.
    a = Expression.Variable("a")
    e = EF.Let(a, 5, EF.Let(a, 5, EF.Add(a, 1)))
    rw = utils.single_elem([r.apply_expr(e) for r in rewrites.rule("cse_bind").get_all_rewrites_expr(e)])
    assert rw == EF.Let(a, 5, EF.Add(a, 1))

def test_let_lifting():
    x = Expression.Variable("x")
    y = Expression.Variable("y")
    a = Expression.Variable("a")
    b = Expression.Variable("b")

    e = EF.Let(a, x + 1.0, EF.Let(b, x * 2.0, a + b))
    assert RewriteTarget(e).third.apply_rule(rewrites.rule("lift_bind")) == EF.Let(b, x * 2.0, EF.Let(a, x + 1.0, a + b))
    assert len(list(rewrites.rule("lift_bind").get_all_rewrites_expr(EF.Let(a, x + 1.0, EF.Let(b, a * 2.0, b))))) == 0

    assert RewriteTarget(EF.Build(10, x, EF.Let(a, y + 1.0, x + y))).third.apply_rule(rewrites.rule("lift_bind")) == EF.Let(a, y + 1.0, EF.Build(10, x, x+y))

    e = EF.Build(10, x, EF.Let(a, x + 1.0, a * 2.0))
    print('expresison e=', e)
    bind = e.binds_any_in(e.third.second)
    print('bind= ', bind)
    assert len(list(rewrites.rule("lift_bind").get_all_rewrites_expr(EF.Build(10, x, EF.Let(a, x + 1.0, a * 2.0))))) == 0

def test_let_lifting_renames():
    exp = EF.Let("a", "b", EF.Let("b", "c", EF.Add("a", "b"))) # Final value is b+c in outermost context
    rewritten = utils.single_elem([rewrite.apply_expr(exp) for rewrite in rewrites.rule("lift_bind").get_all_rewrites_expr(exp)])
    # When binding for b is lifted, it must be renamed so that the binding for a still gets the outermost-context's b
    assert rewritten == EF.Let("b2", "c", EF.Let("a", "b", EF.Add("a", "b2")))
    # Forgetting would lead to...
    assert rewritten != EF.Let("b", "c", EF.Let("a", "b", EF.Add("a", "b"))) # Which would be c+c in outermost context

def test_let_reassociate():
    x = Expression.Variable("x")
    a = Expression.Variable("a")
    b = Expression.Variable("b")

    exp = EF.Let(a, EF.Let(b, x+1, b*2), a/3)
    rewritten = [rewrite.apply_expr(exp) for rewrite in rewrites.rule("lift_bind").get_all_rewrites_expr(exp)]
    assert len(rewritten) == 1
    assert rewritten[0] == EF.Let(b, x+1, EF.Let(a, b*2, a/3))
