# fmt: off
import pytest

from rlo.expression import Expression, EF
from ksc.type import Type
from rlo import utils

def check_equal_but_not_id(x, y):
    assert x == y and id(x) != id(y)

def test_expression_equality_ignores_id():
    a = Expression.Variable("a")
    x = a + a
    assert id(a) == id(x.left)
    assert id(x.left) == id(x.right)

    a2 = Expression.Variable("a")
    assert a == a2
    assert id(a) != id(a2)
    y = a + a2
    assert id(y.left) == id (x.left)
    assert id(y.left) != id(y.right)
    assert x == y

def test_repr_eval():
    roundtrip_reprs = [
        'Expression.Variable("x")',
        'Expression.Variable("x", Type.Tensor(1, Type.Float))',
        'Expression.Constant(1.0)',
        'EF.Add("x", 1.0)',
        'EF.Add(1.0, Expression.Variable("x", Type.Float))',
        'EF.Add(1.0, "x")',
        'EF.Sub("x", "y")',
        'EF.Mul(1.0, "y")',
        'EF.Mul(2.0, 1.0)',
        'EF.Div(Expression.Variable("x", Type.Float), 2.0)',
        'EF.Div("x", Expression.Variable("y", Type.Float))',
        'EF.Index("x", "y")',
        'EF.Let("x", EF.Add("y", 1), "x")',
        'EF.Div(EF.Add(1.0, "x"), "y")',
        'EF.Div("y", EF.Add(1.0, "x"))',
        'EF.Mul("x", EF.Add("y", "z"))',
        'EF.Let("a", EF.Div(1.0, "y"), EF.Let("b", EF.Div("y", "x"), EF.Mul("a", "b")))'
    ]
    for repr in roundtrip_reprs:
        print(repr)
        exp = eval(repr)
        assert isinstance(exp, Expression)
        assert exp.__repr__() == repr

    exprs = [
        1.0 / Expression.Variable("x"),
        "x" / Expression.Constant(2.0),
        "x" / Expression.Variable("y"),
        2.0 / Expression.Constant(3.0),
        EF.DeltaVec("n", "x", 1.0 + Expression.Variable("y")),
        EF.Sumbuild("n", "i", EF.DeltaVec("n" * Expression.Constant(2), "i" * Expression.Constant(2.0), 10)),
        EF.Build("n", "i", EF.If(EF.Apply("is_even", "i"), 10, 0)),
        EF.Tuple(5.0, 1.0)
    ]
    for exp in exprs:
        repr = exp.__repr__()
        exp2 = eval(repr)
        assert exp == exp2
        repr2 = exp2.__repr__()
        assert repr2 == repr

def test_repr_types():
    xi = Expression.Variable("x", Type.Integer)
    xf = Expression.Variable("x", Type.Float)
    assert xi == xf
    # after the introduction of shape propagation, type is suppressed in Expression.__repr__()
    # compare Type.__repr__() instead
    xi=xi.type.__repr__()
    xf=xf.type.__repr__()
    assert xi != xf
    assert xi.find("Integer") != -1 and xi.find("Float") == -1
    assert xf.find("Integer") == -1 and xf.find("Float") != -1

def test_has_same_graph():
    x = Expression.Variable("x")
    y = Expression.Variable("y")
    assert (x*x) != (y*y)
    assert (x*x).has_same_graph(y*y)

def test_hash():
    a = Expression.Variable("a")
    x = Expression.Variable("x", Type.Integer)
    y = Expression.Variable("y", Type.Float)
    assert hash(x) == hash(Expression.Variable("x")) # Types ignored
    assert hash(y) == hash(Expression.Variable("y")) # Types ignored
    assert hash(EF.Let(x, a, x)) == hash(EF.Let(y, a, y))
    assert len(set([EF.Let(x, a,  x), EF.Let(y, a,  y)])) == 1

    x = Expression.Variable("x", Type.Float)
    assert hash(EF.Let(x, a, EF.Let(y, a,  x+y))) == hash(EF.Let(y, a, EF.Let(x, a,  y+x)))
    assert hash(EF.Let(x, a, EF.Let(y, a,  x+y))) != hash(EF.Let(y, a, EF.Let(x, a,  x+y)))

    assert hash(x + y) != hash(y + x)
    # These test de Bruijn numbering a little more, in that the references to the outer bound variable
    # will have different de Bruijn numbers at each use.
    assert hash(EF.Let(x, a, x + EF.Let(y, a,  x + y))) == hash(EF.Let(y, a, y + EF.Let(x, a,  y + x)))
    assert hash(EF.Let(x, a, x + EF.Let(y, a,  x + y))) != hash(EF.Let(y, a, y + EF.Let(x, a,  x + y)))

    assert hash(EF.Lam(x, x)) == hash(EF.Lam(y, y))

def test_hash_with_rebinding():
    left = EF.Let("a", EF.Div(1.0, "x"), EF.Let("b", EF.Div(1.0, "x"), EF.Div("a", EF.Let("c", EF.Add(1.0, "b"), "c"))))
    # Rename 'c'' to overwrite outer a. Should make no difference.
    right = EF.Let("a", EF.Div(1.0, "x"), EF.Let("b", EF.Div(1.0, "x"), EF.Div("a", EF.Let("a", EF.Add(1.0, "b"), "a"))))

    assert left==right
    assert hash(left) == hash(right)

def test_hash_let_rebinding_self():
    a = EF.Let("y", 6, EF.Let("x", EF.Add("y", 5), EF.Add("x", 1)))
    b = EF.Let("x", 6, EF.Let("x", EF.Add("x", 5), EF.Add("x", 1)))
    assert a == b
    assert hash(a) == hash(b)

def test_equals():
    x = Expression.Variable("x")
    y = Expression.Variable("y")
    z = Expression.Variable("z")
    assert x == Expression.Variable("x", Type.Tensor(1,Type.Float)) # Or any type
    exp1 = EF.Let(x, z + 1.0,  x * 2.0)
    exp2 = EF.Let(y, z + 1.0,  y * 2.0)
    assert exp1.third != exp2.third
    assert hash(exp1.third) != hash(exp2.third)
    assert exp1 == exp2
    assert hash(exp1) == hash(exp2)
    # Test that the (x is x) shortcut does not lead to bad equivalences (see get_var_mapping)
    assert not EF.Lam(Expression.Variable("x", Type.Float), x+x) == EF.Lam(Expression.Variable("y", Type.Float), x+y)

    p = Expression.Variable("p", Type.Bool)
    e = EF.Lam(p, EF.Let(x, "p", EF.Apply(y, p))) # x is let-bound, y is free; p is bound
    p2 = Expression.Variable("p2", Type.Bool)
    e2 = EF.Lam(p2, EF.Let(x, "p", EF.Apply(y, p2))) # x is let-bound, y is free; p is free
    # These two are different because in one case p binds p but in the other case p2 does not bind p
    assert not e == e2
    # But this was being declared true (#658):
    assert not e2 == e

def test_ctor_perf():
    DEPTH=2000 # With uncached self.vars, takes about 80 secs
    with utils.override_recursion_limit(DEPTH*4):
        from rlo.rewrites import ParsedRule
        a = Expression.Variable("a")
        b = Expression.Variable("b") * 2
        e = b
        for _ in range(DEPTH):
            e = a + e

        print("READY")
        # A rule that applies only at the bottom
        rule = ParsedRule("(rule mul_by_2 (x : Any) (mul x 2) (add x x))")
        rewrite = utils.single_elem(list(rule.get_all_rewrites_expr(e)))
        print("CALLING APPLY")
        rewrite.apply_expr(e)
        print("DONE")

def test_first_order():
    x = Expression.Variable("x", Type.Float)
    y = Expression.Variable("y", Type.Float)
    Expression.Variable("y", Type.Float)
    func = EF.Lam(x, x)
    # Even with free variables, not yet enforced
    EF.Lam(y, x)
    # But, no returning functions
    with pytest.raises(AssertionError):
        EF.Lam(y, func)

    # No inline lambdas
    with pytest.raises(AssertionError):
        EF.Apply(func, Expression.Constant(5))
    EF.Let("f", func, EF.Apply("f", Expression.Constant(5)))

def test_new_var():
    assert Expression.new_var(Expression.Constant(0)).name == "var0"
    assert Expression.new_var(Expression.Variable("varabc")).name == "var0"
    assert Expression.new_var(Expression.Variable("var0")).name == "var1"
    assert Expression.new_var(Expression.Variable("var0"), Expression.Variable("var1"), Expression.Variable("var2")).name == "var3"

def test_free_vars():
    assert Expression.Variable("x").free_var_names == {"x"}
    assert EF.Add("x", "y").free_var_names == {"x", "y"}
    assert EF.Let("x", 3, "x").free_var_names == frozenset()
    assert EF.Let("x", "x", 4).free_var_names == {"x"}
    assert EF.Let("x", "x", "x").free_var_names == {"x"}
    assert EF.Let("x", 3, EF.Add("x", "y")).free_var_names == {"y"}
    assert EF.Add("x", EF.Let("x", 3, EF.Add("x", "y"))).free_var_names == {"x", "y"}

    a = Expression.Variable("a", type=Type.Integer)
    assert EF.Lam(a, "y").free_var_names == {"y"}
    assert EF.Lam(a, a).free_var_names == frozenset()
    assert EF.Lam(a, EF.Let("y", EF.Add("a", "y"), 4)).free_var_names == {"y"} # The outer one (rebound by let)

    for ef in [EF.Build, EF.Sumbuild]:
        assert ef("n", "x", "x").free_var_names == {"n"}
        assert ef("x", "x", "i").free_var_names == {"x", "i"}
        assert ef(10, "x", "x").free_var_names == frozenset()

def test_get_path():
    e = EF.Add(EF.Mul("x", "y"), EF.Div("a", "b"))
    assert list(e.get_path(3)) == [
        (0, e, 0),
        (1, e.left, 1),
        (3, Expression.Variable("y"), None)
    ]

    assert list(e.get_path(4)) == [
        (0, e, 1),
        (4, e.right, None)
    ]

    assert list(e.get_path(5)) == [
        (0, e, 1),
        (4, e.right, 0),
        (5, Expression.Variable("a"), None)
    ]
