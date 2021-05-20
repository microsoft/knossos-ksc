from ksc.alpha_equiv import are_alpha_equivalent, alpha_hash
from ksc.expr import Const, Let, Var
from ksc.parse_ks import parse_expr_string
from ksc.type_propagate import type_propagate_decls, type_propagate
from ksc.type import Type


def test_alpha_equivalence():
    exp1 = parse_expr_string("(let (x (add z 1.0)) (mul x 2.0))")
    exp2 = parse_expr_string("(let (x (add z 1.0)) (mul x 2.1))")
    assert not are_alpha_equivalent(exp1, exp2)
    assert alpha_hash(exp1) != alpha_hash(exp2)
    exp3 = parse_expr_string("(let (y (add z 1.0)) (mul y 2.0))")
    assert are_alpha_equivalent(exp1, exp3)
    assert alpha_hash(exp1) == alpha_hash(exp3)
    exp4 = parse_expr_string("(let (x (add z 1.0)) (mul y 2.0))")
    assert not are_alpha_equivalent(exp1, exp4)
    assert alpha_hash(exp1) != alpha_hash(exp4)


def test_alpha_equivalence_aliasing():
    body = parse_expr_string("(div x y)")
    # body refers to x,y; bind these differently, so Expr's not equivalent.
    exp1 = Let(Var("x"), Const(1), Let(Var("y"), Const(2), body))
    exp2 = Let(Var("y"), Const(1), Let(Var("x"), Const(2), body))
    assert not are_alpha_equivalent(exp1, exp2)
    assert alpha_hash(exp1) != alpha_hash(exp2)

    # But other variables can be alpha-renamed and Expr's still be equal.
    exp3 = Let(Var("a"), Const(1), Let(Var("b"), Const(2), body))
    exp4 = Let(Var("b"), Const(1), Let(Var("a"), Const(2), body))
    assert are_alpha_equivalent(exp3, exp4)
    assert alpha_hash(exp3) == alpha_hash(exp4)


def test_alpha_equivalence_diff_types():
    x = Var("x")
    x_again = Var("x")
    assert x == x_again
    assert are_alpha_equivalent(x, x_again)
    assert alpha_hash(x) == alpha_hash(x_again)
    x_again.type_ = Type.Integer
    assert x == x_again  # Allows different type
    assert are_alpha_equivalent(x, x_again)
    assert alpha_hash(x) == alpha_hash(x_again)


def test_alpha_equivalence_type_propagation(prelude_symtab):
    exp1 = parse_expr_string("(let (x (add z 1.0)) (mul x 2.0))")
    exp2 = parse_expr_string("(let (x (add z 1.0)) (mul x 2.0))")

    symtab = {**prelude_symtab, "z": Type.Float}
    type_propagate(exp1, symtab)
    assert exp1 != exp2  # One has resolved calls to StructuredNames
    assert not are_alpha_equivalent(exp1, exp2)
    assert alpha_hash(exp1) != alpha_hash(exp2)
    type_propagate(exp2, symtab)
    assert exp1 == exp2
    assert are_alpha_equivalent(exp1, exp2)
    assert alpha_hash(exp1) == alpha_hash(exp2)


def test_alpha_equivalence_shadows_free():
    e = parse_expr_string("(lam (p : Bool) (assert p p))")
    e_renamed = parse_expr_string("(lam (q : Bool) (assert q q))")
    e_diff = parse_expr_string("(lam (q : Bool) (assert p q))")
    type_propagate_decls([e, e_renamed, e_diff], {"p": Type.Bool})
    assert are_alpha_equivalent(e, e_renamed)
    assert alpha_hash(e) == alpha_hash(e_renamed)
    # These two are different because in one case q binds p but in the other case p2 does not bind p
    assert not are_alpha_equivalent(e, e_diff)
    assert alpha_hash(e) != alpha_hash(e_diff)
    # This demonstrates the need to check the right bound var is not used other than in correspondence to the left bound var
    assert not are_alpha_equivalent(e_diff, e)


def test_alpha_equiv_two_lets():
    # The same name is bound twice on the LHS, to two different names on the RHS.
    # This demonstrates why the substitution must be removed when exiting a binder.
    e = parse_expr_string("(assert (let (x true) x) (let (x 3) x))")
    e2 = parse_expr_string("(assert (let (p true) p) (let (z 3) z))")
    type_propagate_decls([e, e2], {})
    assert are_alpha_equivalent(e, e2)
    assert are_alpha_equivalent(e2, e)
    assert alpha_hash(e) == alpha_hash(e2)


def test_alpha_equiv_all_node_types():
    e1 = parse_expr_string(
        "(lam (a : Float) (let (b (if (gt a 0.0) a 0.0)) (assert (gte b 0.0) b)))"
    )
    e2 = parse_expr_string(
        "(lam (c : Float) (let (d (if (gt c 0.0) c 0.0)) (assert (gte d 0.0) d)))"
    )
    assert are_alpha_equivalent(e1, e2)
    assert alpha_hash(e1) == alpha_hash(e2)

    e_diff = parse_expr_string(
        "(lam (a : Float) (let (b (if (gte a 0.0) a 0.0)) (assert (gte b 0.0) b)))"
    )
    assert not are_alpha_equivalent(e1, e_diff)
    assert alpha_hash(e1) != alpha_hash(e_diff)
