from ksc.alpha_equiv import are_alpha_equivalent
from ksc.expr import Var
from ksc.parse_ks import parse_expr_string, parse_ks_filename
from ksc.type_propagate import type_propagate_decls, type_propagate
from ksc.type import Type

def test_alpha_equivalence_diff_types():
    x = Var("x")
    x_again = Var("x")
    assert x == x_again
    assert are_alpha_equivalent(x, x_again)
    x_again.type_ = Type.Integer
    assert x == x_again # Allows different type
    assert not are_alpha_equivalent(x, x_again) # Requires different

def test_alpha_equivalence():
    exp1 = parse_expr_string("(let (x (add z 1.0)) (mul x 2.0))")
    exp2 = parse_expr_string("(let (x (add z 1.0)) (mul x 2.1))")
    assert not are_alpha_equivalent(exp1, exp2)
    exp3 = parse_expr_string("(let (y (add z 1.0)) (mul y 2.0))")
    assert are_alpha_equivalent(exp1, exp3)
    exp4 = parse_expr_string("(let (x (add z 1.0)) (mul y 2.0))")
    assert not are_alpha_equivalent(exp1, exp4)

def test_alpha_equivalence_type_propagation():
    exp1 = parse_expr_string("(let (x (add z 1.0)) (mul x 2.0))")
    exp2 = parse_expr_string("(let (x (add z 1.0)) (mul x 2.0))")

    symtab = {"z": Type.Float}
    type_propagate_decls(list(parse_ks_filename("src/runtime/prelude.ks")), symtab)
    type_propagate(exp1, symtab)
    assert not are_alpha_equivalent(exp1, exp2)
    type_propagate(exp2, symtab)
    assert are_alpha_equivalent(exp1, exp2)

def test_alpha_equivalence_shadows_free():
    e = parse_expr_string("(lam (p : Bool) (assert p p))")
    e_renamed = parse_expr_string("(lam (q : Bool) (assert q q))")
    e_diff = parse_expr_string("(lam (q : Bool) (assert p q))")
    type_propagate_decls([e, e_renamed, e_diff], {"p": Type.Bool})
    assert are_alpha_equivalent(e, e_renamed)
    # These two are different because in one case q binds p but in the other case p2 does not bind p
    assert not are_alpha_equivalent(e, e_diff)
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
