from ksc.alpha_equiv import are_alpha_equivalent
from ksc.parse_ks import parse_expr_string
from ksc.type_propagate import type_propagate_decls
from ksc.type import Type

def test_alpha_equivalence():
    e = parse_expr_string("(lam (p : Bool) (assert p p))")
    e_renamed = parse_expr_string("(lam (q : Bool) (assert q q))")
    e_diff = parse_expr_string("(lam (q : Bool) (assert p q))")
    type_propagate_decls([e, e_renamed, e_diff], {"p": Type.Bool})
    assert are_alpha_equivalent(e, e_renamed)
    # These two are different because in one case q binds p but in the other case p2 does not bind p
    assert not are_alpha_equivalent(e, e_diff)
    # This demonstrates the need to check no mapping *to* the variable q escapes the binder.
    assert not are_alpha_equivalent(e_diff, e)

def test_alpha_equiv_two_lets():
    # The same name is bound twice on the LHS, to two different names on the RHS.
    # This demonstrates why the substitution must be removed when exiting a binder.
    e = parse_expr_string("(assert (let (x true) x) (let (x 3) x))")
    e2 = parse_expr_string("(assert (let (p true) p) (let (z 3) z))")
    type_propagate_decls([e, e2], {})
    assert are_alpha_equivalent(e, e2)
    assert are_alpha_equivalent(e2, e)

if __name__ == "__main__":
    test_alpha_equivalence()