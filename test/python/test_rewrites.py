import pytest

from ksc.alpha_equiv import are_alpha_equivalent
from ksc.rewrites import rule, RuleSet, inline_var, delete_let, parse_rule_str
from ksc.parse_ks import parse_expr_string, parse_ks_file, parse_ks_filename
from ksc.type import Type
from ksc.type_propagate import type_propagate_decls
from ksc import utils


def apply_in_only_location(rule_name, expr):
    cands = list(rule(rule_name).find_all_matches(expr))
    return utils.single_elem(cands).apply_rewrite()


def check_nowhere_applicable(rule_name, expr):
    assert len(list(rule(rule_name).find_all_matches(expr))) == 0


def test_inline_var_single():
    e = parse_expr_string("(let (a (div 1.0 x)) (div a (add a 1.0)))")
    # Should be exactly two candidates
    rw_div, rw_add = sorted(
        rule("inline_var").find_all_matches(e), key=lambda rw: tuple(rw.path)
    )
    assert (rw_div.rule, rw_div.path) == (inline_var, (1, 0))
    assert rw_div.apply_rewrite() == parse_expr_string(
        "(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))"
    )
    assert (rw_add.rule, rw_add.path) == (inline_var, (1, 1, 0))
    assert rw_add.apply_rewrite() == parse_expr_string(
        "(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))"
    )

    assert (
        apply_in_only_location("inline_var", rw_div.apply_rewrite())
        == apply_in_only_location("inline_var", rw_add.apply_rewrite())
        == parse_expr_string(
            "(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x) 1.0)))"
        )
    )


def test_delete_let_single():
    check_nowhere_applicable(
        "delete_let", parse_expr_string("(let (a (div 1.0 x)) (div a (add a 1.0)))")
    )
    check_nowhere_applicable(
        "delete_let",
        parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))"),
    )
    check_nowhere_applicable(
        "delete_let",
        parse_expr_string("(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))"),
    )
    assert apply_in_only_location(
        "delete_let",
        parse_expr_string(
            "(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x)) 1.0))"
        ),
    ) == parse_expr_string("(div (div 1.0 x) (add (div 1.0 x)) 1.0)")


def test_ruleset():
    r = RuleSet([rule("inline_var"), rule("delete_let")])
    e = parse_expr_string("(let (a (div 1.0 x)) (div a (add a 1.0)))")
    # Should be exactly two candidates
    rw_div, rw_add = sorted(r.find_all_matches(e), key=lambda rw: rw.path)
    assert (rw_div.rule, rw_div.path) == (inline_var, (1, 0))
    assert rw_div.apply_rewrite() == parse_expr_string(
        "(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))"
    )
    assert (rw_add.rule, rw_add.path) == (inline_var, (1, 1, 0))
    assert rw_add.apply_rewrite() == parse_expr_string(
        "(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))"
    )

    all_inlined = parse_expr_string(
        "(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x) 1.0)))"
    )
    assert (
        [rw.apply_rewrite() for rw in r.find_all_matches(rw_div.apply_rewrite())]
        == [rw.apply_rewrite() for rw in r.find_all_matches(rw_add.apply_rewrite())]
        == [all_inlined]
    )

    # Now should be only one possible rewrite
    (rw_del,) = list(r.find_all_matches(all_inlined))
    assert (rw_del.rule, rw_del.path) == (delete_let, tuple())
    assert rw_del.apply_rewrite() == parse_expr_string(
        "(div (div 1.0 x) (add (div 1.0 x) 1.0))"
    )


def sorted_rewrites(rule, expr):
    return [
        rw.apply_rewrite()
        for rw in sorted(rule.find_all_matches(expr), key=lambda rw: rw.path)
    ]


def test_inline_var_shadowing():
    e = parse_expr_string("(add a (let (a 2) a))")
    e2 = apply_in_only_location("inline_var", e)
    assert e2 == parse_expr_string("(add a (let (a 2) 2))")
    check_nowhere_applicable("inline_var", e2)


def test_inline_var_rebinding():
    e = parse_expr_string("(let (a 2) (add a (let (a 3) a)))")
    assert sorted_rewrites(rule("inline_var"), e) == [
        parse_expr_string("(let (a 2) (add 2 (let (a 3) a)))"),
        parse_expr_string("(let (a 2) (add a (let (a 3) 3)))"),
    ]


def test_inline_var_renames():
    e = parse_expr_string("(let (a (add x 3)) (let (x 2) (add a x)))")
    assert sorted_rewrites(rule("inline_var"), e) == [
        # Must rename x. The test depends on the new name being picked as x_0.
        parse_expr_string("(let (a (add x 3)) (let (x_0 2) (add (add x 3) x_0)))"),
        # Can also just inline the x, no renaming required
        parse_expr_string("(let (a (add x 3)) (let (x 2) (add a 2)))"),
    ]

    e = parse_expr_string("(let (x (add x 1)) (add x 2))")
    assert apply_in_only_location("inline_var", e) == parse_expr_string(
        "(let (x_0 (add x 1)) (add (add x 1) 2))"
    )


def test_simple_parsed_rule():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)

    r = parse_rule_str(
        '(rule "mul2_to_add$f" (x : Float) (mul x 2.0) (add x x))', symtab
    )
    input1, expected1 = (
        parse_expr_string("(if p (mul (add a b) 2.0) (mul a 3.0))"),
        parse_expr_string("(if p (add (add a b) (add a b)) (mul a 3.0))"),
    )

    type_propagate_decls(
        [input1, expected1],
        {**symtab, "p": Type.Bool, "a": Type.Float, "b": Type.Float},
    )
    actual1 = sorted_rewrites(r, input1)
    assert actual1 == [expected1]

    input2, expected2 = (
        parse_expr_string("(mul (to_float (mul x 2)) 2.0)"),
        parse_expr_string("(add (to_float (mul x 2)) (to_float (mul x 2)))"),
    )

    type_propagate_decls([input2, expected2], {**symtab, "x": Type.Integer})
    actual2 = sorted_rewrites(r, input2)
    assert actual2 == [expected2]


def test_parsed_rule_respects_types():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)

    # Check that we only match subtrees of specified type (without relying on the StructuredNames being different)
    r = parse_rule_str('(rule "rm.let$i" (e : Integer) (let (x e) x) e)', symtab)
    applicable_expr = parse_expr_string("(let (y 4) y)")
    inapplicable_expr = parse_expr_string("(let (z 5.0) z)")

    type_propagate_decls([applicable_expr, inapplicable_expr], symtab)
    assert sorted_rewrites(r, applicable_expr) == [parse_expr_string("4")]
    assert len(list(r.find_all_matches(inapplicable_expr))) == 0


def test_parsed_rule_allows_alpha_equivalence():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)

    # Use ts_add because [add (Tuple (Vec Float) (Vec Float))] is not in the prelude (yet)
    r = parse_rule_str(
        '(rule "add2_to_mul$vf" (v : Vec Float) (ts_add v v) (mul 2.0 v))', symtab
    )
    e = parse_expr_string(
        "(ts_add (build 10 (lam (i : Integer) (to_float i))) (build 10 (lam (j : Integer) (to_float j))))"
    )
    expected = parse_expr_string(
        "(mul 2.0 (build 10 (lam (k : Integer) (to_float k))))"
    )
    type_propagate_decls([e, expected], symtab)
    actual = sorted_rewrites(r, e)
    assert are_alpha_equivalent(utils.single_elem(actual), expected)


def test_parsed_rule_capture():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)

    # If the RHS introduces a new bound variable, then it needs to be renamed
    # into a fresh variable when the rule is applied, to avoid capture
    r = parse_rule_str(
        '(rule "foo1" (x : Integer) (mul x 3) (let (y (add x x)) (add y x)))', symtab
    )
    e = parse_expr_string("(let (y 2) (mul (add y 1) 3))")
    expected = parse_expr_string(
        "(let (y 2) (let (t__0 (add (add y 1) (add y 1))) (add t__0 (add y 1))))"
    )
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected

    # Does it still work if target is using t__0?
    e = parse_expr_string("(let (t__0 2) (mul (add t__0 1) 3))")
    expected = parse_expr_string(
        """
        (let (t__0 2)
            (let (t__1 (add (add t__0 1) (add t__0 1))) (add t__1 (add t__0 1))))
    """
    )
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected

    # Test for 'lam' e.g. inside build
    r = parse_rule_str(
        """
        (rule "buildfoo" (x : Integer)
            (mul x 3)
            (index 0 (build 10 (lam (i : Integer) (mul x 3))))
         )
    """,
        symtab,
    )
    e = parse_expr_string("(let (i 2) (mul (add i 1) 3))")
    expected = parse_expr_string(
        "(let (i 2) (index 0 (build 10 (lam (t__0 : Integer) (mul (add i 1) 3)))))"
    )
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected

    # Bound variables in the RHS should not be rewritten if they are matched
    # by the LHS:
    r = parse_rule_str(
        '(rule "foo2" ((y : Integer) (z : Integer)) (let (x (add y 0)) z) (let (x y) z))',
        symtab,
    )
    e = parse_expr_string("(let (v (add 33 0)) (mul v 3))")
    expected = parse_expr_string("(let (v 33) (mul v 3))")
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected


def test_multiple_occurrences_in_different_binders():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    r = parse_rule_str(
        '(rule "sub.self.let" ((e : Integer) (e2 : Any)) (sub e (let (x e2) e)) 0)',
        symtab,
    )

    e = parse_expr_string("(sub (mul x y) (let (z 17) (mul x y)))")
    expected = parse_expr_string("0")
    no_match = parse_expr_string("(sub (mul x y) (let (x 17) (mul x y)))")
    type_propagate_decls(
        [e, expected, no_match], {**symtab, "x": Type.Integer, "y": Type.Integer}
    )

    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected
    assert len(list(r.find_all_matches(no_match))) == 0


def test_rule_rhs_capture():
    with pytest.raises(AssertionError):
        r = parse_rule_str(
            '(rule "lift_bind_over_if" ((p : Bool) (body : Any) (f : Any) (e : Any)) (if p (let (x e) body) f) (let (x e) (if p body f)))',
            {},
        )
        print(r._binders_escaped)


def test_rule_pickling():
    import pickle

    r = pickle.loads(pickle.dumps(inline_var))
    assert r is inline_var
