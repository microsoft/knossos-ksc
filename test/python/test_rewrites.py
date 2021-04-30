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
    rw_div, rw_add = sorted(rule("inline_var").find_all_matches(e), key=lambda rw: tuple(rw.path))
    assert (rw_div.rule, rw_div.path) == (inline_var, (1, 0))
    assert rw_div.apply_rewrite() == parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))")
    assert (rw_add.rule, rw_add.path) == (inline_var, (1, 1, 0))
    assert rw_add.apply_rewrite() == parse_expr_string("(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))")

    assert (apply_in_only_location("inline_var", rw_div.apply_rewrite())
            == apply_in_only_location("inline_var", rw_add.apply_rewrite())
            ==  parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x) 1.0)))"))

def test_delete_let_single():
    check_nowhere_applicable("delete_let", parse_expr_string("(let (a (div 1.0 x)) (div a (add a 1.0)))"))
    check_nowhere_applicable("delete_let", parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))"))
    check_nowhere_applicable("delete_let", parse_expr_string("(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))"))
    assert apply_in_only_location("delete_let",
        parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x)) 1.0))")
        ) == parse_expr_string("(div (div 1.0 x) (add (div 1.0 x)) 1.0)")

def test_ruleset():
    r = RuleSet([rule("inline_var"), rule("delete_let")])
    e = parse_expr_string("(let (a (div 1.0 x)) (div a (add a 1.0)))")
    # Should be exactly two candidates
    rw_div, rw_add = sorted(r.find_all_matches(e), key=lambda rw: rw.path)
    assert (rw_div.rule, rw_div.path) == (inline_var, (1, 0))
    assert rw_div.apply_rewrite() == parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))")
    assert (rw_add.rule, rw_add.path) == (inline_var, (1, 1, 0))
    assert rw_add.apply_rewrite() == parse_expr_string("(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))")

    all_inlined = parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x) 1.0)))")
    assert ([rw.apply_rewrite() for rw in r.find_all_matches(rw_div.apply_rewrite())]
             == [rw.apply_rewrite() for rw in r.find_all_matches(rw_add.apply_rewrite())]
            == [all_inlined])

    # Now should be only one possible rewrite
    rw_del, = list(r.find_all_matches(all_inlined))
    assert (rw_del.rule, rw_del.path) == (delete_let, tuple())
    assert rw_del.apply_rewrite() == parse_expr_string("(div (div 1.0 x) (add (div 1.0 x) 1.0))")

def sorted_rewrites(rule, expr):
    return [rw.apply_rewrite() for rw in sorted(
        rule.find_all_matches(expr), key=lambda rw: rw.path
    )]

def test_inline_var_shadowing():
    e = parse_expr_string("(add a (let (a 2) a))")
    e2 = apply_in_only_location("inline_var", e)
    assert e2 == parse_expr_string("(add a (let (a 2) 2))")
    check_nowhere_applicable("inline_var", e2)

def test_inline_var_rebinding():
    e = parse_expr_string("(let (a 2) (add a (let (a 3) a)))")
    assert sorted_rewrites(rule("inline_var"), e) == [
        parse_expr_string("(let (a 2) (add 2 (let (a 3) a)))"),
        parse_expr_string("(let (a 2) (add a (let (a 3) 3)))")
    ]

def test_inline_var_renames():
    e = parse_expr_string("(let (a (add x 3)) (let (x 2) (add a x)))")
    assert sorted_rewrites(rule("inline_var"), e) == [
        # Must rename x. The test depends on the new name being picked as x_0.
        parse_expr_string("(let (a (add x 3)) (let (x_0 2) (add (add x 3) x_0)))"),
        # Can also just inline the x, no renaming required
        parse_expr_string("(let (a (add x 3)) (let (x 2) (add a 2)))")
    ]

    e = parse_expr_string("(let (x (add x 1)) (add x 2))")
    assert apply_in_only_location("inline_var", e) == parse_expr_string(
        "(let (x_0 (add x 1)) (add (add x 1) 2))"
    )

def test_simple_parsed_rule():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)

    r = parse_rule_str('(rule "mul2_to_add" (x : Float) (mul x 2.0) (add x x))', symtab)
    foo, rewritten = (
        parse_expr_string("(if p (mul (add a b) 2.0) (mul a 3.0))"),
        parse_expr_string("(if p (add (add a b) (add a b)) (mul a 3.0))"))

    type_propagate_decls([foo, rewritten], {**symtab, "p": Type.Bool, "a": Type.Float, "b": Type.Float})
    all_rewrites = [m.apply_rewrite() for m in r.find_all_matches(foo)]
    assert all_rewrites == [rewritten]

def test_parsed_rule_capture():
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)

    # If the RHS introduces a new bound variable, then it needs to be renamed
    # into a fresh variable when the rule is applied, to avoid capture
    r = parse_rule_str('(rule "foo1" (x : Integer) (mul x 3) (let (y (add x x)) (add y x)))', symtab)
    e = parse_expr_string('(let (y 2) (mul (add y 1) 3))')
    expected = parse_expr_string("(let (y 2) (let (t__0 (add (add y 1) (add y 1))) (add t__0 (add y 1))))")
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected

    # Does it still work if target is using t__0?
    e = parse_expr_string('(let (t__0 2) (mul (add t__0 1) 3))')
    expected = parse_expr_string('''
        (let (t__0 2)
            (let (t__1 (add (add t__0 1) (add t__0 1))) (add t__1 (add t__0 1))))
    ''')
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected

    # Test for 'lam' e.g. inside build
    r = parse_rule_str('''
        (rule "buildfoo" (x : Integer)
            (mul x 3)
            (index 0 (build 10 (lam (i : Integer) (mul x 3))))
         )
    ''', symtab)
    e = parse_expr_string('(let (i 2) (mul (add i 1) 3))')
    expected = parse_expr_string(
        "(let (i 2) (index 0 (build 10 (lam (t__0 : Integer) (mul (add i 1) 3)))))")
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected

    # Bound variables in the RHS should not be rewritten if they are matched
    # by the LHS:
    r = parse_rule_str('(rule "foo2" ((y : Integer) (z : Integer)) (let (x (add y 0)) z) (let (x y) z))', symtab)
    e = parse_expr_string('(let (v (add 33 0)) (mul v 3))')
    expected = parse_expr_string("(let (v 33) (mul v 3))")
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected

def test_lift_if():
    e = parse_expr_string("(add (if p 4.0 2.0) 3.0)")
    symtab = dict()
    type_propagate_decls(list(parse_ks_filename("src/runtime/prelude.ks")), symtab)
    expected = parse_expr_string("(if p (add 4.0 3.0) (add 2.0 3.0))")
    type_propagate_decls([e, expected], {**symtab, "p": Type.Bool})
    actual = utils.single_elem(list(rule("lift_if").find_all_matches(e))).apply_rewrite()
    assert actual == expected

def test_lift_if_from_let():
    e = parse_expr_string("(let (x 4) (if (gt y 0) x 0))")
    match = utils.single_elem(list(rule("lift_if").find_all_matches(e)))
    actual = match.apply_rewrite()
    expected = parse_expr_string("(if (gt y 0) (let (x 4) x) (let (x 4) 0))")
    assert actual == expected
    # Now check we can't lift out of the let if the if-condition uses the bound variable
    e = parse_expr_string("(let (x 4) (if (gt x 0) x 0))")
    assert len(list(rule("lift_if").find_all_matches(e))) == 0

def test_lift_bind():
    e = parse_expr_string("(add (let (x 4.0) (add x 2.0)) 3.0)")
    symtab = dict()
    type_propagate_decls(list(parse_ks_filename("src/runtime/prelude.ks")), symtab)
    expected = parse_expr_string("(let (x 4.0) (add (add x 2.0) 3.0))")
    type_propagate_decls([e, expected], symtab)
    match = utils.single_elem(list(rule("lift_bind").find_all_matches(e)))
    actual = match.apply_rewrite()
    assert actual == expected

def test_interchange_lets():
    e = parse_expr_string("(let (x 4) (let (y 5) (add x y)))")
    e2 = parse_expr_string("(let (y 5) (let (x 4) (add x y)))")
    match = utils.single_elem(list(rule("lift_bind").find_all_matches(e)))
    actual = match.apply_rewrite()
    assert actual == e2
    match2 = utils.single_elem(list(rule("lift_bind").find_all_matches(actual)))
    actual2 = match2.apply_rewrite()
    assert actual2 == e
    # But, can't lift if the inner let uses the outer bound variable
    cant_lift = parse_expr_string("(let (x 5) (let (y (add x 1)) (add x y)))")
    assert len(list(rule("lift_bind").find_all_matches(cant_lift))) == 0
