from ksc.rewrites import rule, RuleSet, inline_var, delete_let
from ksc.parse_ks import parse_expr_string

def apply_in_only_location(rule_name, expr):
    cands = list(rule(rule_name).find_all_matches(expr))
    assert len(cands) == 1
    return cands[0].rewrite()

def check_nowhere_applicable(rule_name, expr):
    assert len(list(rule(rule_name).find_all_matches(expr))) == 0

def test_inline_var_single():
    e = parse_expr_string("(let (a (div 1.0 x)) (div a (add a 1.0)))")
    # Should be exactly two candidates
    rw_div, rw_add = sorted(rule("inline_var").find_all_matches(e), key=lambda rw: tuple(rw.path))
    assert (rw_div.rule, rw_div.path) == (inline_var, (1, 0))
    assert rw_div.rewrite() == parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))")
    assert (rw_add.rule, rw_add.path) == (inline_var, (1, 1, 0))
    assert rw_add.rewrite() == parse_expr_string("(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))")

    assert (apply_in_only_location("inline_var", rw_div.rewrite())
            == apply_in_only_location("inline_var", rw_add.rewrite())
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
    assert rw_div.rewrite() == parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))")
    assert (rw_add.rule, rw_add.path) == (inline_var, (1, 1, 0))
    assert rw_add.rewrite() == parse_expr_string("(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))")

    all_inlined = parse_expr_string("(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x) 1.0)))")
    assert ([rw.rewrite() for rw in r.find_all_matches(rw_div.rewrite())]
             == [rw.rewrite() for rw in r.find_all_matches(rw_add.rewrite())]
            == [all_inlined])

    # Now should be only one possible rewrite
    rw_del, = list(r.find_all_matches(all_inlined))
    assert (rw_del.rule, rw_del.path) == (delete_let, tuple())
    assert rw_del.rewrite() == parse_expr_string("(div (div 1.0 x) (add (div 1.0 x) 1.0))")

def sorted_rewrites(rule, expr):
    return [rw.rewrite() for rw in sorted(
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
