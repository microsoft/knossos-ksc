from ksc.rewrites import rule, RuleSet
from ksc.parse_ks import parse_expr

def apply_in_only_location(rule_name, expr):
    cands = list(rule(rule_name).get_all_rewrites(expr))
    assert len(cands) == 1
    return cands[0]()

def check_nowhere_applicable(rule_name, expr):
    assert len(list(rule(rule_name).get_all_rewrites(expr))) == 0

def test_inline_var_single():
    e = parse_expr("(let (a (div 1.0 x)) (div a (add a 1.0)))")
    # Should be exactly two candidates
    rw_div, rw_add = sorted(
        rule("inline_var").get_all_rewrites(e),
        key=lambda rw: tuple(rw.path) # TODO drop tuple following merge
    )
    assert (rw_div.rule, rw_div.path) == ("inline_var", [1,0])
    assert rw_div() == parse_expr("(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))")
    assert (rw_add.rule, rw_add.path) == ("inline_var", [1,1,0])
    assert rw_add() == parse_expr("(let (a (div 1.0 x)) (div a (add (div 1.0 x) 1.0)))")

    assert apply_in_only_location("inline_var", rw_div()
     ) ==  apply_in_only_location("inline_var", rw_add()
     ) ==  parse_expr("(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x) 1.0)))")

def test_inline_var_shadowing():
    e = parse_expr("(add a (let (a 2) a)")
    e2 = apply_in_only_location("inline_var", e)
    assert e2 == parse_expr("(add a (let a 2) 2)")
    check_nowhere_applicable("inline_var", e2)

def test_delete_let_single():
    check_nowhere_applicable("delete_let", parse_expr("(let (a (div 1.0 x)) (div a (add a 1.0)))"))
    check_nowhere_applicable("delete_let", parse_expr("(let (a (div 1.0 x)) (div (div 1.0 x) (add a 1.0)))"))
    check_nowhere_applicable("delete_let", parse_expr("(let (a (div 1.0 x)) (div a (add (div 1.0 x)) 1.0)))"))
    assert apply_in_only_location("delete_let", parse_expr("(let (a (div 1.0 x)) (div (div 1.0 x) (add (div 1.0 x)) 1.0)))")
      ) == parse_expr("(div (div 1.0 x) (add (div 1.0 x)) 1.0)")