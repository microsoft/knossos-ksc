from ksc.rewrites import RuleSet
from ksc.rewrites_prelude import constant_folding_rules
from ksc.parse_ks import parse_expr_string, parse_ks_filename
from ksc.type_propagate import type_propagate_decls


def sorted_rewrites(rule, expr):
    return [rw.apply_rewrite() for rw in sorted(rule.find_all_matches(expr), key=lambda rw: rw.path)]


def test_constant_folding():
    rules = RuleSet(constant_folding_rules)
    e = parse_expr_string("(add (div (mul 3 5) 4) (add 1 6))")
    # No matches without resolving StructuredNames by type propagation
    assert len(list(rules.find_all_matches(e))) == 0
    symtab = dict()
    decls_prelude = list(parse_ks_filename("src/runtime/prelude.ks"))
    type_propagate_decls(decls_prelude, symtab)
    folded_mul = parse_expr_string("(add (div 15 4) (add 1 6))")
    folded_add = parse_expr_string("(add (div (mul 3 5) 4) 7)")
    type_propagate_decls([e, folded_mul, folded_add], symtab)
    actual = sorted_rewrites(rules, e)
    assert actual == [folded_mul, folded_add]

    actual2 = sorted_rewrites(rules, folded_mul)
    folded_mul_then_div = parse_expr_string("(add 3 (add 1 6))")
    folded_mul_then_add = parse_expr_string("(add (div 15 4) 7)")
    type_propagate_decls([folded_mul_then_div, folded_mul_then_add], symtab)
    assert actual2 == [folded_mul_then_div, folded_mul_then_add]

    # Both of those rewrite (only) to the same:
    folded_all_but_add = parse_expr_string("(add 3 7)")
    type_propagate_decls([folded_all_but_add], symtab)
    assert (
        sorted_rewrites(rules, folded_mul_then_div)
        == sorted_rewrites(rules, folded_mul_then_add)
        == [folded_all_but_add]
    )

    assert sorted_rewrites(rules, folded_all_but_add) == [parse_expr_string("10")]
