import pytest

from ksc.rewrites import RuleSet
from ksc.rewrites_ast import lift_let_rules, lift_if_rules
from ksc.parse_ks import parse_expr_string
from ksc.type import Type
from ksc.type_propagate import type_propagate_decls
from ksc import utils


def sorted_rewrites(rule, expr):
    return [
        rw.apply_rewrite()
        for rw in sorted(rule.find_all_matches(expr), key=lambda rw: rw.path)
    ]


lift_bind = RuleSet(lift_let_rules)
lift_if = RuleSet(lift_if_rules)


def test_lift_if(prelude_symtab):
    e = parse_expr_string("(add (if p 4.0 2.0) 3.0)")
    expected = parse_expr_string("(if p (add 4.0 3.0) (add 2.0 3.0))")
    type_propagate_decls([e, expected], {**prelude_symtab, "p": Type.Bool})
    actual = utils.single_elem(list(lift_if.find_all_matches(e))).apply_rewrite()
    assert actual == expected


def test_lift_if_from_let(prelude_symtab):
    e = parse_expr_string("(let (x 4) (if (gt y 0) x 0))")
    expected = parse_expr_string("(if (gt y 0) (let (x 4) x) (let (x 4) 0))")
    type_propagate_decls([e, expected], {**prelude_symtab, "y": Type.Integer})
    match = utils.single_elem(list(lift_if.find_all_matches(e)))
    actual = match.apply_rewrite()
    assert actual == expected
    # Now check we can't lift out of the let if the if-condition uses the bound variable
    e = parse_expr_string("(let (x 4) (if (gt x 0) x 0))")
    type_propagate_decls([e], prelude_symtab)
    assert len(list(lift_if.find_all_matches(e))) == 0


def test_lift_bind(prelude_symtab):
    e = parse_expr_string("(add (let (x 4.0) (add x 2.0)) 3.0)")
    expected = parse_expr_string("(let (x 4.0) (add (add x 2.0) 3.0))")
    type_propagate_decls([e, expected], prelude_symtab)
    match = utils.single_elem(list(lift_bind.find_all_matches(e)))
    actual = match.apply_rewrite()
    assert actual == expected


def test_interchange_lets(prelude_symtab):
    e = parse_expr_string("(let (x 4) (let (y 5) (add x y)))")
    e2 = parse_expr_string("(let (y 5) (let (x 4) (add x y)))")
    type_propagate_decls([e, e2], prelude_symtab)
    match = utils.single_elem(list(lift_bind.find_all_matches(e)))
    actual = match.apply_rewrite()
    assert actual == e2
    match2 = utils.single_elem(list(lift_bind.find_all_matches(actual)))
    actual2 = match2.apply_rewrite()
    assert actual2 == e
    # But, can't lift if the inner let uses the outer bound variable
    cant_lift = parse_expr_string("(let (x 5) (let (y (add x 1)) (add x y)))")
    type_propagate_decls([cant_lift], prelude_symtab)
    assert len(list(lift_bind.find_all_matches(cant_lift))) == 0


def test_lift_bind_shadowing(prelude_symtab):
    e = parse_expr_string("(add (let (x (add x 1)) x) x)")
    # The RHS of the let refers to another (free) x - just check:
    with pytest.raises(Exception):
        type_propagate_decls([e], prelude_symtab)
    # So, we must rename the bound x so as not to capture the free x
    expected = parse_expr_string("(let (x_0 (add x 1)) (add x_0 x))")
    type_propagate_decls([e, expected], {**prelude_symtab, "x": Type.Integer})
    match = utils.single_elem(list(lift_bind.find_all_matches(e)))
    actual = match.apply_rewrite()
    assert actual == expected

    # But, no need to rename if the uses of the free "x" are in the let RHS itself
    # (such uses will be lifted and stay outside the binder):
    e = parse_expr_string("(add (let (x (add x 1)) x) 2)")
    expected = parse_expr_string("(let (x (add x 1)) (add x 2))")
    renamed = parse_expr_string("(let (x_0 (add x 1)) (add x_0 2))")
    match = utils.single_elem(list(lift_bind.find_all_matches(e)))
    actual = match.apply_rewrite()
    assert actual == expected
    assert actual != renamed


def test_lifting_over_build(prelude_symtab):
    e = parse_expr_string(
        "(build 10 (lam (i : Integer) (let (x (add 5 7)) (if (gt x 5) x i))))"
    )
    expected = parse_expr_string(
        "(let (x (add 5 7)) (build 10 (lam (i : Integer) (if (gt x 5) x i))))"
    )
    type_propagate_decls([e, expected], prelude_symtab)
    rules = RuleSet(lift_let_rules + lift_if_rules)
    match = utils.single_elem(list(rules.find_all_matches(e)))
    actual = match.apply_rewrite()
    # Let should have been lifted:
    assert actual == expected

    # Now we can lift the if:
    match2 = utils.single_elem(list(rules.find_all_matches(actual)))
    actual2 = match2.apply_rewrite()
    expected2 = parse_expr_string(
        "(let (x (add 5 7)) (if (gt x 5) (build 10 (lam (i : Integer) x)) (build 10 (lam (i : Integer) i))))"
    )
    type_propagate_decls([expected2], prelude_symtab)
    assert actual2 == expected2

    # But, don't allow lifting an expression that refers to the build/lam-bound 'i':
    e3 = parse_expr_string(
        "(build 10 (lam (i : Integer) (let (x (add 5 i)) (if (gt i 5) x i))))"
    )
    expected3 = parse_expr_string(
        "(build 10 (lam (i : Integer) (if (gt i 5) (let (x (add 5 i)) x) (let (x (add 5 i)) i))))"
    )
    type_propagate_decls([e3, expected3], prelude_symtab)
    match3 = utils.single_elem(list(rules.find_all_matches(e3)))
    actual3 = match3.apply_rewrite()
    assert actual3 == expected3

    # Now we should be able to lift either of those let's, but not the 'if'
    actual4s = [m.apply_rewrite() for m in rules.find_all_matches(actual3)]
    expected4s = [
        parse_expr_string(
            "(build 10 (lam (i : Integer) (let (x (add 5 i)) (if (gt i 5) x (let (x (add 5 i)) i)))))"
        ),
        parse_expr_string(
            "(build 10 (lam (i : Integer) (let (x (add 5 i)) (if (gt i 5) (let (x (add 5 i)) x) i))))"
        ),
    ]
    type_propagate_decls(expected4s, prelude_symtab)
    assert actual4s == expected4s
