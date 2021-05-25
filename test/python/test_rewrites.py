import pytest

from ksc.alpha_equiv import are_alpha_equivalent
from ksc.expr import Call
from ksc.rewrites import rule, RuleSet, inline_var, delete_let, parse_rule_str
from ksc.parse_ks import parse_expr_string
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


def test_simple_parsed_rule(prelude_symtab):
    r = parse_rule_str(
        '(rule "mul2_to_add$f" (x : Float) (mul x 2.0) (add x x))', prelude_symtab
    )
    input1, expected1 = (
        parse_expr_string("(if p (mul (add a b) 2.0) (mul a 3.0))"),
        parse_expr_string("(if p (add (add a b) (add a b)) (mul a 3.0))"),
    )

    type_propagate_decls(
        [input1, expected1],
        {**prelude_symtab, "p": Type.Bool, "a": Type.Float, "b": Type.Float},
    )
    actual1 = sorted_rewrites(r, input1)
    assert actual1 == [expected1]

    input2, expected2 = (
        parse_expr_string("(mul (to_float (mul x 2)) 2.0)"),
        parse_expr_string("(add (to_float (mul x 2)) (to_float (mul x 2)))"),
    )

    type_propagate_decls([input2, expected2], {**prelude_symtab, "x": Type.Integer})
    actual2 = sorted_rewrites(r, input2)
    assert actual2 == [expected2]


def test_parsed_rule_respects_types(prelude_symtab):
    # Check that we only match subtrees of specified type (without relying on the StructuredNames being different)
    r = parse_rule_str(
        '(rule "rm.let$i" (e : Integer) (let (x e) x) e)', prelude_symtab
    )
    applicable_expr = parse_expr_string("(let (y 4) y)")
    inapplicable_expr = parse_expr_string("(let (z 5.0) z)")

    type_propagate_decls([applicable_expr, inapplicable_expr], prelude_symtab)
    assert sorted_rewrites(r, applicable_expr) == [parse_expr_string("4")]
    assert len(list(r.find_all_matches(inapplicable_expr))) == 0


def test_parsed_rule_binders(prelude_symtab):
    # Check that variables bound in the rule template are correctly substituted with the real program variables
    r = parse_rule_str(
        '(rule "foo" ((e : Integer)) (let (x e) (add x 3)) (add e 3))', prelude_symtab
    )
    expr = parse_expr_string("(let (i 7) (add i 3))")
    expected = parse_expr_string("(add 7 3)")

    type_propagate_decls([expr, expected], prelude_symtab)
    actual = sorted_rewrites(r, expr)
    assert actual == [expected]

    # Also test on lam's.
    r2 = parse_rule_str(
        '(rule "index_of_build$f" ((n : Integer) (idx : Integer) (e : Float)) (index idx (build n (lam (i : Integer) e))) (let (i idx) e))',
        prelude_symtab,
    )
    expr2 = parse_expr_string(
        "(index 7 (build 10 (lam (idx : Integer) (to_float idx))))"
    )
    expected2 = parse_expr_string("(let (idx 7) (to_float idx))")

    type_propagate_decls([expr2, expected2], prelude_symtab)
    actual2 = sorted_rewrites(r2, expr2)
    assert actual2 == [expected2]


def test_parsed_rule_allows_alpha_equivalence(prelude_symtab):
    # Use ts_add because [add (Tuple (Vec Float) (Vec Float))] is not in the prelude (yet)
    r = parse_rule_str(
        '(rule "add2_to_mul$vf" (v : Vec Float) (ts_add v v) (mul 2.0 v))',
        prelude_symtab,
    )
    e = parse_expr_string(
        "(ts_add (build 10 (lam (i : Integer) (to_float i))) (build 10 (lam (j : Integer) (to_float j))))"
    )
    expected = parse_expr_string(
        "(mul 2.0 (build 10 (lam (k : Integer) (to_float k))))"
    )
    type_propagate_decls([e, expected], prelude_symtab)
    actual = sorted_rewrites(r, e)
    assert are_alpha_equivalent(utils.single_elem(actual), expected)


def test_parsed_rule_capture(prelude_symtab):
    # If the RHS introduces a new bound variable, then it needs to be renamed
    # into a fresh variable when the rule is applied, to avoid capture
    r = parse_rule_str(
        '(rule "foo1" (x : Integer) (mul x 3) (let (y (add x x)) (add y x)))',
        prelude_symtab,
    )
    e = parse_expr_string("(let (y 2) (mul (add y 1) 3))")
    expected = parse_expr_string(
        "(let (y 2) (let (t__0 (add (add y 1) (add y 1))) (add t__0 (add y 1))))"
    )
    type_propagate_decls([e, expected], prelude_symtab)
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
    type_propagate_decls([e, expected], prelude_symtab)
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
        prelude_symtab,
    )
    e = parse_expr_string("(let (i 2) (mul (add i 1) 3))")
    expected = parse_expr_string(
        "(let (i 2) (index 0 (build 10 (lam (t__0 : Integer) (mul (add i 1) 3)))))"
    )
    type_propagate_decls([e, expected], prelude_symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected

    # Bound variables in the RHS should not be rewritten if they are matched
    # by the LHS:
    r = parse_rule_str(
        '(rule "foo2" ((y : Integer) (z : Integer)) (let (x (add y 0)) z) (let (x y) z))',
        prelude_symtab,
    )
    e = parse_expr_string("(let (v (add 33 0)) (mul v 3))")
    expected = parse_expr_string("(let (v 33) (mul v 3))")
    type_propagate_decls([e, expected], prelude_symtab)
    actual = utils.single_elem(list(r.find_all_matches(e))).apply_rewrite()
    assert actual == expected


def parsed_rule_side_conditions(prelude_symtab):
    e = parse_expr_string("(if (gt (index 1 v) 0) 2 2)")
    type_propagate(e, prelude_symtab)
    rule_general = parse_rule_str(
        '(rule "if_both_same$i" ((p : Bool) (x : Integer)) (if p x x) x)'
    )
    assert apply_in_only_location(rule_general, e) == parse_expr_string("2")

    # As an example, this side condition makes (very) sure we are not changing
    # an exception-throwing program into a succeeding one
    rule_restricted = parse_rule_str(
        'rule "if_both_same_restricted$i" ((p : Bool) (x : Integer)) (if p x x) x)',
        side_conditions=lambda *, p, x: p.__class__ in [Const, Var],
    )
    check_nowhere_applicable(rule_restricted, e)

    e2 = parse_expr_string("(if True (add 2 3) (add 2 3))")
    assert apply_in_only_location(rule_restricted, e2) == parse_expr_string("(add 2 3)")


def test_polymorphic_rules(prelude_symtab):
    from ksc.rewrites_prim import index_of_build

    symtab = {**prelude_symtab, "v": Type.Tensor(1, Type.Float)}  # Free in expr's below

    map_to_build = parse_rule_str(
        """
       (rule "map_to_build" ((in : Vec Any) (body : Any))
           (map (lam (e : Any) body) in)
           (build (size in) (lam (i : Integer) (let (e (index i in)) body))))""",
        {},
    )
    repl = map_to_build._rule.replacement
    assert map_to_build._rule.template.type_ == repl.type_ == Type.Tensor(1, Type.Any)

    e = parse_expr_string("(index 5 (map (lam (x : Float) (exp x)) v))")
    expected = parse_expr_string(
        # Renaming of binders introduced on the RHS is a bit aggressive atm.
        "(index 5 (build (size v) (lam (t__0 : Integer) (let (x (index t__0 v)) (exp x)))))"
    )
    type_propagate_decls([e, expected], symtab)
    assert e.type_ == expected.type_ == Type.Float

    actual = utils.single_elem(list(map_to_build.find_all_matches(e))).apply_rewrite()
    assert actual == expected
    assert actual.type_ == Type.Float  # Of course - rewrite does not change type
    # Check we figured out correct types for intermediate Expr's created when we applied the rewrite
    assert (
        isinstance(actual, Call)
        and isinstance(actual.args[1], Call)
        and actual.args[1].type_ == Type.Tensor(1, Type.Float)
    )
    lam = actual.args[1].args[1]
    assert lam.type_ == Type.Lam(Type.Integer, Type.Float)
    assert lam.body.rhs.type_ == lam.body.body.type_ == Type.Float

    expected2 = parse_expr_string("(let (t__0 5) (let (x (index t__0 v)) (exp x)))")
    type_propagate_decls([expected2], symtab)
    match = utils.single_elem(list(index_of_build.find_all_matches(actual)))
    actual2 = match.apply_rewrite()
    assert expected2 == actual2
    assert (
        actual2.type_
        == actual2.body.type_
        == actual2.body.rhs.type_
        == actual2.body.body.type_
        == Type.Float
    )


def test_polymorphic_replacement(prelude_symtab):
    # The "Any" here is new in the replacement, but is still figured out by type propagation.
    build_to_map = parse_rule_str(
        """
        (rule "build_to_map" ((inp : Vec Any) (body : Any))
           (build (size inp) (lam (i : Integer) (let (x (index i inp)) body)))
           (map (lam (x : Any) body) inp))""",
        {},
        side_conditions = lambda *, inp, body, i, x: i.name not in body.free_vars_
    )
    e = parse_expr_string(
        "(build (size v) (lam (idx : Integer) (let (elem (index idx v)) (add elem 1.0))))"
    )
    expected = parse_expr_string("(map (lam (elem : Float) (add elem 1.0)) v)")
    symtab = {**prelude_symtab, "v": Type.Tensor(1, Type.Float)}
    type_propagate_decls([e, expected], symtab)
    actual = utils.single_elem(list(build_to_map.find_all_matches(e))).apply_rewrite()
    assert actual == expected


def test_rule_pickling():
    import pickle

    r = pickle.loads(pickle.dumps(inline_var))
    assert r is inline_var
