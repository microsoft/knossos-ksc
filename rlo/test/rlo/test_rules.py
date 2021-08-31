# fmt: off
import math
import os
import pickle
import random
import tempfile
import numpy
import pytest

from rlo.expression import Expression, RewriteTarget, EF
from rlo.expression_util import SymtabAndDefs
from rlo import rewrites
from rlo.sparser import parse_expr, parse_defs
from ksc.type import Type
from rlo.utils import single_elem

from testutils import make_toplevel as MT

def check_eq(rule, target, result):
    assert RewriteTarget(target).apply_rule(rule) == result

def check_eq_parsed(rule, target, result):
    check_eq(rule, parse_expr(target), parse_expr(result))

def check_nowhere_applicable(rule, expr):
    assert len(list(rule.get_all_rewrites_expr(expr))) == 0

def check_nowhere_applicable_parsed(rule, target):
    check_nowhere_applicable(rule, parse_expr(target))

def apply_in_only_location(rule, expr):
    rewrite = single_elem(list(rule.get_all_rewrites_expr(expr)))
    return rewrite.apply_expr(expr)

def test_subst_template_nested():
    x = Expression.Variable("x", type=Type.Float)
    y = Expression.Variable("y", type=Type.Float)
    a = (x + 1.0) * y
    b = rewrites.subst_template(a, {"x": Expression.Constant(1.3), "y": y})
    assert str(a) == "(mul (add x 1.0) y)"
    assert str(b) == "(mul (add 1.3 1.0) y)"

def test_subst_template_bound():
    pat = EF.Let("x", "e", "e2")
    subst = rewrites.subst_template(pat,
        {"x": Expression.Variable("y"), "e": Expression.Variable("e"), "e2": EF.Add("y", 1)})
    assert subst == EF.Let("y", "e", EF.Add("y", 1))
    # If we'd avoided renaming bound variables we'd have got:
    assert subst != EF.Let("x", "e", EF.Add("y", 1))

def test_subst_template_multi():
    x = Expression.Variable("x")
    y = Expression.Variable("y")

    a = x * y
    b = rewrites.subst_template(a, {"x": Expression.Constant(1.0), "y": Expression.Constant(2.0)})
    assert str(a) == "(mul x y)"
    assert str(b) == "(mul 1.0 2.0)"

def test_delete_let():
    check_eq_parsed(rewrites.rule("delete_let"), "(let a (mul x x) (div 1.0 (add 1.0 x)))", "(div 1.0 (add 1.0 x))")

def test_apply_preserves_symtab():
    x = Expression.Variable("x", Type.Integer)
    sd = SymtabAndDefs(symtab={"f": Type.Lam(Type.Integer, Type.Float)})
    exprenv = sd.make_toplevel(EF.Let("a", EF.Mul(x, x), EF.Apply("f", 3)))
    rewrite = single_elem(list(rewrites.rule("delete_let").get_all_rewrites(exprenv)))
    without_a = rewrite.apply(exprenv)
    assert without_a.env is sd

def test_constant_rules():
    c1 = Expression.Constant(random.random())
    while True:
        c2 = Expression.Constant(random.random())
        if c1 != c2: break
    c1c2 = EF.Tuple(c1, c2)
    check_eq(rewrites.rule("cprop_add"), c1 + c2, Expression.Constant(c1.value + c2.value))
    check_eq(rewrites.rule("cfold"), EF.Apply("add$ff", c1c2), Expression.Constant(c1.value + c2.value))
    check_eq(rewrites.rule("cprop_sub"), c1 - c2, Expression.Constant(c1.value - c2.value))
    check_eq(rewrites.rule("cfold"), EF.Apply("sub$ff", c1c2), Expression.Constant(c1.value - c2.value))
    check_eq(rewrites.rule("cprop_mul"), c1 * c2, Expression.Constant(c1.value * c2.value))
    check_eq(rewrites.rule("cfold"), EF.Apply("mul$ff", c1c2), Expression.Constant(c1.value * c2.value))
    check_eq(rewrites.rule("cprop_div"), c1 / c2, Expression.Constant(c1.value / c2.value))
    check_eq(rewrites.rule("cfold"), EF.Apply("div$ff", c1c2), Expression.Constant(c1.value / c2.value))
    check_eq(rewrites.rule("cprop_log"), EF.Log(c1), Expression.Constant(numpy.log(c1.value)))
    check_eq(rewrites.rule("cfold"), EF.Apply("log$f", c1), Expression.Constant(numpy.log(c1.value)))
    check_eq(rewrites.rule("cprop_exp"), EF.Exp(c1), Expression.Constant(numpy.exp(c1.value)))
    check_eq(rewrites.rule("cfold"), EF.Apply("exp$f", c1), Expression.Constant(numpy.exp(c1.value)))

    check_eq(rewrites.rule("cprop_eq"), EF.Eq(c1, c2), Expression.Constant(False))
    check_eq(rewrites.rule("cfold"), EF.Apply("eq$ff", c1c2), Expression.Constant(False))
    check_nowhere_applicable(rewrites.rule("cprop_eq"), EF.Eq("a", "a"))
    # Just be pedantic that the constants are different objects
    check_eq(rewrites.rule("cprop_eq"), EF.Eq(c1, Expression.Constant(c1.value)), Expression.Constant(True))
    check_eq(rewrites.rule("cfold"), EF.Apply("eq$ff", EF.Tuple(c1, c1.value)), Expression.Constant(True))

    check_eq(rewrites.rule("cprop_gt"), c1 > c2, Expression.Constant(c1.value > c2.value))
    check_eq(rewrites.rule("cfold"), EF.Apply("gt$ff", c1c2), Expression.Constant(c1.value > c2.value))

    # Since we know c1 != c2, of course gt and gte are equivalent here:
    check_eq(rewrites.rule("cprop_gteq"), EF.Gte(c1, c2), Expression.Constant(c1.value >= c2.value))
    check_eq(rewrites.rule("cfold"), EF.Apply("gte$ff", c1c2), Expression.Constant(c1.value >= c2.value))
    # Test equality too.
    check_eq(rewrites.rule("cprop_gteq"), EF.Gte(c1, c1.value), Expression.Constant(True))
    check_eq(rewrites.rule("cfold"), EF.Apply("gte$ff", EF.Tuple(c1, c1.value)), Expression.Constant(True))


    # Since we know c1 != c2, of course gt and gte are equivalent here:
    check_eq(rewrites.rule("cprop_gteq"), EF.Gte(c1, c2), Expression.Constant(c1.value >= c2.value))
    # Test equality too.
    check_eq(rewrites.rule("cprop_gteq"), EF.Gte(c1, c1.value), Expression.Constant(True))


def test_simplify_rules():
    check_eq_parsed(rewrites.rule("add_zero"), "(add 0 (add a b))", "(add a b)")
    check_nowhere_applicable_parsed(rewrites.rule("add_zero"), "(add (add a b) 0)")
    check_eq_parsed(rewrites.rule("mul_zero"), "(mul 0 (add a b))", "0.0") # (if (isinfinite a) a 0.0)
    check_nowhere_applicable_parsed(rewrites.rule("mul_zero"), "(mul 1 (add a b))") # (if (isinfinite a) a 0.0)
    check_eq_parsed(rewrites.rule("mul_one"), "(mul 1.0 (add a b))", "(add a b)")
    check_nowhere_applicable_parsed(rewrites.rule("mul_one"), "(mul (add a b) 1.0)")
    check_eq_parsed(rewrites.rule("sub_zero"), "(sub (mul x y) 0.0)", "(mul x y)")
    check_nowhere_applicable_parsed(rewrites.rule("sub_zero"), "(sub 0.0 (mul x y))")
    check_eq_parsed(rewrites.rule("div_one"), "(div (add a b) 1.0)", "(add a b)")
    check_nowhere_applicable_parsed(rewrites.rule("div_one"), "(div (add a b) (div x x))")
    check_eq_parsed(rewrites.rule("add_neg_to_sub"), "(add 6.0 (mul (div a b) -1.0))", "(sub 6.0 (div a b))")
    check_nowhere_applicable_parsed(rewrites.rule("add_neg_to_sub"), "(add 6.0 (mul -1.0 (div a b)))")
    check_eq_parsed(rewrites.rule("commute_mul"), "(mul e1 (add a b))", "(mul (add a b) e1)")
    check_nowhere_applicable_parsed(rewrites.rule("commute_mul"), "(div e e)")
    # As documentation of behaviour - we do not rule out the two operands being identical:
    check_eq_parsed(rewrites.rule("commute_mul"), "(mul a a)", "(mul a a)")
    check_nowhere_applicable_parsed(rewrites.rule("commute_mul"), "(div a a)")
    check_eq_parsed(rewrites.rule("commute_add"), "(add (exp 5) (log 3))", "(add (log 3) (exp 5))")
    check_nowhere_applicable_parsed(rewrites.rule("commute_add"), "(sub (exp 5) (log 3))")
    check_eq_parsed(rewrites.rule("sub_self"), "(sub (mul a c) (mul a c))", "0.0")
    check_nowhere_applicable_parsed(rewrites.rule("sub_self"), "(sub (mul a c) (mul c a))")
    check_eq_parsed(rewrites.rule("assoc_add_add"), "(add (add 5 (mul x y)) 7)", "(add (add 5 7) (mul x y))")
    check_nowhere_applicable_parsed(rewrites.rule("assoc_add_add"), "(add (sub 5 (mul x y)) 7)")
    check_eq_parsed(rewrites.rule("assoc_sub_add"), "(sub (add 5 (mul x y)) 5)", "(add (sub 5 5) (mul x y))")
    check_nowhere_applicable_parsed(rewrites.rule("assoc_sub_add"), "(sub (sub 5 (mul x y)) 5)")
    check_eq_parsed(rewrites.rule("assoc_add_sub"), "(add (sub 4 (mul x y)) 9)", "(sub (add 4 9) (mul x y))")
    check_nowhere_applicable_parsed(rewrites.rule("assoc_add_sub"), "(add (add 4 (mul x y)) 9)")
    check_eq_parsed(rewrites.rule("assoc_sub_sub"), "(sub (sub 5 (mul x y)) 8)", "(sub (sub 5 8) (mul x y))")
    check_nowhere_applicable_parsed(rewrites.rule("assoc_sub_sub"), "(sub (add 5 (mul x y)) 8)")
    check_eq_parsed(rewrites.rule("sub_sub_to_sub_add"), "(sub 3 (sub (mul x y) 7))", "(sub (add 3 7) (mul x y))")
    check_nowhere_applicable_parsed(rewrites.rule("sub_sub_to_sub_add"), "(sub 3 (add (mul x y) 7))")
    check_eq_parsed(rewrites.rule("assoc_mul_mul"), "(mul (mul 2 (add x y)) 7)", "(mul (mul 2 7) (add x y))")
    check_nowhere_applicable_parsed(rewrites.rule("assoc_mul_mul"), "(mul (div 2 (add x y)) 7)")
    check_eq_parsed(rewrites.rule("assoc_div_mul"), "(div (mul 8 (add x y)) 4)", "(mul (div 8 4) (add x y))")
    check_nowhere_applicable_parsed(rewrites.rule("assoc_div_mul"), "(mul (div 8 (add x y)) 4)")
    check_eq_parsed(rewrites.rule("assoc_mul_div"), "(mul (div 2 (add x y)) 3)", "(div (mul 2 3) (add x y))")
    check_nowhere_applicable_parsed(rewrites.rule("assoc_mul_div"), "(div (mul 2 (add x y)) 3)")
    check_eq_parsed(rewrites.rule("assoc_div_div"), "(div (div 8 (add x y)) 2)", "(div (div 8 2) (add x y))")
    check_nowhere_applicable_parsed(rewrites.rule("assoc_div_div"), "(div (mul 8 (add x y)) 2)")
    check_eq_parsed(rewrites.rule("div_by_div"), "(div 2 (div (add x y) 3))", "(div (mul 2 3) (add x y))")
    check_nowhere_applicable_parsed(rewrites.rule("div_by_div"), "(div 2 (sub (add x y) 3))")
    check_eq_parsed(rewrites.rule("div_of_div"), "(div (div (add x y) 2) 3)", "(div (add x y) (mul 2 3))")
    check_nowhere_applicable_parsed(rewrites.rule("div_of_div"), "(div (mul (add x y) 2) 3)")
    check_eq_parsed(rewrites.rule("mul_by_add"), "(mul 5.0 (add 0.2 z))", "(add (mul 5.0 0.2) (mul 5.0 z))")
    check_nowhere_applicable_parsed(rewrites.rule("mul_by_add"), "(mul 5 (div 0.2 z))")
    check_eq_parsed(rewrites.rule("mul_by_sub"), "(mul 5.0 (sub z 0.2))", "(sub (mul 5.0 z) (mul 5.0 0.2))")
    check_nowhere_applicable_parsed(rewrites.rule("mul_by_sub"), "(mul 5.0 (add (sub z 0.2) 0.0))")
    check_eq_parsed(rewrites.rule("add_of_muls"), "(add (mul (exp z) 5) (mul (exp z) 7))", "(mul (exp z) (add 5 7))")
    check_nowhere_applicable_parsed(rewrites.rule("add_of_muls"), "(add (mul (exp z) 5) (mul 5 (exp z)))")
    check_eq_parsed(rewrites.rule("sub_of_muls"), "(sub (mul (log a) 3) (mul (log a) 4))", "(mul (log a) (sub 3 4))")
    check_nowhere_applicable_parsed(rewrites.rule("sub_of_muls"), "(sub (mul (log a) 3) (mul (log (mul a a)) 2))")
    check_eq_parsed(rewrites.rule("add_of_divs"), "(add (div 5 (log y)) (div 3 (log y)))", "(div (add 5 3) (log y))")
    check_nowhere_applicable_parsed(rewrites.rule("add_of_divs"), "(add (div 5 (log y)) (div 3 (log z)))")
    check_eq_parsed(rewrites.rule("sub_of_divs"), "(sub (div y x) (div z x))", "(div (sub y z) x)")
    check_nowhere_applicable_parsed(rewrites.rule("sub_of_divs"), "(sub (div y x) (div y z))")
    check_eq_parsed(rewrites.rule("div_by_mul"), "(div 8 (mul 2 (add a b)))", "(div (div 8 2) (add a b))")
    check_nowhere_applicable_parsed(rewrites.rule("div_by_mul"), "(div 8 (add (add a b) (add a b)))")
    check_eq_parsed(rewrites.rule("div_by_self"), "(div (mul x y) (mul x y))", "1.0")
    check_nowhere_applicable_parsed(rewrites.rule("div_by_self"), "(div (mul x y) (mul y x))")

def test_vector_tuple_rules():
    # These are arguably a bit redundant, they follow straightforwardly from the representation as ParsedRules
    check_eq_parsed(rewrites.rule("mk_sumbuild"), "(sum (build n (lam x b)))", "(sumbuild n (lam x b))")
    check_eq_parsed(rewrites.rule("sumbuild_of_deltavec"), "(sumbuild n (lam x (deltaVec n x v)))", "(build n (lam x v))")
    check_nowhere_applicable_parsed(rewrites.rule("sumbuild_of_deltavec"), "(sumbuild n (lam x (deltaVec m i v)))")

    check_eq_parsed(rewrites.rule("sumbuild_of_build"), "(sumbuild o (lam oi (build n (lam ni e))))", "(build n (lam ni (sumbuild o (lam oi e))))")
    check_eq_parsed(rewrites.rule("sum_of_deltavecs"), "(add (deltaVec n i e1) (deltaVec n i e2))", "(deltaVec n i (add e1 e2))")

    check_eq_parsed(rewrites.rule("sumbuild_of_sum"), "(sumbuild n (lam i (add (mul i 2) (div i 2))))", "(add (sumbuild n (lam i (mul i 2))) (sumbuild n (lam i (div i 2))))")

    check_eq_parsed(rewrites.rule("sumbuild_other_deltavec"), "(sumbuild n (lam x (deltaVec m i v)))", "(deltaVec m i (sumbuild n (lam x v)))")

    check_eq_parsed(rewrites.rule("sumbuild_of_tuple"), "(sumbuild n (lam x (tuple (f x) (g x))))", "(tuple (sumbuild n (lam x (f x))) (sumbuild n (lam x (g x))))")
    check_eq_parsed(rewrites.rule("add_tuples"), "(add (tuple x y) (tuple a b))", "(tuple (add x a) (add y b))")
    check_eq_parsed(rewrites.rule("mul_tuple_scalar"), "(mul (tuple x y) z)", "(tuple (mul x z) (mul y z))")

    check_eq_parsed(rewrites.rule("sumbuild_invariant"), "(sumbuild n (lam x c))", "(mul (to_float n) c)")
    check_nowhere_applicable_parsed(rewrites.rule("sumbuild_invariant"), "(sumbuild n (lam x (add x 1)))")

    x = Expression.Variable("x")
    y = Expression.Variable("y")
    check_eq(rewrites.rule("select_of_tuple"), EF.Select(EF.Tuple(x,y), 0), x)
    check_eq_parsed(rewrites.rule("select_of_tuple"), "(get$1$2 (tuple a b))", "a")

def check_parsed_rule(rule):
    # This tests that algebraic identites e.g. a+0.0 => a evaluate to the same with random
    # float values of all the variables except predicates, and all combinations of True/False.
    # It won't handle e.g. let-expressions or builds.
    start_env = {}
    bool_vars = set() # we will try all combinations of True/False
    for arg_name, typ in rule._var_types.items():
        if typ == Type.Bool:
            bool_vars.add(arg_name)
        elif typ == Type.Float or typ is None:
            # Patterns that work on variables of any type, we test with random floats.
            start_env[arg_name] = random.random()
        elif typ == Type.Integer:
            r = random.randint(-10, 10)
            # Avoid testing division by zero. We'd need to check both sides did/n't throw, together.
            start_env[arg_name] = r+1 if r >= 0 else r
        else:
            assert False, f"Don't know how to make values of type {typ}"

    infix_ops = {"add": "+", "sub": "-", "mul": "*", "div": "/", "gt": ">", "gte": ">=", "eq": "=="}
    def eval_exp(e, env):
        if e.op == "variable": return env[e.name]
        if e.op == "constant": return e.value
        if e.op == "log": return numpy.log(eval_exp(e.only_child, env))
        if e.op == "exp": return numpy.exp(eval_exp(e.only_child, env))
        if e.op == "if":
            cond = eval_exp(e.first, env)
            return eval_exp(e.second if cond else e.third, env)
        if e.op == "apply":
            # Use the constant folding rule - so build constant-folded sub-expressions
            args = e.right.children if e.right.op == "tuple" else [e.right]
            assert (len(args) == 1) ^ (e.right.op == "tuple")
            args = [Expression.Constant(eval_exp(arg, env)) for arg in args]
            apply_to_constant = e.clone_with_new_children([
                e.left,  # The function name
                # And the arguments - may be a Tuple or a single value
                args[0] if len(args) == 1 else EF.Tuple(*args)
            ])
            res = apply_in_only_location(rewrites.rule("cfold"), apply_to_constant)
            assert res.op == "constant"
            return res.value
        # Use python eval with infix op for remainder
        return eval("{} {} {}".format(eval_exp(e.left, env), infix_ops[e.op], eval_exp(e.right, env)))
    def test(env, cvs_needing_values):
        if len(cvs_needing_values) == 0:
            assert math.isclose(eval_exp(rule.lhs, env), eval_exp(rule.rhs, env))
        else:
            test({**env, cvs_needing_values[0]: True}, cvs_needing_values[1:])
            test({**env, cvs_needing_values[0]: False}, cvs_needing_values[1:])
    test(start_env, list(bool_vars))

def test_parsed_rules():
    for rules_name in list(rewrites.available_rules()):
        print("{}...".format(rules_name))
        for rule in rewrites.get_rules(rules_name):
            if isinstance(rule, rewrites.ParsedRule):
                if not any(n.is_binder or n.makes_vec for n in rule.lhs.nodes):
                    check_parsed_rule(rule)

def test_parsed_rule_capture():
    # If the RHS introduces a new bound variable, then it needs to be renamed
    # into a fresh variable when the rule is applied, to avoid capture
    r = rewrites.ParsedRule('(rule foo1 (x : Any) (mul x 3) (let y (add x x) (add y x)))')
    e = parse_expr('(let y 2 (mul (add y 1) 3))')
    e1 = parse_expr('''
        (let y 2 (let var0 (add (add y 1) (add y 1)) (add var0 (add y 1))))
    ''')
    assert RewriteTarget(e).third.apply_rule(r) == e1

    # Does it still work if target is using var0?
    e = parse_expr('(let var0 2 (mul (add var0 1) 3))')
    e1 = parse_expr('''
        (let var0 2
            (let var1 (add (add var0 1) (add var0 1)) (add var1 (add var0 1))))
    ''')
    assert RewriteTarget(e).third.apply_rule(r) == e1

    # Test for other binders like lam
    r = rewrites.ParsedRule('''
        (rule lamfoo (e2 : Any)
            (let f (lam (x : Integer) (mul x 3)) e2)
            (let g (lam (y : Integer) (mul y 2))
                (let f (lam (x : Integer) (add x (g x))) e2))
        )
    ''')
    e = parse_expr('(let h (lam (z : Integer) (mul z 3)) (h 4))')
    e1 = parse_expr('''
        (let var0 (lam (var1 : Integer) (mul var1 2))
            (let h (lam (z : Integer) (add z (var0 z))) (h 4)))
    ''')
    assert RewriteTarget(e).apply_rule(r) == e1

    # Test for build (this rule isn't "correct", but it tests the renaming):
    r = rewrites.ParsedRule('''
        (rule buildfoo (x : Integer)
            (mul x 3)
            (build 10 (lam (i : Integer) (mul x 3)))
         )
    ''')
    e = parse_expr('(let i 2 (mul (add i 1) 3))')
    e1 = parse_expr('''
        (let i 2 (build 10 (lam (var0 : Integer) (mul (add i 1) 3))))
    ''')
    assert RewriteTarget(e).third.apply_rule(r) == e1

    # Bound variables in the RHS should not be rewritten if they are matched
    # by the LHS:
    r = rewrites.ParsedRule('(rule foo3 ((y : Integer) (z : Integer)) (let x (add y 0) z) (let x y z))')
    e = parse_expr('(let v (add 33 0) (mul v 3))')
    assert RewriteTarget(e).apply_rule(r) == parse_expr('(let v 33 (mul v 3))')

def test_rule_does_not_invalidate():
    x = Expression.Variable("x")
    a = Expression.Variable("a")

    e = EF.Let(a, x + 1, a * 2) + x
    assert str(e) == "(add (let (a (add x 1)) (mul a 2)) x)"
    e2 = RewriteTarget(e).left.apply_rule(rewrites.rule("lift_bind"))
    assert str(e2) == "(let (a (add x 1)) (add (mul a 2) x))"
    assert str(e) == "(add (let (a (add x 1)) (mul a 2)) x)"
    assert str(e.left.second) == "(add x 1)"
    # Also check we reused the same tree for the bound value
    # (We could check others but this'll do.)
    assert id(e.left.second) == id(e2.second)

def test_rule_alpha_conversion():
    x = Expression.Variable("x")
    var0 = Expression.Variable("var0")
    var1 = Expression.Variable("var1")
    e = (EF.Let(var0, x, var0) * 1.0) + (EF.Let(var1, x, var1) * 2.0)
    # Consider applying each rule only to the topmost expression
    rewritten = [rewrite.apply_expr(e) for rewrite in rewrites.get_rules("simplify_rules").get_all_rewrites_expr(e) if rewrite.node_id == 0]
    target = EF.Let(var0, x, var0) * (Expression.Constant(1.0) + 2.0)
    assert target in rewritten

    e = (1.0 / EF.Let(var0, x, var0)) + (2.0 / EF.Let(var1, x, var1))
    rewritten = [rewrite.apply_expr(e) for rewrite in rewrites.get_rules("simplify_rules").get_all_rewrites_expr(e) if rewrite.node_id == 0]
    target = (Expression.Constant(1.0) + 2.0) / (EF.Let(var0, x, var0))
    assert target in rewritten

def test_rule_lam():
    # checking new_bind2 and lift_bind1 is not needed - they do not support lam op
    a = Expression.Variable("a")
    x = Expression.Variable("x", Type.Float)
    y = Expression.Variable("y")

    # Check that we cannot lift a bind out of a lambda (as this would break the KSC first-order restriction)
    # Check we can lift a bind out of a lambda where the bound value does not refer to the lambda's parameter
    e1 = EF.Lam(x, EF.Let(y, a * 3.0, x + y))
    check_nowhere_applicable(rewrites.rule("lift_bind"), e1)

def test_inline_call():
    e = dict(parse_defs("(def foo Float (x : Float) (add x 1.0)) (def bar Float (y : Float) (mul (foo y) 2.0))"))['bar'].expr
    assert single_elem([n for n in e.nodes if n.op == "apply"]).left.name == "foo"
    check_nowhere_applicable(rewrites.rule("delete_def"), e)

    inlined = apply_in_only_location(rewrites.rule("inline_call"), e)
    check_nowhere_applicable(rewrites.rule("inline_call"), inlined)
    assert all(n.op != "apply" for n in inlined.nodes)

    deleted = apply_in_only_location(rewrites.rule("delete_def"), inlined)
    check_nowhere_applicable(rewrites.rule("inline_call"), deleted)
    check_nowhere_applicable(rewrites.rule("delete_def"), deleted)
    assert len([n for n in deleted.nodes if n.op == "lam"]) == 1

def test_if_rules():
    x = Expression.Variable("x", Type.Float)
    y = Expression.Variable("y")

    # Check we cannot lift an if over a lambda (as this would break the KSC first-order restriction)
    e2 = EF.Lam(x, EF.If(y > 1.0, x + 1.0, x - 1.0))
    check_nowhere_applicable(rewrites.rule("lift_if"), e2)

    assert RewriteTarget(parse_expr("(build n (lam x (if c (add x 1) (mul x 2))))")).third.apply_rule(rewrites.rule("lift_if")) \
            == parse_expr("(if c (build n (lam x (add x 1))) (build n (lam x (mul x 2))))")
    check_nowhere_applicable_parsed(rewrites.rule("lift_if"), "(build n (lam x (if (eq x 2) t f)))")
    assert RewriteTarget(parse_expr("(sumbuild n (lam x (if c t f)))")).third.apply_rule(rewrites.rule("lift_if")) \
                == parse_expr("(if c (sumbuild n (lam x t)) (sumbuild n (lam x f)))")

def test_lifting():
    x = Expression.Variable("x")
    y = Expression.Variable("y")
    # Lift let out of build that does not refer to the variable bound by the build
    assert RewriteTarget(EF.Build(10, x, EF.Let("a", y+1, "a"))).third.apply_rule(rewrites.rule("lift_bind")) \
        == EF.Let("a", y+1, EF.Build(10, x, "a"))
     # Cannot lift if the value bound by the let refers to the build-bound variable
    check_nowhere_applicable(rewrites.rule("lift_bind"), EF.Build(10, x, EF.Let("a", x+1, "a")))

def test_rules_with_stop_node():
    x = Expression.Variable("x")
    y = Expression.Variable("y")

    def check_rewrites(e, rules, num_apply, expect_stops=False):
        can_apply = [rewrite.node_id for rewrite in rules.get_all_rewrites_expr(e)]
        assert any(e.nodes[n].op == "stop" for n in can_apply) == expect_stops
        assert len(can_apply) == num_apply

    # simplify_rules
    e1 = x * y + EF.Stop(x * x)
    check_rewrites(e1, rewrites.simplify_rules, 2)

    e2 = (1.0 / x) / (EF.Stop(1.0 / x) + 1.0)
    check_rewrites(e2, rewrites.simplify_rules, 3)

    e3 = EF.Stop(x * x * x / x)
    check_rewrites(e3, rewrites.simplify_rules, 0)


    # if_rules + simplify_rules
    e1 = parse_expr("(stop (if x 3 4))")
    check_rewrites(e1, rewrites.if_rules + rewrites.simplify_rules, 0)

    e2 = parse_expr("(add (if (stop (add x (div x 1.0))) 3 4) 2)")
    check_rewrites(e2, rewrites.if_rules + rewrites.simplify_rules, 2)

    # binding_rules
    e1 = EF.Stop(1.0 + EF.Let(x, y, x + 1))
    check_rewrites(e1, rewrites.binding_rules, 0, False)

    e2 = (1.0 / x) / (1.0 + EF.Stop(1.0 / x))
    check_rewrites(e2, rewrites.binding_rules, 3, True)

    e3 = (x / y) / (1.0 + EF.Stop(x / y) + (x / y))
    check_rewrites(e3, rewrites.binding_rules, 5, True)


    # build_simplify_rules
    e1 = parse_expr("(let x (build 10 (lam y (add y 1))) (index (index 5 (stop(x))) x))")
    check_rewrites(e1, rewrites.get_rules("build_simplify_rules"), 2)

    e2 = parse_expr("(let x (build 10 (lam y (add y 1))) (index (index 5 (stop(x))) (stop(x))))")
    check_rewrites(e2, rewrites.get_rules("build_simplify_rules"), 1)

def test_inline_let_avoids_capture():
    e = parse_expr("(let x (add y 1) (let y other (mul x 2)))")
    inlined = apply_in_only_location(rewrites.rule("inline_let"), e)
    assert inlined == parse_expr("(let x (add y 1) (let y2 other (mul (add y 1) 2)))")
    assert inlined != parse_expr("(let x (add y 1) (let y other (mul (add y 1) 2)))")

def test_inline_let_respects_rebinding():
    # This is more of a test of the RuleMatcher.get_all_rewrites environment, but we need a suitable rule.
    e = parse_expr("(let a other (build 5 (lam (a : Integer) (add a (let a other2 a)))))")
    rewritten = single_elem([r.apply_expr(e) for r in rewrites.rule("inline_let").get_all_rewrites_expr(e)])
    assert rewritten == parse_expr("(let a other (build 5 (lam (a : Integer) (add a (let a other2 other2)))))")
    # None of the other a's can be inlined.

def test_inline_let_rebinding_self():
    e = parse_expr("(let x (add x 1) (mul x 2))") #So that's (the outer x + 1) * 2
    rewritten = single_elem([r.apply_expr(e) for r in rewrites.rule("inline_let").get_all_rewrites_expr(e)])
    assert rewritten == parse_expr("(let x2 (add x 1) (mul (add x 1) 2))") # Still (the outer x + 1) * 2
    # If we didn't rename:
    assert rewritten != parse_expr("(let x (add x 1) (mul (add x 1) 2))") # We'd get (the outer x + 1 + 1) * 2

def test_inline_call_avoids_capture():
    edef = "(edef g Float (Float))"
    f = dict(parse_defs(edef + "(def f Float (x : Float) (add (g x) 1.0))  (def g Float (x : Float) (add x 1.0))  (def h Float (x : Float) (f (mul x 2.0)))"))["h"].expr
    # Note f refers to some outer function "g" not the one defined here
    inlined = apply_in_only_location(rewrites.rule("inline_call"), f)
    without_f = RewriteTarget(inlined).apply_rule(rewrites.rule("delete_let"))
    assert without_f == dict(parse_defs(edef + "(def g2 Float (x : Float) (add x 1.0))  (def h Float (x : Float) (let x2 (mul x 2.0) (add (g x2) 1.0)))"))["h"].expr
    # Vs forgetting to rename g:
    assert without_f != dict(parse_defs("(def g Float (x : Float) (add x 1.0))  (def h Float (x : Float) (let x2 (mul x 2.0) (add (g x2) 1.0)))"))["h"].expr

def test_inline_call_renames_in_argument():
    edef = "(edef g Float (Float))"
    f = dict(parse_defs(edef + "(def f Float (x : Float) (add (g x) 1.0))  (def g Float (x : Float) (add x 1.0))  (def h Float (x : Float) (f (g x)))"))["h"].expr
    # The 'g' in 'f' refers to a different g than is used on the argument to h before calling f
    actions = list(rewrites.rule("inline_call").get_all_rewrites_expr(f))
    assert len(actions) == 2
    nodes = f.nodes
    assert all(nodes[act.node_id].op == "apply" for act in actions)
    f_inlined = single_elem([act for act in actions if nodes[act.node_id+1].name == "f"]).apply_expr(f)
    without_f = RewriteTarget(f_inlined).apply_rule(rewrites.rule("delete_let"))
    assert without_f == dict(parse_defs(edef + "(def g2 Float (x : Float) (add x 1.0))  (def h Float (x : Float) (let x (g2 x) (add (g x) 1.0)))"))["h"].expr
    # Forgot to apply renaming inside the argument:
    assert without_f != dict(parse_defs(edef + "(def g2 Float (x : Float) (add x 1.0))  (def h Float (x : Float) (let x (g x) (add (g x) 1.0)))"))["h"].expr

def test_monomorphic():
    _,e = parse_defs("(def foo Float ((a : Float) (b : Float) (c : Float)) (add$ff (mul$ff a b) (mul$ff a c)))")[-1]
    mono_rules = rewrites.get_rules("monomorphic_rules")
    assert sorted([rw.rule_name for rw in mono_rules.get_all_rewrites(e)]) == [ "add_of_muls$f", "commute_add$f", "commute_mul$f", "commute_mul$f"]

    rewritten = single_elem(list(rewrites.rule("add_of_muls$f").get_all_rewrites(e))).apply(e) # ExprWithEnv
    assert sorted([rw.rule_name for rw in mono_rules.get_all_rewrites(rewritten)]) == ["commute_add$f", "commute_mul$f", "mul_by_add$f"]

    check_nowhere_applicable(rewrites.RuleSet(rewrites._algebraic_rules), e.expr)
    assert all(rw.rule_name == "inline_let" for rw in rewrites.get_rules("simplify_rules").get_all_rewrites(e))

    check_nowhere_applicable(mono_rules, EF.Add(EF.Mul("a", "b"), EF.Mul("a", "c")))

def assert_rules_same(actual, expected):
    # we want to test tuples
    assert not isinstance(actual, rewrites.RuleMatcher)
    assert len(actual) == len(expected)  # not actually necessary, but good
    for act, exp in zip(actual, expected):
        assert act is exp

def test_rule_pickling():
    rule_sets = [rewrites.get_rules(name) for name in rewrites.available_rules()]
    with tempfile.TemporaryDirectory() as save_dir:
        for rule_set in rule_sets:
            rules = tuple(rule_set)
            path = os.path.join(save_dir, f"{rule_set.rules_name}.pck")
            with open(path, "wb") as fp:
                pickle.dump(rules, fp)

            with open(path, "rb") as fp:
                loadeds = pickle.load(fp)

            assert_rules_same(rules, loadeds)


def _test_helper(expression, rule_set):
    """Used in test_multiprocessing_transportable."""
    rw = rule_set.get_all_rewrites_expr(expression)
    return tuple(rw), tuple(rule_set)


def test_multiprocess_transportable():
    import multiprocessing as mp
    expression = parse_expr("(let (a (div 1.0 x)) (div a (add a 1.0)))")
    rules = rewrites.get_rules("simplify_rules")
    expected = _test_helper(expression, rules)
    with mp.Pool() as pool:
        actual = pool.apply(_test_helper, args=(expression, rules))

    for act, exp in zip(actual[0], expected[0]):
        # rewrites
        assert type(act) == type(exp)
        assert act.__dict__ == exp.__dict__
    # rules
    for act, exp in zip(actual[1], expected[1]):
        assert act is exp

def test_rewrite_seq_to_exprenvs():
    e = MT(EF.Let("x", 17, EF.Add("x", 1)))
    exprs = rewrites.rewrite_seq_to_exprenvs(e, [(4, "inline_let"), (0, "delete_let")], strict=True)
    assert len(exprs) == 3
    assert exprs[0] == e
    assert exprs[-1] == MT(EF.Add(17, 1))

    bad_sequence = [(4, "inline_let"), (4, "inline_let")]
    with pytest.raises(ValueError):
        rewrites.rewrite_seq_to_exprenvs(e, bad_sequence, strict=True)
    # Without strict, we should get back *something*
    exprs = rewrites.rewrite_seq_to_exprenvs(e, bad_sequence)
    assert exprs[0] == e
