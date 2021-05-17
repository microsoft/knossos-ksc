from typing import Iterator
from ksc.expr import Expr, Call, Match, If, Rule
from ksc.rewrites import RuleMatcher, Location

@singleton
class lift_if_over_call(RuleMatcher):
    possible_filter_terms = frozenset([Call])
    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Match]:
        for i,arg in enumerate(subtree.args):
            if isinstance(arg, If):
                yield Match(self, root, path_from_root + (i,))

    def apply_at(self, expr: Expr, path: Location) -> Expr:
        def apply_here(const_zero: Expr, call_node: Expr) -> Expr:
            assert const_zero == Const(0.0)  # Passed to replace_subtree below
            which_child = path[-1]
            if_node = call_node.args[which_child]
            args_t = call_node.args[:]  # copy
            args_t[which_child] = if_node.t_body
            args_f = call_node.args[:]  # copy
            args_f[which_child] = if_node.f_body
            return If(if_node.cond,
                Call(call_node.name, args_t, type=call_node.type_),
                Call(call_node.name, args_f, type=call_node.type_),
                type=call_node.type_)
        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(expr, path[:-1], Const(0.0), apply_here)

def can_speculate_ahead_of_condition(e: Expr, cond: Expr, cond_value: bool) -> bool:
    """ Can we speculatively evaluate e, when the original program would not have done so,
        moving <e> to a point that it is evaluated before testing <cond> ?
        <cond_value> indicates the value that <cond> would have to take in order for <e> to have been evaluated. """
    # TODO: check if 'e' might raise an exception if evaluated without testing 'cond' first
    return True

# TODO: extend to allow template_vars with type : Any
lift_if_rules = [
    parse_rule_str(s) for s in [
        '(rule "lift_if_over_let_rhs" ((p : Bool) (t : Any) (f : Any) (e : Any)) (let (x (if p t f)) e) (if p (let (x t) e) (let (x f) e)))',
        '(rule "lift_if_over_if_cond" ((p : Bool) (t : Bool) (f : Bool) (x : Any) (y : Any)) (if (if p t f) x y) (if p (if t x y) (if f x y)))',
        '(rule "lift_if_over_assert_cond" ((p : Bool) (t : Bool) (f : Bool) (body: Any)) (assert (if p t f) body) (if p (assert t body) (assert f body)))',
        '(rule "lift_if_over_assert_body" ((cond: Bool) (p : Bool) (t : Any) (f : Any)) (assert cond (if p t f)) (if p (assert cond t) (assert cond f)))',
    ]
] + [
    parse_rule_str(
        '(rule "lift_if_over_let_body" ((v : Any) (p : Bool) (t : Any) (f : Any)) (let (x v) (if p t f)) (if p (let (x v) t) (let (x v) f)))',
        side_conditions=lambda *,x,v,p,t,f: x.name not in p.free_vars_
    ),
    parse_rule_str(
        '(rule "lift_if_over_if_true" ((p : Bool) (q : Bool) (t : Any) (f : Any) (e : Any)) (if p (if q t f) e) (if q (if p t e) (if p f e)))',
        side_conditions=lambda *,p,q,t,f,e: can_speculate_ahead_of_condition(q, p, True)
    ),
    parse_rule_str(
        '(rule "lift_if_over_if_false" ((p : Bool) (q : Bool) (t : Any) (f : Any) (e : Any)) (if p e (if q t f)) (if q (if p e t) (if p e f)))',
        side_conditions=lambda *,p,q,t,f,e: can_speculate_ahead_of_condition(q, p, False)
    ),
    lift_if_over_call
]

class LetLifter(ParsedRuleMatcher):
    """ A ParsedRuleMatcher that deals with patterns contain (let (x rhs) body). If there is a match, and the rewrite is applied,
        then 'x' will be renamed (in body) to avoid capturing any free variables in any other part of the substitution """
    def __init__(self, rule: Rule, **kwargs)
        super().__init__(self, rule, **kwargs)
        assert "rhs" in self.arg_types.keys() and "body" in self.arg_types.keys()

    def apply_at(self, expr: Expr, path: Location, **substs: VariableSubstitution) -> Expr:
        exprs_not_to_capture = [e for v,e in substs.values() if v not in ["x", "rhs", "body"]]
        bound_var = substs["x"]
        # Avoid capturing any identically-named (but different) variable in exprs_not_to_capture
        if any(bound_var.name in replacement_subexp.free_var_names_ for replacement_subexp in exprs_not_to_capture):
            # Rename x (bound in body)
            nv = make_nonfree_var(bound_var.name, exprs_not_to_capture, type=bound_var.type_)
            substs = {**substs, "x": nv, "body": replace_free_vars(substs["body"], bound_var.name, nv)}
        return super().apply_at(expr, path, **substs)


lift_let_rules = [
    LetLifter( # avoid x capturing in outer
        '(rule "lift_let_over_let_rhs" (let (y (let (x rhs) body)) outer) (let (x rhs) (let (y body) outer))'
    ),
    LetLifter( # avoid x capturing in val
        '(rule "lift_let_over_let_body" (let (y val) (let (x rhs) body) (let (x rhs) (let (y val) body))',
        side_conditions=lambda *, y, val, x, rhs, body: y.name not in rhs.free_vars_
    ),
    LetLifter( # avoid x capturing in t, f
        '(rule "lift_let_over_if_cond" (if (let (x rhs) body) t f) (let (x rhs) (if body t f)))'
    ),
    LetLifter( # avoid x capturing in p, f
        '(rule "lift_let_over_if_true" (if p (let (x rhs) body) f) (let (x rhs) (if p body f)))'
        side_conditions=lambda *, p, rhs, body, f: can_speculate_ahead_of_condition(rhs, p, True)
    ),
    LetLifter( # avoid x capturing in p, t
        '(rule "lift_let_over_if_false" (if p t (let (x rhs) body)) (let (x rhs) (if p t body)))'
        side_conditions=lambda *, p, rhs, body, f: can_speculate_ahead_of_condition(rhs, p, False)
    ),
    LetLifter( # avoid x capturing in val
        '(rule "lift_let_over_assert_cond" (assert (let (x rhs) body) val) (let (x rhs) (assert body val)))'
    ),
    LetLifter( # avoid x capturing in cond
        '(rule "lift_let_over_assert_bady" (assert cond (let (x rhs) body)) (let (x rhs) (assert cond body)))'
        #side_conditions=lambda *, cond, rhs, body: ok_to_evaluate(rhs, cond, False) # But we're gonna fail the assertion anyway, so OK?
    )
]

@singleton
class lift_let_over_call(RuleMatcher):
    possible_filter_terms = frozenset([Call])
    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Match]:
        for i,arg in enumerate(subtree.args):
            if isinstance(arg, Let):
                yield Match(self, root, path_from_root + (i,))

    def apply_at(self, expr: Expr, path: Location) -> Expr:
        # TODO: also need the capture-avoiding logic in LetLifter
        def apply_here(const_zero: Expr, call_node: Expr) -> Expr:
            assert const_zero == Const(0.0)  # Passed to replace_subtree below
            which_child = path[-1]
            let_node = call_node.args[which_child]
            args = call_node.args[:]  # copy
            args[which_child] = let_node.body
            return Let(let_node.vars,
                let_node.rhs,
                Call(call_node.name, args, type=call_node.type_),
                type=call_node.type_)
        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(expr, path[:-1], Const(0.0), apply_here)

# TODO finally, need to work with lam nested within build/sumbuild/etc.