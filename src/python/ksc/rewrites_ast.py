from abc import ABC, abstractmethod
from typing import Iterable, Iterator, Tuple

from ksc.cav_subst import replace_subtree, replace_free_vars, make_nonfree_var
from ksc.expr import Expr, Call, If, Rule, Var, Let, Const
from ksc.rewrites import RuleMatcher, ParsedRuleMatcher, Match, Location, VariableSubstitution, parse_rule_str
from ksc.utils import singleton

class LiftOverCall(RuleMatcher):
    possible_filter_terms = frozenset([Call])
    @abstractmethod
    def can_lift_arg(self, arg: Expr) -> bool:
        """ Given <arg> used as argument to a Call, return true if some part of <arg> can be lifted above the Call.
            For example, if <arg> is (if p x_t x_f). """

    def matches_for_possible_expr(self, subtree: Expr, path_from_root: Location, root: Expr, env) -> Iterator[Match]:
        for i, arg in enumerate(subtree.args):
            if self.can_lift_arg(arg):
                yield Match(self, root, path_from_root + (i,))

    @abstractmethod
    def build_call_replacement(self, call_node: Call, which_arg: int) -> Expr:
        """ Build a new Expr which is equivalent in value to the call_node, but where argument
            <which_arg> is lifted. The caller ensures that argument satisfies can_lift_arg(). """

    @staticmethod
    def make_call_one_arg_different(call_node: Call, which_arg: int, new_arg: Expr):
        """ Helper method, provided as a utility for subclasses. Copies the specified call_node,
            but replacing one argument with the provided Expr. """
        args = call_node.args[:]  # copy
        args[which_arg] = new_arg
        return Call(call_node.name, args, type=call_node.type_)

    def apply_at(self, expr: Expr, path: Location) -> Expr:
        def apply_here(const_zero: Expr, call_node: Expr) -> Expr:
            assert const_zero == Const(0.0)
            assert isinstance(call_node, Call)
            return self.build_call_replacement(call_node, path[-1])
        # The constant just has no free variables that we want to avoid being captured
        return replace_subtree(expr, path[:-1], Const(0.0), apply_here)

@singleton
class lift_if_over_call(LiftOverCall):
    def can_lift_arg(self, arg: Expr) -> bool:
        return isinstance(arg, If)

    def build_call_replacement(self, call_node: Call, which_arg: int) -> Expr:
        if_node = call_node.args[which_arg]
        return If(if_node.cond,
            self.make_call_one_arg_different(call_node, which_arg, if_node.t_body),
            self.make_call_one_arg_different(call_node, which_arg, if_node.f_body))

def can_speculate_ahead_of_condition(e: Expr, cond: Expr, cond_value: bool) -> bool:
    """ Can we speculatively evaluate e, when the original program would not have done so,
        moving <e> to a point that it is evaluated before testing <cond> ?
        <cond_value> indicates the value that <cond> would have to take in order for <e> to have been evaluated. """
    # TODO: check if 'e' might raise an exception if evaluated without testing 'cond' first
    return True

# TODO: extend to allow template_vars with type : Any
lift_if_rules = [
    ParsedRuleMatcher(parse_rule_str(s, {})) for s in [
        '(rule "lift_if_over_let_rhs" ((p : Bool) (t : Any) (f : Any) (e : Any)) (let (x (if p t f)) e) (if p (let (x t) e) (let (x f) e)))',
        '(rule "lift_if_over_if_cond" ((p : Bool) (t : Bool) (f : Bool) (x : Any) (y : Any)) (if (if p t f) x y) (if p (if t x y) (if f x y)))',
        '(rule "lift_if_over_assert_cond" ((p : Bool) (t : Bool) (f : Bool) (body : Any)) (assert (if p t f) body) (if p (assert t body) (assert f body)))',
        '(rule "lift_if_over_assert_body" ((cond : Bool) (p : Bool) (t : Any) (f : Any)) (assert cond (if p t f)) (if p (assert cond t) (assert cond f)))',
    ]
] + [
    ParsedRuleMatcher(parse_rule_str(
        '(rule "lift_if_over_let_body" ((v : Any) (p : Bool) (t : Any) (f : Any)) (let (x v) (if p t f)) (if p (let (x v) t) (let (x v) f)))',
        {}), side_conditions=lambda *,x,v,p,t,f: x.name not in p.free_vars_
    ),
    ParsedRuleMatcher(parse_rule_str(
        '(rule "lift_if_over_if_true" ((p : Bool) (q : Bool) (t : Any) (f : Any) (e : Any)) (if p (if q t f) e) (if q (if p t e) (if p f e)))',
        {}), side_conditions=lambda *,p,q,t,f,e: can_speculate_ahead_of_condition(q, p, True)
    ),
    ParsedRuleMatcher(parse_rule_str(
        '(rule "lift_if_over_if_false" ((p : Bool) (q : Bool) (t : Any) (f : Any) (e : Any)) (if p e (if q t f)) (if q (if p e t) (if p e f)))',
        {}), side_conditions=lambda *,p,q,t,f,e: can_speculate_ahead_of_condition(q, p, False)
    ),
    ParsedRuleMatcher(parse_rule_str(
        """(rule "lift_if_over_build" ((n : Integer) (p : Bool) (t : Any) (f : Any))
                 (build n (lam (i : Integer) (if p t f)))
                 (if (gt n 0) (if p (build n (lam (i : Integer) t))
                                    (build n (lam (i : Integer) f)))
                              (build 0 (lam (i : Integer) (if p t f)))))""", {}),
            # TODO: replace "else" case with a constVec of size 0 dummy values of appropriate type.
            # In the meantime we prevent an infinite chain of applications producing if (gt 0 0).
            # TODO Should we have another version that avoids the outer "if" when can_speculate_ahead_of_condition(rhs, n > 0) is true?
            side_conditions=lambda *, i, x, n, body: (i.name not in rhs.free_vars_) and (n != Const(0.0))
    ),
    lift_if_over_call
]

def rename_to_avoid_capture(bound_var: Var, body: Expr, exprs_not_to_capture: Iterable[Expr]) -> Tuple[Var, Expr]:
    # Avoid capturing any identically-named (but different) variable in exprs_not_to_capture
    if any(bound_var.name in replacement_subexp.free_vars_ for replacement_subexp in exprs_not_to_capture):
        # Rename x in body
        nv = make_nonfree_var(bound_var.name, exprs_not_to_capture, type=bound_var.type_)
        return (nv, replace_free_vars(body, {bound_var.name: nv}))
    #  No renaming necessary
    return (bound_var, body)

@singleton
class lift_let_over_call(LiftOverCall):
    def can_lift_arg(self, arg: Expr) -> bool:
        return isinstance(arg, Let)

    def build_call_replacement(self, call_node: Call, which_arg: int) -> Expr:
        let_node = call_node.args[which_arg]
        bv, body = rename_to_avoid_capture(let_node.vars, let_node.body, [arg for i,arg in enumerate(call_node.args) if i != which_arg])
        return Let(bv, let_node.rhs, self.make_call_one_arg_different(call_node, which_arg, body))

class LetLifter(ParsedRuleMatcher):
    """ A ParsedRuleMatcher that deals with patterns contain (let (x rhs) body). If there is a match, and the rewrite is applied,
        then 'x' will be renamed (in body) to avoid capturing any free variables in any other part of the substitution """
    def __init__(self, rule: Rule, **kwargs):
        super().__init__(rule, **kwargs)
        assert frozenset(self._arg_types.keys()).issuperset(["rhs", "body"])

    def apply_at(self, expr: Expr, path: Location, **substs: VariableSubstitution) -> Expr:
        exprs_not_to_capture = [e for v, e in substs.items() if v not in ["x", "rhs", "body"]]
        x, body = rename_to_avoid_capture(substs["x"], substs["body"], exprs_not_to_capture)
        # The repeated ** here is so we override the entries in substs for x and body
        return super().apply_at(expr, path, **{**substs, "x": x, "body": body})

lift_let_rules = [
    LetLifter(parse_rule_str( # avoid x capturing in outer
        '(rule "lift_let_over_let_rhs" ((rhs : Any) (body : Any) (outer : Any)) (let (y (let (x rhs) body)) outer) (let (x rhs) (let (y body) outer)))', {})
    ),
    LetLifter(parse_rule_str( # avoid x capturing in val
        '(rule "lift_let_over_let_body" ((rhs : Any) (body : Any) (outer : Any)) (let (y outer) (let (x rhs) body)) (let (x rhs) (let (y outer) body)))', {}),
        side_conditions=lambda *, x, y, rhs, body, outer: y.name not in rhs.free_vars_
    ),
    LetLifter(parse_rule_str( # avoid x capturing in t, f
        '(rule "lift_let_over_if_cond" ((rhs : Any) (body : Bool) (t : Any) (f : Any)) (if (let (x rhs) body) t f) (let (x rhs) (if body t f)))', {})
    ),
    LetLifter(parse_rule_str( # avoid x capturing in p, f
        '(rule "lift_let_over_if_true" ((p : Bool) (rhs : Any) (body : Any) (f : Any)) (if p (let (x rhs) body) f) (let (x rhs) (if p body f)))', {}),
        side_conditions=lambda *, p, rhs, body, f: can_speculate_ahead_of_condition(rhs, p, True)
    ),
    LetLifter(parse_rule_str( # avoid x capturing in p, t
        '(rule "lift_let_over_if_false" ((p : Bool) (t : Any) (rhs : Any) (body : Any)) (if p t (let (x rhs) body)) (let (x rhs) (if p t body)))', {}),
        side_conditions=lambda *, p, rhs, body, f: can_speculate_ahead_of_condition(rhs, p, False)
    ),
    LetLifter(parse_rule_str( # avoid x capturing in val
        '(rule "lift_let_over_assert_cond" ((rhs : Any) (body : Bool) (val : Any)) (assert (let (x rhs) body) val) (let (x rhs) (assert body val)))', {}),
    ),
    LetLifter(parse_rule_str( # avoid x capturing in cond
        '(rule "lift_let_over_assert_body" ((cond : Bool) (rhs : Any) (body : Any)) (assert cond (let (x rhs) body)) (let (x rhs) (assert cond body)))', {}),
        #side_conditions=lambda *, cond, rhs, body: ok_to_evaluate(rhs, cond, False) # But we're gonna fail the assertion anyway, so OK?
    ),
    LetLifter(parse_rule_str( # avoid x capturing in n
        """(rule "lift_let_over_build" ((n : Integer) (rhs : Any) (body : Any))
                 (build n (lam (i : Integer) (let (x rhs) body)))
                 (if (gt n 0) (let (x rhs) (build n (lam (i : Integer) body))) (build 0 (lam (i : Integer) (let (x rhs) body)))))""", {}),
            # TODO: replace "else" case with a constVec of size 0 dummy values of appropriate type.
            # In the meantime we prevent an infinite chain of applications producing if (gt 0 0).
            # TODO Should we have another version that avoids the "if" when can_speculate_ahead_of_condition(rhs, n > 0) is true?
            side_conditions=lambda *, i, x, n, rhs, body: (i.name not in rhs.free_vars_) and (n != Const(0.0))
    ),
    lift_let_over_call
]
