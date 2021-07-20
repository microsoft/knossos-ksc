from abc import ABC, abstractmethod
from typing import Iterable, Iterator, Tuple, Type
import functools

from ksc.cav_subst import replace_subtree, replace_free_vars, make_nonfree_var
from ksc.expr import Expr, Call, If, Rule, Var, Let, Const
from ksc.path import ExprWithPath, PathElement
from ksc.rewrites import (
    RuleMatcher,
    ParsedRuleMatcher,
    Match,
    VariableSubstitution,
    parse_rule_str,
)
from ksc.utils import singleton, single_elem

###############################################################################
# Lifting rules
#   e.g. (foo a (if p x y) b) ==> (if p (foo a x b) (foo a y b))
#   e.g. (foo (let (x e) body)) ==> (let (x e) (foo body))


class LiftOverCall(RuleMatcher, ABC):
    """ Superclass for lifting if's or let's (or potentially other constructs) that are
        used as arguments to Calls. Such rules cannot be expressed in ks syntax as there
        is no syntax to match an arbitrary function, or an arbitrary (number of) argument(s).
    """

    may_match_any_call = True
    possible_filter_terms = frozenset()

    liftable_arg_type: Type[Expr]
    """ Must be defined by concrete instances as the subclass of Expr which, as argument to a Call,
        the instance of LiftOverCall can lift to above the Call. For example, 'If' or 'Let'. """

    def matches_for_possible_expr(self, ewp: ExprWithPath, env) -> Iterator[Match]:
        def apply(arg_with_path: ExprWithPath):
            return replace_subtree(
                arg_with_path.root,
                arg_with_path.path[:-1],
                Const(0.0),
                lambda _zero, call_node: self.build_call_replacement(
                    call_node, arg_with_path.path[-1]
                ),
            )

        for arg_with_path in ewp.args:
            if isinstance(arg_with_path.expr, self.liftable_arg_type):
                yield Match(
                    self, functools.partial(apply, arg_with_path), arg_with_path
                )

    @abstractmethod
    def build_call_replacement(self, call_node: Call, which_arg: PathElement) -> Expr:
        """ Build a new Expr which is equivalent to the call_node, but where argument <which_arg>
            is lifted. The caller guarantees that argument is of class <self.liftable_arg_type>. """


###########
# Lifting "if"


@singleton
class lift_if_over_call(LiftOverCall):
    liftable_arg_type = If

    def build_call_replacement(self, call_node: Call, which_arg: PathElement) -> Expr:
        if_node = which_arg.get(call_node)
        return If(
            if_node.cond,
            replace_subtree(call_node, (which_arg,), if_node.t_body),
            replace_subtree(call_node, (which_arg,), if_node.f_body),
        )


def can_evaluate_without_condition(e: Expr, cond: Expr, cond_value: bool) -> bool:
    """ Given an input program that evaluated <e> only when <cond> evaluated to <cond_value>,
        tells whether we can output a program that evaluates <e> when <cond> either
            * evaluates to the opposite of <cond_value>,
            * raises an exception itself ? """
    # TODO: we can return True here if we are sure e cannot raise an exception.
    # For now we'll use almost the simplest test possible ("return False" would be simpler).
    return isinstance(e, (Var, Const))


lift_if_rules = (
    [
        parse_rule_str(s, {})
        for s in [
            """(rule "lift_if_over_let_rhs" ((p : Bool) (t : Any) (f : Any) (e : Any))
                 (let (x (if p t f)) e)
                 (if p (let (x t) e) (let (x f) e)))""",
            """(rule "lift_if_over_if_cond" ((p : Bool) (t : Bool) (f : Bool) (x : Any) (y : Any))
                 (if (if p t f) x y)
                 (if p (if t x y) (if f x y)))""",
            """(rule "lift_if_over_assert_cond" ((p : Bool) (t : Bool) (f : Bool) (body : Any))
                 (assert (if p t f) body)
                 (if p (assert t body) (assert f body)))""",
        ]
    ]
    + [
        parse_rule_str(
            """(rule "lift_if_over_assert_body" ((cond : Bool) (p : Bool) (t : Any) (f : Any))
                 (assert cond (if p t f))
                 (if p (assert cond t) (assert cond f)))""",
            {},
            side_conditions=lambda *, cond, p, t, f: can_evaluate_without_condition(
                p, cond, True
            ),
        ),
        parse_rule_str(
            """(rule "lift_if_over_let_body" ((v : Any) (p : Bool) (t : Any) (f : Any))
                 (let (x v) (if p t f))
                 (if p (let (x v) t) (let (x v) f)))""",
            {},
            side_conditions=lambda *, x, v, p, t, f: x.name not in p.free_vars_,
        ),
        parse_rule_str(
            """(rule "lift_if_over_if_true" ((p : Bool) (q : Bool) (t : Any) (f : Any) (e : Any))
                 (if p (if q t f) e)
                 (if q (if p t e) (if p f e)))""",
            {},
            side_conditions=lambda *, p, q, t, f, e: can_evaluate_without_condition(
                q, p, True
            ),
        ),
        parse_rule_str(
            """(rule "lift_if_over_if_false" ((p : Bool) (q : Bool) (t : Any) (f : Any) (e : Any))
                     (if p e (if q t f))
                     (if q (if p e t) (if p e f)))""",
            {},
            side_conditions=lambda *, p, q, t, f, e: can_evaluate_without_condition(
                q, p, False
            ),
        ),
        parse_rule_str(
            """(rule "lift_if_over_build" ((n : Integer) (p : Bool) (t : Any) (f : Any))
                 (build n (lam (i : Integer) (if p t f)))
                 (if (gt n 0) (if p (build n (lam (i : Integer) t))
                                    (build n (lam (i : Integer) f)))
                              (build 0 (lam (i : Integer) (if p t f)))))""",
            {},
            # TODO: replace the final "else" case with a constVec of size 0 dummy values of appropriate type.
            # TODO Should we have another version that avoids the outer "if" when can_evaluate_ahead_of_condition(rhs, n > 0) is true?
            side_conditions=lambda *, i, n, p, t, f: (
                i.name not in p.free_vars_
                and
                # In the absence of constVec 0 this avoids an infinite chain of rewrites each adding an (if (gt 0 0) ...).
                (n != Const(0))
            ),
        ),
        lift_if_over_call,
    ]
)

###########
# Lifting "let"
#
# Here, in the absence of All Binders Unique, we must do extra renaming to avoid capture. For example,
# "(foo2 (let (x e) body) x)" must not be lifted straightforwardly to "(let (x e) (foo2 body x))" as the second argument to foo2 is captured.
# Instead we must rename 'x' within 'body' to give something like "(let (x2 e) (foo2 body[x=>x2] x))"


def rename_to_avoid_capture(
    bound_var: Var, body: Expr, exprs_not_to_capture: Iterable[Expr]
) -> Tuple[Var, Expr]:
    # Rename bound_var in body to avoid capturing any identically-named
    # (but different) variable in exprs_not_to_capture.
    if any(
        bound_var.name in replacement_subexp.free_vars_
        for replacement_subexp in exprs_not_to_capture
    ):
        # Rename x in body
        nv = make_nonfree_var(
            bound_var.name, exprs_not_to_capture, type=bound_var.type_
        )
        return (nv, replace_free_vars(body, {bound_var.name: nv}))
    #  No renaming necessary
    return (bound_var, body)


@singleton
class lift_let_over_call(LiftOverCall):
    liftable_arg_type = Let

    def build_call_replacement(self, call_node: Call, which_arg: PathElement) -> Expr:
        let_node = which_arg.get(call_node)
        bv, body = rename_to_avoid_capture(
            let_node.vars,
            let_node.body,
            [
                arg.expr
                for arg in ExprWithPath.from_expr(call_node).args
                if single_elem(arg.path) != which_arg
            ],
        )
        return Let(bv, let_node.rhs, replace_subtree(call_node, (which_arg,), body))


class ParsedLetLifter(ParsedRuleMatcher):
    """ Performs the extra renaming-to-avoid-capture, for a rule parsed from KS of the form:
          template: .... (let (x rhs) body) ....
          replacement: (let (x rhs) .... body ....)
        the variable named 'x' is then renamed (within body) to avoid capturing any free variables in the replacement """

    def __init__(self, rule: Rule, **kwargs):
        super().__init__(rule, **kwargs)
        assert "rhs" in self._arg_types.keys()
        assert "body" in self._arg_types.keys()

    def _apply_at(self, ewp: ExprWithPath, **substs: VariableSubstitution) -> Expr:
        exprs_not_to_capture = [
            e for v, e in substs.items() if v not in ["x", "rhs", "body"]
        ]
        x, body = rename_to_avoid_capture(
            substs["x"], substs["body"], exprs_not_to_capture
        )
        # The repeated ** here is so we override the entries in substs for x and body
        return super()._apply_at(ewp, **{**substs, "x": x, "body": body})


lift_let_rules = [
    parse_rule_str(
        """(rule "lift_let_over_let_rhs" ((rhs : Any) (body : Any) (outer : Any))
                 (let (y (let (x rhs) body)) outer)
                 (let (x rhs) (let (y body) outer)))""",
        {},
        ParsedLetLifter,  # avoid x capturing in outer (or y)
    ),
    parse_rule_str(
        """(rule "lift_let_over_let_body" ((rhs : Any) (body : Any) (outer : Any))
                 (let (y outer) (let (x rhs) body))
                 (let (x rhs) (let (y outer) body)))""",
        {},
        ParsedLetLifter,  # avoid x capturing in outer (or y)
        side_conditions=lambda *, x, y, rhs, body, outer: y.name not in rhs.free_vars_,
    ),
    parse_rule_str(
        """(rule "lift_let_over_if_cond" ((rhs : Any) (body : Bool) (t : Any) (f : Any))
                 (if (let (x rhs) body) t f)
                 (let (x rhs) (if body t f)))""",
        {},
        ParsedLetLifter,  # avoid x capturing in t, f
    ),
    parse_rule_str(
        """(rule "lift_let_over_if_true" ((p : Bool) (rhs : Any) (body : Any) (f : Any))
                 (if p (let (x rhs) body) f)
                 (let (x rhs) (if p body f)))""",
        {},
        ParsedLetLifter,  # avoid x capturing in p, f
        side_conditions=lambda *, x, p, rhs, body, f: can_evaluate_without_condition(
            rhs, p, True
        ),
    ),
    parse_rule_str(
        """(rule "lift_let_over_if_false" ((p : Bool) (t : Any) (rhs : Any) (body : Any))
                 (if p t (let (x rhs) body))
                 (let (x rhs) (if p t body)))""",
        {},
        ParsedLetLifter,  # avoid x capturing in p, t
        side_conditions=lambda *, x, p, t, rhs, body: can_evaluate_without_condition(
            rhs, p, False
        ),
    ),
    # Additional variants for lifting over "if" have no side conditions because they work
    # only on cases where the rhs already executes regardless of the if-condition p.
    parse_rule_str(
        """(rule "lift_let_over_if_both" ((p : Bool) (rhs : Any) (body : Any) (f : Any))
                 (if p (let (x rhs) body) (let (y rhs) f))
                 (let (x rhs) (if p body (let (y x) f))))""",
        {},
        ParsedLetLifter,  # avoid x capturing in p or f (as required), or in y (which would be
        # harmless, there is no need to avoid, but it happens as a side effect of ParsedLetLifter)
    ),
    parse_rule_str(
        """(rule "lift_let_over_if_true_dominated" ((p : Bool) (rhs : Any) (body : Any) (f : Any))
                 (let (x rhs) (if p (let (y rhs) body) f))
                 (let (x rhs) (if p (let (y x) body) f)))""",
        {},
    ),
    parse_rule_str(
        # Ideally we'd like a rule that detects (let (x rhs) ....) anywhere which *must* be executed
        # in order to execute the "if". So to handle the general case, additional rules will be needed
        # (beyond those here) to push the outer let into place.
        """(rule "lift_let_over_if_false_dominated" ((p : Bool) (rhs : Any) (body : Any) (t : Any))
                 (let (x rhs) (if p t (let (y rhs) body)))
                 (let (x rhs) (if p t (let (y x) body))))""",
        {},
    ),
    parse_rule_str(
        """(rule "lift_let_over_assert_cond" ((rhs : Any) (body : Bool) (val : Any))
                 (assert (let (x rhs) body) val)
                 (let (x rhs) (assert body val)))""",
        {},
        ParsedLetLifter,  # avoid x capturing in val
    ),
    parse_rule_str(
        """(rule "lift_let_over_assert_body" ((cond : Bool) (rhs : Any) (body : Any))
                 (assert cond (let (x rhs) body))
                 (let (x rhs) (assert cond body)))""",
        {},
        ParsedLetLifter,  # avoid x capturing in cond
        side_conditions=lambda *, cond, rhs, body: can_evaluate_without_condition(
            rhs, cond, True
        ),  # But we're gonna fail the assertion anyway, so OK?
    ),
    parse_rule_str(
        """(rule "lift_let_over_build" ((n : Integer) (rhs : Any) (body : Any))
                 (build n (lam (i : Integer) (let (x rhs) body)))
                 (if (gt n 0) (let (x rhs) (build n (lam (i : Integer) body))) (build 0 (lam (i : Integer) (let (x rhs) body)))))""",
        {},
        ParsedLetLifter,  # avoid x capturing in n
        # TODO: replace "else" case with a constVec of size 0 dummy values of appropriate type.
        # TODO Should we have another version that avoids the "if" when can_evaluate_without_condition(rhs, n > 0) is true?
        side_conditions=lambda *, i, x, n, rhs, body: (i.name not in rhs.free_vars_) and
        # In the absence of constVec 0, this avoids infinite chain of rewrites producing if (gt 0 0).
        (n != Const(0.0)),
    ),
    lift_let_over_call,
]
