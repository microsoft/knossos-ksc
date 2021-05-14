from abc import abstractmethod
from typing import Iterator, Optional

from ksc.expr import Expr, Call, Lam, If, Let, Const, Var, Assert
from ksc.cav_subst import (
    Location,
    subexps_no_binds,
    get_node_at_location,
    make_nonfree_var,
    replace_subtree,
    replace_free_vars,
)
from ksc.rewrites import RuleMatcher, LetBindingEnvironment, Match
from ksc.utils import singleton

###############################################################################
# Lifting rules:
#   lift_bind:
#      (foo a1 a2 (LET (x e1) y) a4) ==> (LET (x e1) (foo a1 a2 y a4))
#      (let (p q) (LET (x e1) y)) ==> (LET (x e1) (let (p q) y))
#   lift_if: (foo a1 a2 (if p x y) a4) ==> (if p (foo a1 a2 x a4) (foo a1 a2 y a4))
# where foo can be any variety of Expr.
#     (if (if p x y) )


def can_speculate_ahead_of_condition(e: Expr, cond: Expr, cond_value: bool) -> bool:
    """ Can we speculatively evaluate e, when the original program would not have done so,
        moving <e> to a point that it is evaluated before testing <cond> ?
        <cond_value> indicates the value that <cond> would have to take in order for <e> to have been evaluated. """
    # TODO: check if 'e' might raise an exception if evaluated without testing 'cond' first
    return True


class LiftingRule(RuleMatcher):
    possible_filter_terms = frozenset([Call, If, Let, Assert])

    def matches_for_possible_expr(
        self,
        subtree: Expr,
        path_from_root: Location,
        root: Expr,
        env: LetBindingEnvironment,
    ) -> Iterator[Match]:
        if isinstance(subtree, Lam):
            # Lam's inside build/sumbuild are handled as part of the (sum)build
            return
        children = subexps_no_binds(subtree)
        for i, ch in enumerate(children):
            nested_lam = isinstance(ch, Lam)
            e = ch.body if nested_lam else ch

            to_lift = None
            if isinstance(self, lift_if.__class__) and isinstance(e, If):
                to_lift = e.cond
            if isinstance(self, lift_bind.__class__) and isinstance(e, Let):
                to_lift = e.rhs

            if to_lift is None:
                continue

            if (
                isinstance(subtree, Let)
                and i == 1
                and subtree.vars.name in to_lift.free_vars_
            ):
                pass  # Cannot lift computation using a variable outside of let that binds that variable
            elif nested_lam and ch.arg.name in to_lift.free_vars_:
                pass  # Similarly - cannot lift loop-variant computation out of lam within (sum)build
            elif (
                isinstance(subtree, If)
                and i > 0
                and not can_speculate_ahead_of_condition(to_lift, subtree.cond, i == 1)
            ):
                pass  # Don't lift computation out of "if" that may be guarding against an exception
            else:
                yield Match(
                    self,
                    root,
                    path_from_root + ((i, 0) if nested_lam else (i,)),
                    {"buildlam": nested_lam},
                )

    def apply_at(self, e: Expr, path: Location, buildlam: bool) -> Expr:
        assert (not buildlam) or len(path) > 1
        rest_of_path = path[-(1 + int(buildlam)) :]
        path_to_parent = path[: -len(rest_of_path)]
        return replace_subtree(
            e,
            path_to_parent,
            Const(0.0),
            lambda _, parent: self.apply_to_parent(parent, rest_of_path),
        )

    @abstractmethod
    def apply_to_parent(self, parent: Expr, path_to_child: Location) -> Expr:
        """ Args -
            <parent>: the node to lift over
            <path_to_child> path within parent identifying the liftable child (maybe a grandchild to lift over (sum)build+lam).
        Return an Expr equivalent to <parent>. """


@singleton
class lift_if(LiftingRule):
    def apply_to_parent(self, parent: Expr, path_to_child: Location) -> Expr:
        if_node = get_node_at_location(parent, path_to_child)
        return If(
            if_node.cond,
            replace_subtree(
                parent, path_to_child, Const(0.0), lambda *_: if_node.t_body
            ),
            replace_subtree(
                parent, path_to_child, Const(0.0), lambda *_: if_node.f_body
            ),
        )


@singleton
class lift_bind(LiftingRule):
    def apply_to_parent(self, parent: Expr, path_to_child: Location) -> Expr:
        let_node = get_node_at_location(parent, path_to_child)
        assert isinstance(
            let_node.vars, Var
        ), "Tupled lets not supported - use untuple_lets first"
        bound_var, let_body = let_node.vars, let_node.body
        let_siblings = [
            sibling
            for i, sibling in enumerate(subexps_no_binds(parent))
            if i != path_to_child[0]
        ]
        if any(bound_var.name in sibling.free_vars_ for sibling in let_siblings):
            # Occurrences of the bound variable in the let_node's body are not free.
            # So there must be other occurrences of the same name (referring to an outer variable).
            # These would be captured if we lifted the binder above the parent.
            # Thus, rename.
            new_var = make_nonfree_var(bound_var.name, [parent], type=bound_var.type_)
            let_body = replace_free_vars(let_node.body, {bound_var.name: new_var})
            bound_var = new_var
        return Let(
            bound_var,
            let_node.rhs,
            replace_subtree(parent, path_to_child, Const(0.0), lambda *_: let_body),
        )
