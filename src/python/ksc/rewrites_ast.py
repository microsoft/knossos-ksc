from abc import abstractmethod
from sys import path
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
#
# lift_if: EXPR{If} -> If{EXPR}
#

#  TODO: consistently handle Lambda, as below,
#   or document all of the above cases, with nested lambdas
#
#  Lam:
#     (lam  (if p x y)) -> (if p (lam V x) (lam V y))
#                             -? v not in freevars(p)
#  Which then implies build will take two rewrites to lift "if":
#     (build e1 (lam V (if p x y)))
#     -> (build e1 (if p (lam V x) (lam V y)))
#     -> (if p (build e1 (lam V x))
#              (build e1 (lam V y)))
#
#  And we should undo today's special casing, which achieves this:
#     (build e1 (lam V (if p x y))) -> (if p (build e1 (lam V x))
#                                            (build e1 lam V y)))
#                                   -? not v in freevars(p)
#
# Alternative: documenting nested_lam behaviour
#
# If:
#     (if (lam V (if p x y)) a b) -> # Won't typecheck
#     (if q (lam V (if p x y)) b) -> (if p (if q (lam V x) b)
#                                          (if q (lam V y) b))
#                                 -? Unless q guards p
# Let:
#     (let (f (lam V (if p x y))) body) -> (if p (let (f (lam V x)) body)
#                                                (let (f (lam V y)) body))
#     (let (f a) (lam V (if p x y)))) -> (if p (lam V (let (f a) x))
#                                              (lam V (let (f a) y)))


# Note: guards
# In this context, "cond" guards "e" if evaluating "e" might fail if cond is != cond_value
def can_evaluate_ahead_of_condition(e: Expr, cond: Expr, cond_value: bool) -> bool:
    """ Can we evaluate e, when the original program would not have done so,
        moving <e> to a point that it is evaluated before testing <cond> == <cond_value> ?
    """
    # TODO: check if 'e' might raise an exception if evaluated without testing 'cond' first
    # TODO: check for index - the only real exception-raiser we have
    return True


@singleton
class lift_if(RuleMatcher):
    possible_filter_terms = frozenset([Call, If, Let, Assert])
    # TOUNDO: Not Lam: lams inside build/sumbuild are handled as part of the (sum)build

    def matches_for_possible_expr(
        self,
        expr: Expr,  # (foo a1 a2 (if p x y) a4 a5 (if q z w) a7)
        path_from_root: Location,
        root: Expr,
        env: LetBindingEnvironment,
    ) -> Iterator[Match]:  # ((if p ...), path + [3]); ((if q ...), path + [7]))
        # For any expr: (foo e1 e2 (if cond t f) e4 e5)
        # We check for children that are "if", and lift them if they don't depend
        # on vars brought into scope by e0

        def check_child(ch, i, vars_in_scope=set()):
            if isinstance(ch, If) and vars_in_scope.isdisjoint(ch.cond.free_vars_):
                yield Match(self, root, path=path_from_root + (i,))

        # Lam:
        if isinstance(expr, Lam):
            # (lam (v : T) (if p x y)) -> (if p (lam (v : T) x)
            #                                   (lam (v : T) y))
            #                          -? v not in freevars{p}
            #                          -? p won't sideeffect
            raise NotImplementedError()

        # Let:
        elif isinstance(expr, Let):
            # (let (v (if p x y)) body) -> (if p (let (v x) body)
            #                                    (let (v y) body))
            #                           -? v not in freevars{p}
            yield from check_child(expr.rhs, 0)

            # (let (v a) (if p x y)) -> (if p (let (v a) x)
            #                                 (let (v a) y))
            #                        -? v not in freevars(p)
            yield from check_child(expr.body, 1, {expr.vars.name})

        #  Call:
        elif isinstance(expr, Call):
            #     (foo a1 a2 (if p x y) a4) -> (if p (foo a1 a2 x a4)
            #                                        (foo a1 a2 y a4))
            for i, ch in enumerate(expr.args):
                yield from check_child(ch, i)

        #  If:
        elif isinstance(expr, If):
            # (if (if p x y) a b) -> (if p (if x a b)
            #                              (if y a b))
            yield from check_child(expr.cond, 0)

            # (if q (if p x y) b) -> (if p (if q x b)
            #                              (if q y b))
            #                     -? Unless q guards p
            if can_evaluate_ahead_of_condition(expr.t_body, expr.cond, True):
                yield from check_child(expr.t_body, 1)

            # (if q a (if p x y)) -> (if p (if q a x)
            #                              (if q a y))
            #                     -? Unless q guards p
            if can_evaluate_ahead_of_condition(expr.f_body, expr.cond, False):
                yield from check_child(expr.t_body, 2)

        #  Assert:
        elif isinstance(expr, Assert):
            # (assert cond (if p x y)) -> (if p (assert cond x)
            #                                   (assert cond y))
            #                          -? Unless cond guards p;
            # We might argue that the only consequence can be an assert fail either way,
            # but we do want "index" to be able to run unchecked;
            # which might induce segfault instead of assert failure.
            if can_evaluate_ahead_of_condition(expr.body, expr.cond, True):
                yield from check_child(expr.cond, 0)

            # (assert (if p x y) body) -> (if p (assert x body)
            #                                   (assert y body))
            yield from check_child(expr.body, 1)

        else:
            assert False  # Should not have got here, check possible_filter_terms

    def apply_at(self, e: Expr, path: Location) -> Expr:
        rest_of_path = path[-1:]
        path_to_parent = path[:-1]
        return replace_subtree(
            e,
            path_to_parent,
            Const(0.0),
            lambda _, parent: self.apply_to_parent(parent, rest_of_path),
        )

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


###############################################################################
#
# lift_bind: Expr{Let} -> Let{Expr}
#


@singleton
class lift_bind(RuleMatcher):
    possible_filter_terms = frozenset([Call, If, Let, Assert])

    def matches_for_possible_expr(
        self,
        expr: Expr,
        path_from_root: Location,
        root: Expr,
        env: LetBindingEnvironment,
    ) -> Iterator[Match]:

        # For any expr: (foo e1 e2 (LET (x rhs) body) e4 e5)
        # We check for children that are "let", and lift them if they don't depend
        # on vars brought into scope by expr itself (when foo == "let")

        def check_child(ch, i, vars_in_scope=set()):
            if isinstance(ch, Let) and vars_in_scope.isdisjoint(ch.rhs.free_vars_):
                yield Match(self, root, path=path_from_root + (i,))

        # Let:
        if isinstance(expr, Let):
            # (let (p (LET (x rhs) y))) body) -> (let (x rhs) (let (p y) body))
            #         ^^expr.rhs^^            -? p not in freevars{rhs}
            yield from check_child(expr.rhs, 0)

            # (let (v a) (LET (x rhs) body)) -> (let (x rhs) (let (v a) body)
            #            ^^^^expr.body^^^^^  -? v not in freevars(rhs)
            yield from check_child(expr.body, 1, {expr.vars.name})

        #  Call:
        elif isinstance(expr, Call):
            # (foo a1 a2 (LET (x rhs) body) a4) -> (let (x rhs) (foo a1 a2 body a4))
            #            ^^^expr.args[i]^^^     -? x not in freevars{a1,a2,a4}
            # Note: not equivalent to x not in freevars{expr} as x can be in rhs
            for i, ch in enumerate(expr.args):
                yield from check_child(ch, i)

        #  If:
        elif isinstance(expr, If):
            # (if (LET (v e) p) x y) -> (LET (v e) (if p x y))
            #     ^^expr.cond^^      -? v not in freevars{x,y}
            #                         which is equivalent to v not in freevars{expr}?
            yield from check_child(expr.cond, 0)

            #      (if p (LET (v e) x) y) -> (LET (v e) (if p x y))
            #                             -? v not in freevars{p} u freevars{y}
            #                             -? e not guarded by p
            if can_evaluate_ahead_of_condition(expr.t_body, expr.cond, True):
                yield from check_child(expr.t_body, 1)

            #      (if p x (LET (v e) y)) -> (LET (v e) (if p x y))
            #                             -? v not in freevars{p} u freevars{y}
            #                             -? e not guarded by (p==False)
            if can_evaluate_ahead_of_condition(expr.f_body, expr.cond, False):
                yield from check_child(expr.t_body, 2)

        #  Assert:
        elif isinstance(expr, Assert):
            #      (assert (LET (x e1) body) y) -> (LET (x e1) (assert body y))
            #                                   -? x not in freevars{y}
            yield from check_child(expr.body, 1)

            # (assert cond (LET (x e1) body)) -> (LET (x e1) (assert cond body))
            #                                   -? x not in freevars{cond}
            if can_evaluate_ahead_of_condition(expr.body, expr.cond, True):
                yield from check_child(expr.cond, 0)

        # Lam:
        elif isinstance(expr, Lam):
            # (lam (v : T) (let (x rhs) body)) -> (let (x rhs) (lam (v : T) body))
            #              ^^^^^^^^^^^^^^^^^^  -? v not in freevars{p}
            raise NotImplementedError()

        else:
            assert False  # Should not have got here, check possible_filter_terms

    def apply_at(self, e: Expr, path: Location) -> Expr:
        rest_of_path = path[-1:]
        path_to_parent = path[:-1]
        return replace_subtree(
            e,
            path_to_parent,
            Const(0.0),
            lambda _, parent: self.apply_to_parent(parent, rest_of_path),
        )

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
