from typing import Union
from dataclasses import dataclass
from functools import singledispatch
import itertools
from typing import (
    Callable,
    List,
    Mapping,
    Optional,
    Tuple,
    Sequence,
)

from ksc.expr import Expr, If, Call, Let, Lam, Var, Const, Assert
from ksc.path import Path, ExprWithPath
from ksc.visitors import ExprTransformer
from ksc.utils import singleton

# cav_subst = Capture-AVoiding SUBSTitution

#####################################################################
# Public members


@dataclass(frozen=True)
class ReplaceLocationRequest:
    """ Describes a subtree to be replaced by the replace_subtree(s) function. """

    target: Path
    """ The location within the root Expr, i.e. upon which replace_subtree(s) is called, that should be replaced. """

    payload: Expr
    """ An Expr to be "delivered" to that index, i.e. with the same value as it would have at the top.
        That is, any binders on the path from the root to <target> that bind variables free in <payload>,
        will be alpha-renamed so that <payload> sees the same values in those variables that it would have at the root. """

    applicator: Optional[Callable[[Expr, Expr], Expr]] = None
    """ Details how the <payload> should be inserted into the specified <target>.
        * If applicator is None, then replace_subtree should merely replace the node at <target> with <payload>.
        * Otherwise, the replacement (new) subtree is given by `applicator(payload, x)` where x is
            the node currently at that location,
            but *after* any alpha-renaming necessary to preserve the meaning of <payload>. """


# Type of variable substitutions. This reflects Var.name being a 'str', as only bound Variable's need renaming,
# Call.name targets do not unless they are Var.name's).
VariableSubstitution = Mapping[str, Expr]


def replace_subtrees(e: Expr, reqs: List[ReplaceLocationRequest]) -> Expr:
    """ Replaces locations within <e> with new subtrees, as per ReplaceLocationRequest.
        Returns a new Expr, which may share subtrees with the original. """
    return CAvSubst.visit(ExprWithPath.from_expr(e), reqs, {})


def replace_subtree(e: Expr, *args):
    """ Replaces a single location within e. The additional arguments are as per the fields of ReplaceLocationRequest. """
    return replace_subtrees(e, [ReplaceLocationRequest(*args)])


def replace_free_vars(e: Expr, subst: VariableSubstitution) -> Expr:
    """ Replaces all occurrences of some free-variables in e with new subtrees.
        <subst> details the mapping from the free variables to replace, with the new subtree for each.
        Renames as necessary any binders within e to avoid capturing any variables in the values of <subst>.
        Returns a new Expr, which may share subtrees with the original (as well as with values of <subst>).
        """
    return CAvSubst.visit(ExprWithPath.from_expr(e), [], subst)


#####################################################################
# Name Generation


def make_nonfree_var(prefix, exprs, type=None):
    for idx in itertools.count():
        name = prefix + "_" + str(idx)
        if all(name not in e.free_vars_ for e in exprs):
            return Var(name, type)


#####################################################################
# CAV implementation


@singleton
class CAvSubst(ExprTransformer):
    def visit(
        self,
        ewp: ExprWithPath,
        reqs: List[ReplaceLocationRequest],
        substs: VariableSubstitution,
    ) -> Expr:
        # First, work out if there's anything to do in this subtree, i.e. if
        # intersection(substs, ewp.free_vars)>0 or any `reqs` are within this subtree
        substs = {
            varname: repl
            for varname, repl in substs.items()
            if varname in ewp.free_vars_
        }
        reqs = [req for req in reqs if req.target[: len(ewp.path)] == ewp.path]
        if len(reqs) == 0:
            if len(substs) == 0:
                # Nothing to do in this subtree
                return ewp.expr
            # No paths identified to replace, but we are still propagating a rename: carry on
        elif any(req.target == ewp.path for req in reqs):
            # Apply here
            if len(reqs) != 1:
                raise ValueError(
                    "Multiple ReplaceLocationRequests on locations nested within each other"
                )
            if reqs[0].applicator is None:
                return reqs[0].payload
            # Continue renaming.
            renamed_e = self.visit(ewp, [], substs)
            return reqs[0].applicator(reqs[0].payload, renamed_e)
        # No ReplaceLocationRequest targets this node. Do any Expr-specific processing, perhaps recursing deeper.
        return super().visit(ewp, reqs, substs)

    def visit_var(self, e: Union[ExprWithPath, Var], reqs, substs):
        return substs.get(e.name, e)

    @staticmethod
    def _rename_if_needed(
        arg: Var,
        binder: Expr,
        reqs: List[ReplaceLocationRequest],
        subst: VariableSubstitution,
    ) -> Tuple[Var, VariableSubstitution]:
        # If <arg> binds a variable free in any Expr's in a request payload or the RHS of a substitution, then
        # returns a new Var (chosen not to capture any variable free in <binder> or as above), and an updated <subst>.
        # Otherwise, returns <arg> and <subst> but *removing* any mapping for <arg>.
        conflicting_binders = [req.payload for req in reqs] + list(subst.values())
        if any(arg.name in rhs.free_vars_ for rhs in conflicting_binders):
            # Must rename "arg". Make a new name.
            nv = make_nonfree_var(arg.name, [binder] + conflicting_binders)
            nv.type_ = arg.type_
            return nv, {**subst, arg.name: nv}
        return arg, {k: v for k, v in subst.items() if k != arg.name}

    def visit_lam(self, e: Union[Lam, ExprWithPath], reqs, substs):
        assert isinstance(e, ExprWithPath) and isinstance(
            e.expr, Lam
        )  # ExprTransformer ensures.
        arg, substs = self._rename_if_needed(e.arg, e.expr, reqs, substs)
        return Lam(
            Var(arg.name, type=arg.type_, decl=True),
            self.visit(e.body, reqs, substs),
            type=e.type_,
        )

    def visit_let(self, e: Union[Let, ExprWithPath], reqs, substs):
        assert isinstance(e, ExprWithPath) and isinstance(
            e.expr, Let
        )  # ExprTransformer ensures.
        new_vars = []
        # alpha-rename any capturing `e.vars` in `e.body` only
        body_substs = substs
        for var in [e.vars] if isinstance(e.vars, Var) else e.vars:
            new_var, body_substs = self._rename_if_needed(var, e, reqs, body_substs)
            new_vars.append(new_var)
        if len(new_vars) == 1:
            assert isinstance(e.vars, Var)
            new_vars = new_vars[0]
        else:
            assert isinstance(e.vars, list)
        # Do not use super().visit: we need different substitutions for each child.
        return Let(
            new_vars,
            self.visit(e.rhs, reqs, substs),
            self.visit(e.body, reqs, body_substs),
            type=e.type_,
        )
