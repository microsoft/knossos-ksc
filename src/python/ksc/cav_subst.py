from dataclasses import dataclass
from functools import singledispatch
import itertools
from typing import Callable, Iterable, Generator, List, Mapping, Optional, Tuple, Sequence

from ksc.expr import Expr, If, Call, Let, Lam, Var, Const, Assert

# cav_subst = Capture-AVoiding SUBSTitution

##### Node numbering scheme


@singledispatch
def get_children(e: Expr) -> List[Expr]:
    # The rewritable children of an Expr
    raise ValueError("Must be overridden for every Expr subclass")


@get_children.register
def _get_children_var(e: Var):
    return []


@get_children.register
def _get_children_const(e: Const):
    return []


@get_children.register
def _get_children_let(e: Let):
    return [e.rhs, e.body]


@get_children.register
def _get_children_lam(e: Lam):
    return [e.body]


@get_children.register
def _get_children_call(e: Call):
    return e.args


@get_children.register
def _get_children_if(e: If):
    return [e.cond, e.t_body, e.f_body]


@get_children.register
def _get_children_assert(e: Assert):
    return [e.cond, e.body]


Location = Sequence[int]  # Used immutably, so normally a tuple


def get_node_at_location(expr, path: Location):
    return expr if len(path) == 0 else get_node_at_location(get_children(expr)[path[0]], path[1:])


#####################################################################
# Public members


@dataclass(frozen=True)
class ReplaceLocationRequest:
    """ Describes a subtree to be replaced by the replace_subtree(s) function. """

    target: Location
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
    return _cav_helper(e, reqs, {})


def replace_subtree(e: Expr, *args):
    """ Replaces a single location within e. The additional arguments are as per the fields of ReplaceLocationRequest. """
    return replace_subtrees(e, [ReplaceLocationRequest(*args)])


def replace_free_vars(e: Expr, subst: VariableSubstitution) -> Expr:
    """ Replaces all occurrences of some free-variables in e with new subtrees.
        <subst> details the mapping from the free variables to replace, with the new subtree for each.
        Renames as necessary any binders within e to avoid capturing any variables in the values of <subst>.
        Returns a new Expr, which may share subtrees with the original (as well as with values of <subst>).
        """
    return _cav_helper(e, tuple(), subst)


#####################################################################
# Name Generation


def make_nonfree_var(prefix, exprs, type=None):
    for idx in itertools.count():
        name = prefix + "_" + str(idx)
        if all(name not in e.free_vars_ for e in exprs):
            return Var(name, type)


#####################################################################
# CAV implementation


def _cav_helper(e: Expr, reqs: List[ReplaceLocationRequest], substs: VariableSubstitution) -> Expr:
    # First, work out if there's anything to do in this subtree
    substs = {varname: repl for varname, repl in substs.items() if varname in e.free_vars_}
    if len(reqs) == 0:
        if len(substs) == 0:
            # Nothing to do in this subtree
            return e
    elif any(len(req.target) == 0 for req in reqs):
        # Apply here
        if len(reqs) != 1:
            raise ValueError("Multiple ReplaceLocationRequests on locations nested within each other")
        if reqs[0].applicator is None:
            return reqs[0].payload
        # Continue renaming.
        renamed_e = _cav_helper(e, [], substs)
        return reqs[0].applicator(reqs[0].payload, renamed_e)
    # No ReplaceLocationRequest targets this node. Do any Expr-specific processing, perhaps recursing deeper.
    return _cav_children(e, reqs, substs)


@singledispatch
def _cav_children(e: Expr, reqs: List[ReplaceLocationRequest], substs: VariableSubstitution) -> Expr:
    return e.__class__(*_cav_child_list(e, reqs, substs))


def _requests_to_child(reqs: List[ReplaceLocationRequest], ch_idx: int) -> List[ReplaceLocationRequest]:
    return [
        ReplaceLocationRequest(req.target[1:], req.payload, req.applicator) for req in reqs if req.target[0] == ch_idx
    ]


def _cav_child_list(e: Expr, reqs, substs):
    # Applies _cav_helper to the children of the expression, and returns the list of substituted children
    return [_cav_helper(ch, _requests_to_child(reqs, ch_idx), substs) for ch_idx, ch in enumerate(get_children(e))]


@_cav_children.register
def _cav_const(e: Const, reqs, substs):
    return e


@_cav_children.register
def _cav_var(e: Var, reqs, substs):
    return substs.get(e.name, e)


@_cav_children.register
def _cav_call(e: Call, reqs, substs):
    name = e.name
    if name.se in substs:  # Will only match if name.se is a str.
        # The variable (holding the function we are calling) is being substituted.
        # It had better be a rename to another variable, because we can't Call anything else....
        name = substs[name.se].structured_name
    return Call(name, _cav_child_list(e, reqs, substs))


def _rename_if_needed(
    arg: Var, binder: Expr, reqs: List[ReplaceLocationRequest], subst: VariableSubstitution
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


@_cav_children.register
def _cav_lam(e: Lam, reqs, substs):
    arg, substs = _rename_if_needed(e.arg, e, reqs, substs)
    return Lam(Var(arg.name, type=arg.type_, decl=True), *_cav_child_list(e, reqs, substs))


@_cav_children.register
def _cav_let(e: Let, reqs, substs):
    new_vars = []
    # alpha-rename any capturing `e.vars` in `e.body` only
    body_substs = substs
    for var in [e.vars] if isinstance(e.vars, Var) else e.vars:
        new_var, body_substs = _rename_if_needed(var, e, reqs, body_substs)
        new_vars.append(new_var)
    if len(new_vars) == 1:
        assert isinstance(e.vars, Var)
        new_vars = new_vars[0]
    else:
        assert isinstance(e.vars, list)
    # Do not use _cav_child_list: we need different substitutions for each child.
    # Hence, this must match _idch_let's ordering of rhs/body.
    return Let(
        new_vars,
        _cav_helper(e.rhs, _requests_to_child(reqs, 0), substs),
        _cav_helper(e.body, _requests_to_child(reqs, 1), body_substs),
    )
