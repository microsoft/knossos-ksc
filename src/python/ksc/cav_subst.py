from functools import singledispatch
from typing import Callable, List, Mapping, NamedTuple, Optional, Tuple

from ksc.expr import StructuredName, Expr, If, Call, Let, Lam, Var, Const

### Capture-AVoiding SUBSTitution ###

class ReplaceLocationRequest(NamedTuple):
    """ This is a pure DTO/struct for recording requests to replace_subtree(s). """
    # Location within the root Expr, i.e. upon which replace_subtree is called, that should be replaced
    target_idx: int

    # An Expr to be "delivered" to that index, i.e. with the same value as it would have at the top.
    # That is, any binders on the path from the root to <target_idx> that bind variables free in <payload>,
    # will be alpha-renamed so that <payload> sees the same values in those variables that it would have at the root.
    payload: Expr

    # If applicator is None, then replace_subtree should merely replace the node at <target_idx> with <payload>.
    # Otherwise, the replacement (new) subtree is given by applicator(payload, x) where x is
    #    - the node currently at that location,
    #    - but *after* any alpha-renaming necessary to preserve the meaning of <payload>.
    applicator: Optional[Callable[[Expr, Expr], Expr]]=None

# Type of variable substitutions. This reflects Var.name being a 'str', as only bound Variable's need renaming,
# Call.name targets do not unless they are Var.name's).
VariableSubstitution = Mapping[str, Expr]

def replace_subtrees(e: Expr, reqs: List[ReplaceLocationRequest]) -> Expr:
    return _cav_helper(e, 0, sorted(reqs, key=lambda rlr: rlr.target_idx), {})

def replace_subtree(e: Expr, *args):
    return replace_subtrees(e, [ReplaceLocationRequest(*args)])

def replace_free_vars(e: Expr, subst: VariableSubstitution) -> Expr:
    return _cav_helper(e, 0, [], subst)

def _cav_helper(e: Expr, start_idx: int, reqs: List[ReplaceLocationRequest], substs: VariableSubstitution) -> Expr:
    # First, work out if there's anything to do in this subtree
    reqs = [req for req in reqs if req.target_idx in range(start_idx, start_idx + e.num_nodes)]
    substs = {varname: repl
        for varname, repl in substs.items()
        if StructuredName.from_str(varname) in e.free_vars}
    if len(reqs) == 0:
        if len(substs) == 0:
            # Nothing to do in this subtree
            return e
    elif reqs[0].target_idx == start_idx:
        # Apply here
        if reqs[0].applicator is None:
            return reqs[0].payload
        # Continue renaming. (And if any of the *other* ReplaceLocationRequests apply within, do those too.)
        renamed_e = _cav_helper(e, start_idx, reqs[1:], substs)
        return reqs[0].applicator(reqs[0].payload, renamed_e)
    # No ReplaceLocationRequest targets this node. Do any Expr-specific processing, perhaps recursing deeper.
    return _cav_children(e, start_idx, reqs, substs)

@singledispatch
def _cav_children(e: Expr, start_idx, reqs: List[ReplaceLocationRequest], substs: VariableSubstitution, factory=None) -> Expr:
    raise ValueError("May be called only on Expr's")

@_cav_children.register(Expr)
def _cav_expr(e: Expr, start_idx, reqs, substs, factory=None):
    # Applies _cav_helper to all children of e, then constructs a new version of e using factory.
    # If factory is None, e.__class__ is used.
    # Overridden in subclasses to deal with e.g. binding.
    if factory is None:
        factory = e.__class__
    new_children = []
    ch_idx = start_idx + 1
    for ch in e.children():
        new_children.append(_cav_helper(ch, ch_idx, reqs, substs))
        ch_idx += ch.num_nodes
    return factory(*new_children)

@_cav_children.register(Const)
def _cav_const(e: Const, start_idx, reqs, substs):
    return e

@_cav_children.register(Var)
def _cav_var(e: Var, start_idx, reqs, substs):
    return substs.get(e.name, e)

@_cav_children.register(Call)
def _cav_call(e: Call, start_idx, reqs, substs):
    name = e.name
    if name.se in substs: # Will only match if name.se is a str.
        # The variable (holding the function we are calling) is being substituted.
        # It had better be a rename to another variable, because we can't Call anything else....
        name = substs[name.se].structured_name
    return _cav_expr(e, start_idx, reqs, substs, factory=lambda *args: Call(name, list(args)))

def _rename_if_needed(arg: Var, reqs: List[ReplaceLocationRequest], subst: VariableSubstitution
) -> Tuple[Var, VariableSubstitution]:
    """ If <arg> binds a variable free in any Expr's in a request payload or the RHS of a substitution,
        then returns a new Var (chosen not to capture any variable free...as above), and an updated <subst>.
        Otherwise, returns <arg> and <subst> but *removing* any mapping for <arg>.
    """
    all_potential_binders = [req.payload for req in reqs] + list(subst.values())
    if any(arg.structured_name in rhs.free_vars for rhs in all_potential_binders):
        # Must rename "arg". Make a new name.
        nv = Var.numbered(max(e._next_unused_var_num for e in all_potential_binders))
        return nv, {**subst, arg.name: nv}
    return arg, {k:v for k,v in subst.items() if k != arg.name}

@_cav_children.register(Lam)
def _cav_lam(e: Lam, start_idx, reqs, substs):
    arg, substs = _rename_if_needed(e.arg, req, substs)
    return Lam(arg, _cav_helper(e.body, start_idx + 1, reqs, substs))


@_cav_children.register(Let)
def _cav_let(e: Let, start_idx, reqs, substs):
    new_vars = []
    # alpha-rename any capturing `e.vars` in `e.body` only
    body_subst = substs
    for var in ([e.vars] if isinstance(e.vars, Var) else e.vars):
        new_var, body_subst = _rename_if_needed(var, reqs, body_subst)
        new_vars.append(new_var)
    if len(new_vars) == 1:
        assert isinstance(e.vars, Var)
        new_vars = new_vars[0]
    else:
        assert isinstance(e.vars, list)
    # Do not call super(): we need different substitutions for each child
    return Let(new_vars,
        _cav_helper(e.rhs, start_idx + 1, reqs, substs),
        _cav_helper(e.body, start_idx + 1 + e.rhs.num_nodes, reqs, body_subst))
