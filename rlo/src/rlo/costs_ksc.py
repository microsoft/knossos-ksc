from functools import singledispatch
from typing import Mapping, Union
import warnings

from ksc.expr import (
    StructuredName,
    make_structured_name,
    Def,
    Expr,
    If,
    Assert,
    Const,
    Call,
    Let,
    Lam,
    Var,
)

assumed_vector_size = 16
build_malloc_cost = 2
if_selection_cost = 1
if_epsilon = 0.0001

# This is overhead on top of the cost of the body of the called function,
# so strongly encourages inlining *whenever possible* (there is never a penalty).
apply_cost = 2

default_edef_cost = 7  # Unless specified otherwise in the edef declaration

let_cost = 0.1  # Also per-element cost of tuple
add_cost = 1
mul_cost = 2

compare_epsilon = 0.0001

_func_costs = {
    "stop": 0,
    "add": add_cost,
    "sub": add_cost,
    "mul": mul_cost,
    "div": 2,
    "exp": 4,
    "log": 4,
    "eq": 1,
    "gt": 1,
    "gte": 1,
    "to_float": 1,
    "$ranhashdoub": 1,
    "index": 1,
    "size": 1,
    "sum": assumed_vector_size - 1,  # Adding together assumed_vector_size elements
    "deltaVec": build_malloc_cost,
    "constVec": build_malloc_cost,
}


@singledispatch
def compute_cost(expr: Expr, defs: Mapping[StructuredName, Def]) -> float:
    # For non-overridden Exprs
    del expr, defs  # placate pylint
    return 0


@compute_cost.register
def _cost_let(expr: Let, defs):
    assert isinstance(
        expr.vars, Var
    ), "Tupled lets not supported - use untuple lets first"
    return let_cost + compute_cost(expr.rhs, defs) + compute_cost(expr.body, defs)


@compute_cost.register
def _cost_lam(expr: Lam, defs):
    # TODO: also map/fold
    raise AssertionError("Should not have a lam outside build/sumbuild")


@compute_cost.register
def _cost_if(expr: If, defs):
    cond_cost = compute_cost(expr.cond, defs)
    arm_costs = [compute_cost(expr.t_body, defs), compute_cost(expr.f_body, defs)]
    return (
        if_selection_cost
        + cond_cost
        + sum(
            # The most expensive arm counts full (*1); the cheaper arm * epsilon
            # This gives some benefit to optimizing the cheaper arm and, unlike
            # weights that sum to one, ensures if(True, a, 0) is never cheaper than a.
            cost * weight
            for cost, weight in zip(sorted(arm_costs), sorted([1, if_epsilon]))
        )
    )


@compute_cost.register
def _cost_assert(expr: Assert, defs):
    # Ignore the cost of the condition, as it'll be turned off in "release" builds.
    # Of course this means RLO has no incentive to optimize the condition,
    # nor any incentive not to pessimize it if there's nothing else to do.
    return 1 + compute_cost(expr.body, defs)


@compute_cost.register
def _cost_call(expr: Call, defs):
    func_name = expr.name.mangle_without_type()

    if func_name in ["build", "sumbuild"]:
        assert len(expr.args) == 2 and isinstance(expr.args[1], Lam)
        body_cost = compute_cost(expr.args[1].body, defs)
        child_cost = (
            compute_cost(expr.args[0], defs) + body_cost * assumed_vector_size
        )  # absint would use expr.first insted of assumed_vector_size.
        if func_name == "build":
            return build_malloc_cost + child_cost
        else:
            assert func_name == "sumbuild"
            # additional cost is for (assumed_vector_size-1) adds of two elements
            cost_of_add = (
                expr.args[1].body.type_.num_elements(assumed_vector_size) * add_cost
            )
            return (assumed_vector_size - 1) * cost_of_add + child_cost
    if func_name in ["map", "fold"]:
        raise NotImplementedError()
    args_cost: float = sum(compute_cost(a, defs) for a in expr.args)
    return _get_op_cost(expr, func_name, defs) + args_cost


def _get_op_cost(expr, func_name, defs) -> Union[int, float]:
    cost_func = defs.get(
        make_structured_name(("cost$" + expr.name.se[0], expr.name.se[1]))
    )  # TODO we should add [cost f] as a kind of derivation in ksc, then use that

    if expr.name in defs:
        if cost_func is not None:
            warnings.warn(
                f"Ignoring cost$ function {cost_func.name} for def {expr.name}"
            )
        # Note: we'll re-traverse the function every time here,
        # even tho the result depends only on the prelude.
        return apply_cost + compute_cost(defs[expr.name].body, defs)

    if cost_func is not None:
        # User-provided cost$ function. (Must be a constant for now.)
        if isinstance(cost_func.body, Const):
            op_cost = cost_func.body.value
        else:
            warnings.warn(
                f"Ignoring cost$ function {cost_func.name} as body is non-constant"
            )
            op_cost = default_edef_cost
        return op_cost + apply_cost  # TODO: not clear this is right, but parallels RLO

    if func_name == "tuple":
        return let_cost * len(expr.args)

    if func_name not in _func_costs:
        raise ValueError(f"Did not find definition or cost$ function for {func_name}")

    op_cost = _func_costs[func_name]
    if func_name in ["add", "mul", "sub", "div"]:
        # compute the cost of potentially broadcasted arithmetic operations
        l_size, r_size = [a.type_.num_elements(assumed_vector_size) for a in expr.args]
        op_cost *= max(l_size, r_size)
        # Add malloc cost for tensors
        op_cost += build_malloc_cost * (l_size > 1 or r_size > 1)
    return op_cost
