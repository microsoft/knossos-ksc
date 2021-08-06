from typing import Mapping, Optional

from pyrsistent import pmap
from pyrsistent.typing import PMap

from rlo.expression import Expression

from ksc.type import Type

from rlo.costs_ksc import (
    assumed_vector_size,
    build_malloc_cost,
    if_selection_cost,
    if_epsilon,
    apply_cost,
    default_edef_cost,
    let_cost,
    add_cost,
    mul_cost,
)

# This is imported to enable (later) switching costs_ksc directly into place of this file
from rlo.costs_ksc import compare_epsilon  # pylint: disable=unused-import


# For some node_types, cost is sum of child costs + this number.
_node_costs = {
    "constant": 0,
    "select": 0,  # Extracting element from tuple.
    "apply": apply_cost,
    # Not sure whether these should be considered primitive functions...
    "stop": 0,  # if present - does not allow any rules to be applied to the consequent expr
    "assert": 1,
    # KSC distinguishes the following as being "primitive functions", subject to a call,
    # but there seems no advantage in treating them differently in RLO.
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

assert all(k in Expression.node_types for k in _node_costs.keys())


def cost(expr: Expression, defs: Mapping[str, Expression] = pmap()) -> float:
    return _compute_cost(expr, defs, pmap())


def _interpret_cost(e: Expression):
    # Evaluate a cost$ function. For now we support only functions whose body is a constant.
    # When we have an interpreter we can use that.
    assert e.op == "lam"
    e = e.right
    while e.op == "let":
        e = e.third
    if e.op == "constant":
        return e.value
    else:
        raise ValueError("Only constant cost$ functions supported ATM")


def _compute_cost(
    expr: Expression, defs: Mapping[str, Expression], bound_lams: PMap[str, float]
):
    # First we (must) special-case all nodes binding or dealing with the environment (bound_lams)
    if expr.op == "let":
        bound_cost = _compute_cost(expr.second, defs, bound_lams)
        body_cost = _compute_cost(
            expr.third,
            defs,
            bound_lams.set(expr.first.name, bound_cost)
            if expr.second.type is not None and expr.second.type.kind == "Lam"
            else bound_lams.discard(expr.first.name),
        )
        # For Lambdas, we incur no cost now (the cost will be incurred when the function is used).
        # This is fine for compute but may not put enough emphasis on program size.
        return (
            (0 if expr.second.without_stop().op == "lam" else bound_cost)
            + body_cost
            + let_cost
        )
    if expr.op == "variable":
        # For Lambdas, we incur the cost at variable read (=~= application) time,
        # as we didn't when we bound the variable in the let.
        # TODO it might be better if we could do this in the apply instead,
        # but at present "(def f rettype (x:type) (body))" sparses to "let f = (lam x body) in f"
        # so we must give that some cost to incentivize optimization.
        if expr.type is not None and expr.type.kind == "Lam":
            cost_fn = defs.get("cost$" + expr.name)
            return (
                bound_lams[expr.name]
                if expr.name in bound_lams
                else _interpret_cost(cost_fn)
                if cost_fn is not None
                else default_edef_cost
            )
        # Variable (eager evaluation)
        return 0
    if expr.op == "lam":
        assert expr.left.op == "variable" and expr.left.type is not None
        # Variable is not bound to a lam, so costs 0 (eager evaluation)
        return _compute_cost(expr.right, defs, bound_lams.discard(expr.left.name))
    if expr.op in ["build", "sumbuild"]:
        assert expr.second.op == "variable"
        size_cost = _compute_cost(expr.first, defs, bound_lams)
        body_cost = _compute_cost(
            expr.third, defs, bound_lams.discard(expr.second.name)
        )
        child_cost = (
            size_cost + body_cost * assumed_vector_size
        )  # absint would use expr.first insted of assumed_vector_size.
        if expr.op == "build":
            return build_malloc_cost + child_cost
        # For sumbuild, additional cost is (assumed_vector_size-1) * cost_of_add.
        body_elements = expr.third.type.num_elements(assumed_vector_size)
        return (assumed_vector_size - 1) * body_elements + child_cost
    if expr.op == "constant":
        return 0
    if expr.op == "stop" and expr.declared_cost is not None:
        return expr.declared_cost
    child_costs = [_compute_cost(c, defs, bound_lams) for c in expr.children]
    # Special case cost of 'if'
    if expr.op == "if":
        arm_cost = sum(
            # The most expensive child, we count in full (*1); we add the cheaper child * epsilon
            # This gives some benefit to optimizing the cheaper child and, unlike making the weights
            # sum to one, prevents if(True, a, 0) being cheaper than a (for expensive a)
            cost * weight
            for cost, weight in zip(sorted(child_costs[1:]), sorted([1, if_epsilon]))
        )
        return if_selection_cost + child_costs[0] + arm_cost
    # All remaining node_types have cost a constant plus sum of children, and type can be computed from children
    if expr.op == "tuple":
        op_cost = let_cost * len(child_costs)
    else:
        op_cost = _node_costs[expr.op]
        if expr.op in ["add", "mul", "sub", "div"]:
            # compute the cost of potentially broadcasted arithmetic operations
            l_type, r_type = [ch.type for ch in expr.children]
            l_size = 1 if l_type is None else l_type.num_elements(assumed_vector_size)
            r_size = 1 if r_type is None else r_type.num_elements(assumed_vector_size)
            op_cost *= max(l_size, r_size)
            op_cost += build_malloc_cost * (
                l_size > 1 or r_size > 1
            )  # malloc cost if vector
    return op_cost + sum(child_costs)


def elementwise_cost(
    typ: Type, cost_one_element: float, override_avs: Optional[int] = None
) -> float:
    if typ.is_tuple:
        return sum(
            [
                elementwise_cost(c, cost_one_element, override_avs)
                for c in typ.tuple_elems()
            ]
        )
    elif typ.is_tensor:
        avs = assumed_vector_size if override_avs is None else override_avs
        return build_malloc_cost + (
            (avs ** typ.tensor_rank)
            * elementwise_cost(typ.tensor_elem_type, cost_one_element, override_avs)
        )
    elif typ.kind in ["Integer", "Float", "Bool"]:
        return cost_one_element
    assert typ.is_lam_or_LM
    # It doesn't make sense to apply operation to Lam in first-order ksc.
    raise ValueError("elementwise_cost not defined for Lam / LM")
