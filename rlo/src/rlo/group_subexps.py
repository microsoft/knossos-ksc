from typing import Iterable, List, Tuple

from rlo import utils
from rlo.expression import Expression


def nodes_and_groups(expr: Expression) -> Tuple[List[Expression], Iterable[List[int]]]:
    """
    Returns a list of all sub-expressions, and an iterable of lists of indices to sub-expressions that are equivalent.
    
    Example 1:
        (let (x 3) 
            add (
                (let (z 3) (add z (add x x)))
                (let (z 5) (add z (add x x)))
            )
        )

        Here, the two identical expressions '(add x x)' will be in one equivalence group (the closest binder for the
        free variable 'x' is the same).

        The four (single-node) sub-expressions 'x' will also be in one equivalence group.

    
    Example 2:
        In expression:

        (foo
            (let (x 3) (add x x))   # 1
            (let (x 4) (add x x))   # 2
            (let (y 3) (add y y))   # 3
        )

         - sub-expressions '(let (x 3) (add x x))' and '(let (y 3) (add y y))' are equivalent.
         - The sub-expressions `(add x x)` on line #1 and `(add y y)` on line #3 will not be in equivalence group,
           because they are in a different binding scope, even though they will evaluate to the same value.
         - '(let (x 3) (add x x))' and '(let (x 4) (add x x))' are not equivalent, because 'x' is assigned a 
           different value.
        
        Also, for each 'add' expression, the pair of identical variables within it will, of course, be in an 
        equivalence group.

    Args:
        expr: An expression

    Returns:
        A tuple of:
        * a list of subtrees (nodes) of the Expression; the same as expr.nodes, but returned to avoid an extra traversal
             (and more clearly corresponding to the second element as they are constructed by the same algorithm)
        * an iterable of lists of indices, where each list contains indices of nodes which are equivalent
             (compute the same value). Note that nodes that are not in 
    """
    nodes: List[Expression] = []
    closest_binders: List[int] = []

    def traverse(subexp: Expression, binder_stack: List[Tuple[str, int]]) -> None:
        idx = len(nodes)
        nodes.append(subexp)
        # Calculate the closest binder of a free-variable - intuitively, the scope of the subexp,
        # the highest point to which a let containing this subexp's value could be lifted.
        # (That is - this subexp cannot be the same as any other subexp unless their closest binder's are the same)
        closest_binder = -1  # Global
        for skip, (bv_name, binder_idx) in enumerate(reversed(binder_stack)):
            if bv_name in subexp.free_var_names:
                closest_binder = binder_idx
                if skip > 0 and len(subexp.children) > 0:
                    binder_stack = binder_stack[:-skip]
                break
        closest_binders.append(closest_binder)

        if subexp.is_binder:
            bound_stack = binder_stack + [(subexp.bound_var.name, idx)]
        for i, c in enumerate(subexp.children):
            traverse(
                c,
                bound_stack
                if subexp.is_binder and subexp.binds_in_child(i)
                else binder_stack,
            )

    traverse(expr, [])
    assert len(nodes) == expr.num_nodes
    assert len(closest_binders) == expr.num_nodes

    def equiv_groups() -> Iterable[List[int]]:
        # Group node indices by whether they have the same closest binder, same number of nodes, and are the same op.
        for g in utils.group_by(
            range(len(nodes)),
            lambda idx: (closest_binders[idx], nodes[idx].num_nodes, nodes[idx].op),
        ).values():
            # Skip obviously-singleton groups
            if len(g) >= 2:
                yield from utils.group_by(g, lambda idx: nodes[idx]).values()

    return nodes, equiv_groups()
