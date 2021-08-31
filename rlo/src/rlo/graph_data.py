# mypy: ignore-errors
from enum import IntEnum, unique
import itertools
from typing import NamedTuple, Tuple

import numpy as np

from rlo.expression import Expression
from rlo.group_subexps import nodes_and_groups


class GraphData(NamedTuple):
    """ Represents the Expr as a graph suitable for a GNN but in a format independent of PyTorch, Tensorflow, etc. """

    node_reps: np.ndarray
    """  Array of shape [num_nodes, ], each element being an integer node type.
        In the future this might become something more expressive to allow e.g. sharing between node-types """

    edge_lists: Tuple[np.ndarray, ...]
    """ A tuple of length <num_edge_types>. Each element is an array[num_edges, 2],
        containing for each edge a pair (src, dest) of indices into the node_reps array. """


@unique
class EdgeType(IntEnum):
    FIRST_CHILD_FWD = 0
    FIRST_CHILD_BWD = 1
    SECOND_CHILD_FWD = 2
    SECOND_CHILD_BWD = 3
    THIRD_CHILD_FWD = 4
    THIRD_CHILD_BWD = 5
    TUPLE_CHILD_FWD = 6
    TUPLE_CHILD_BWD = 7
    SUBTREE_MATCH = 8

    @classmethod
    def num_edge_types(cls, use_subtree_match_edges=True):
        return len(cls.__members__) - (0 if use_subtree_match_edges else 1)


# Check values are densely packed
assert min(EdgeType.__members__.values()) == 0
assert max(EdgeType.__members__.values()) == len(EdgeType.__members__) - 1
# Do not include these amongst the __members__ of the enum
EdgeType.forward_edge_types = [
    EdgeType.FIRST_CHILD_FWD,
    EdgeType.SECOND_CHILD_FWD,
    EdgeType.THIRD_CHILD_FWD,
]
EdgeType.backward_edge_types = [
    EdgeType.FIRST_CHILD_BWD,
    EdgeType.SECOND_CHILD_BWD,
    EdgeType.THIRD_CHILD_BWD,
]
EdgeType.other_edge_types = [
    EdgeType.TUPLE_CHILD_FWD,
    EdgeType.TUPLE_CHILD_BWD,
    EdgeType.SUBTREE_MATCH,
]
assert len(
    EdgeType.forward_edge_types
    + EdgeType.backward_edge_types
    + EdgeType.other_edge_types
) == len(EdgeType.__members__)

# Check we have enough edge types
assert all(
    node_type == "tuple" or len(EdgeType.forward_edge_types) >= num_ch
    for node_type, num_ch, in Expression.node_types.items()
)
assert all(
    node_type == "tuple" or len(EdgeType.backward_edge_types) >= num_ch
    for node_type, num_ch, in Expression.node_types.items()
)


def expr_to_graph_data(
    expression: Expression, use_subtree_match_edges: bool
) -> GraphData:
    adj_lists = [[] for _ in range(EdgeType.num_edge_types(use_subtree_match_edges))]

    if use_subtree_match_edges:
        nodes, groups = nodes_and_groups(expression)
        adjacency_edges = adj_lists[EdgeType.SUBTREE_MATCH]
        # <groups> is an iterable of lists of indices where in each inner list
        # the indices are of subtrees that are all equivalent. Link each such group into a clique.
        for idxs in groups:
            for idx_in_list, src_idx in enumerate(idxs):
                for dst_idx in idxs[idx_in_list + 1 :]:
                    adjacency_edges.extend([(src_idx, dst_idx), (dst_idx, src_idx)])
    else:
        nodes = expression.nodes

    tuple_edge_types = (
        itertools.repeat(EdgeType.TUPLE_CHILD_FWD),
        itertools.repeat(EdgeType.TUPLE_CHILD_BWD),
    )
    other_edge_types = (EdgeType.forward_edge_types, EdgeType.backward_edge_types)

    # `nodes` are ordered according to a depth-first traversal, meaning that for a node at index `idx`,
    # the indexes of the children nodes will be `idx + 1`, `idx + 1 + child1.num_nodes`, etc.
    for idx, n in enumerate(nodes):
        src = idx
        dst = idx + 1
        # Each consecutive child will be given a different edge type, except for tuple operations
        for fwd_edge_type, bwd_edge_type, child in zip(
            *(tuple_edge_types if n.op == "tuple" else other_edge_types), n.children,
        ):
            adj_lists[fwd_edge_type].append((src, dst))
            adj_lists[bwd_edge_type].append((dst, src))
            dst += child.num_nodes

    adj_lists = tuple(
        np.empty((0, 2), dtype=np.int32)
        if len(adj) == 0
        else np.array(adj, dtype=np.int32)
        for adj in adj_lists
    )
    node_types = np.array(
        [Expression.node_type_lookup[n.op] for n in nodes], dtype=np.int32
    )
    return GraphData(node_types, adj_lists)
