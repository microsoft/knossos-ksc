import numpy as np

from rlo.graph_data import EdgeType, GraphData, expr_to_graph_data
from testutils import parse_expr_typed


def parse_to_graph(s: str, use_subtree_match_edges: bool = True) -> GraphData:
    return expr_to_graph_data(parse_expr_typed(s).expr, use_subtree_match_edges)


def test_single_node():
    data = parse_to_graph("x")
    assert len(data.node_reps) == 1
    assert all(edges_of_type.shape == (0, 2) for edges_of_type in data.edge_lists)

    np.testing.assert_equal(
        data, parse_to_graph("y"),
    )

    data2 = parse_to_graph("0.0")
    assert len(data2.node_reps) == 1
    assert data.node_reps != data2.node_reps
    with np.testing.assert_raises(AssertionError):
        np.testing.assert_equal(data.node_reps, data2.node_reps)
    np.testing.assert_equal(data.edge_lists, data2.edge_lists)


def test_binary_arith():
    data = parse_to_graph("(mul x x)")
    assert len(data.node_reps) == 3
    assert data.node_reps[0] != data.node_reps[1]
    assert data.node_reps[1] == data.node_reps[2]
    for edge_type in [
        EdgeType.FIRST_CHILD_FWD,
        EdgeType.FIRST_CHILD_BWD,
        EdgeType.SECOND_CHILD_FWD,
        EdgeType.SECOND_CHILD_BWD,
    ]:
        # One edge of each type
        np.testing.assert_equal(len(data.edge_lists[edge_type]), 1)
    # Also there should be one equal-sub-expression edge, which runs in both directions
    np.testing.assert_equal(len(data.edge_lists[EdgeType.SUBTREE_MATCH]), 2)

    # Check a different arithmetic op looks the same except for the arithmetic op node-type
    data2 = parse_to_graph("(add x x)")
    assert data.node_reps[0] != data2.node_reps[0]
    np.testing.assert_equal(data.node_reps[1:], data2.node_reps[1:])
    np.testing.assert_equal(data.edge_lists, data2.edge_lists)


def test_match_edges():
    def skip_match_edges(data):
        return GraphData(
            data.node_reps,
            [
                edge_list
                for edge_type, edge_list in enumerate(data.edge_lists)
                if edge_type != EdgeType.SUBTREE_MATCH
            ],
        )

    xx = "(mul x x)"
    xy = "(mul x y)"
    data_xx = parse_to_graph(xx)
    data_xy = parse_to_graph(xy)
    with np.testing.assert_raises(AssertionError):
        np.testing.assert_equal(data_xx, data_xy)
    np.testing.assert_equal(skip_match_edges(data_xx), skip_match_edges(data_xy))

    data_xx_no_match = parse_to_graph(xx, use_subtree_match_edges=False)
    with np.testing.assert_raises(AssertionError):
        np.testing.assert_equal(data_xx, data_xx_no_match)
    np.testing.assert_equal(skip_match_edges(data_xx), data_xx_no_match)
    data_xy_no_match = parse_to_graph(xx, use_subtree_match_edges=False)
    np.testing.assert_equal(data_xx_no_match, data_xy_no_match)


def test_tuple():
    e = "(tuple 0 1 2)"
    assert len(parse_expr_typed(e).expr.nodes) == 4
    data = parse_to_graph(e)
    assert len(data.node_reps) == 4
    adj_lists = dict(enumerate(data.edge_lists))
    # Forward edges from tuple to children
    np.testing.assert_equal(
        adj_lists.pop(EdgeType.TUPLE_CHILD_FWD), [(0, 1), (0, 2), (0, 3)]
    )

    # Backward edges from children to tuple
    np.testing.assert_equal(
        adj_lists.pop(EdgeType.TUPLE_CHILD_BWD), [(1, 0), (2, 0), (3, 0)]
    )

    # No edges of any remaining edge type
    assert all(len(e) == 0 for e in adj_lists.values())
