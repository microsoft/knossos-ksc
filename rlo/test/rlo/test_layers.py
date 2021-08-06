import numpy as np
import tensorflow as tf

from rlo.layers import DenseGNN, SparseGNN
from rlo.pipelines import DensePipeline, SparsePipeline
from rlo import utils
from testutils import parse_expr_typed, load_dataset


def eval_dense_gnn(
    expressions,
    use_subtree_match_edges,
    hidden_dim,
    num_propagations,
    weights=None,
    training=False,
):

    graph_data = DensePipeline(use_subtree_match_edges).prepare_batch(expressions)

    ragged_features = tf.RaggedTensor.from_row_splits(
        tf.one_hot(graph_data["node_type"], hidden_dim, dtype=tf.float32),
        graph_data["node_row_splits"],
    )

    adjacency_matrices = tf.convert_to_tensor(graph_data["adjacency"], tf.float32)

    adj_mat_transpose = tf.transpose(
        adjacency_matrices, [1, 0, 2, 3]
    )  # shape [E, B, V, V]
    layer = DenseGNN(num_propagations=num_propagations, recurrent_dropout=0.5)
    inputs = [ragged_features.to_tensor(), adj_mat_transpose]
    layer.build(tf.nest.map_structure(lambda x: x.shape, inputs))
    if weights is not None:
        for dst, src in zip(layer.weights, weights):
            dst.assign(src)
    gnn_output = layer(inputs, training=training)
    flat_output = tf.boolean_mask(
        gnn_output, tf.sequence_mask(ragged_features.row_lengths())
    )
    return tuple(w.numpy() for w in layer.weights), flat_output.numpy()


def eval_sparse_gnn(
    expressions, use_subtree_match_edges, hidden_dim, num_propagations, weights=None
):

    graph_data = SparsePipeline(use_subtree_match_edges).prepare_batch(expressions)

    node_features = tf.one_hot(graph_data["node_type"], hidden_dim, dtype=tf.float32)

    edge_lists = [
        tf.convert_to_tensor(adj, tf.int32) for adj in graph_data["adjacency"]
    ]
    layer = SparseGNN(num_propagations=num_propagations, recurrent_dropout=0.5)
    inputs = [node_features, edge_lists]
    layer.build(tf.nest.map_structure(lambda x: x.shape, inputs))
    if weights is not None:
        for dst, src in zip(layer.weights, weights):
            dst.assign(src)
    flat_output = layer(inputs)

    return tuple(w.numpy() for w in layer.weights), flat_output.numpy()


def parse_exprs(dataset, batch_size):
    """Parse first `batch_size` expressions in the dataset."""
    return [parse_expr_typed(expr_str) for _, expr_str, _ in dataset[:batch_size]]


def test_dense_gnn():
    """Ensure DenseGNN runs."""
    batch_size = 16
    hidden_dim = 200
    num_propagations = 10
    use_subtree_match_edges = True
    dataset = load_dataset("datasets/value_dataset.json")
    expressions = parse_exprs(dataset, batch_size)
    eval_dense_gnn(expressions, use_subtree_match_edges, hidden_dim, num_propagations)


def test_dense_sparse_consistent():
    """Ensure DenseGNN is consistent with SparseGNN."""
    batch_size = 16
    hidden_dim = 200
    num_propagations = 10
    use_subtree_match_edges = True
    dataset = load_dataset("datasets/value_dataset.json")
    expressions = parse_exprs(dataset, batch_size)
    weights, expected_output = eval_dense_gnn(
        expressions, use_subtree_match_edges, hidden_dim, num_propagations
    )

    weights, output = eval_sparse_gnn(
        expressions, use_subtree_match_edges, hidden_dim, num_propagations, weights
    )
    np.testing.assert_allclose(expected_output, output, rtol=1e-3, atol=1e-4)


def test_dense_consistent():
    """Ensure DenseGNN is consistent across runs."""
    batch_size = 16
    hidden_dim = 200
    num_propagations = 10
    use_subtree_match_edges = True
    dataset = load_dataset("datasets/value_dataset.json")
    expressions = parse_exprs(dataset, batch_size)

    w0, flat_out0 = eval_dense_gnn(
        expressions, use_subtree_match_edges, hidden_dim, num_propagations
    )
    w1, flat_out1 = eval_dense_gnn(
        expressions, use_subtree_match_edges, hidden_dim, num_propagations, w0
    )
    for w00, w11 in zip(w0, w1):
        np.testing.assert_equal(w00, w11)
    np.testing.assert_equal(flat_out0, flat_out1)


def test_sparse_consistent():
    """Ensure SparseGNN is consistent across runs."""
    batch_size = 16
    hidden_dim = 200
    num_propagations = 10
    use_subtree_match_edges = True
    dataset = load_dataset("datasets/value_dataset.json")
    expressions = parse_exprs(dataset, batch_size)

    w0, flat_out0 = eval_sparse_gnn(
        expressions, use_subtree_match_edges, hidden_dim, num_propagations
    )
    w1, flat_out1 = eval_sparse_gnn(
        expressions, use_subtree_match_edges, hidden_dim, num_propagations, w0
    )
    for w00, w11 in zip(w0, w1):
        np.testing.assert_equal(w00, w11)
    np.testing.assert_equal(flat_out0, flat_out1)


def test_dropout_working():
    batch_size = 16
    hidden_dim = 200
    num_propagations = 10
    use_subtree_match_edges = True
    dataset = load_dataset("datasets/value_dataset.json")
    expressions = parse_exprs(dataset, batch_size)

    with utils.random_state_context(context_seed=3):
        # make random dropout seeds consistent across runs
        w0, flat_out0 = eval_dense_gnn(
            expressions,
            use_subtree_match_edges,
            hidden_dim,
            num_propagations,
            training=True,
        )
        _, flat_out1 = eval_dense_gnn(
            expressions,
            use_subtree_match_edges,
            hidden_dim,
            num_propagations,
            w0,
            training=True,
        )

    assert not np.allclose(flat_out0, flat_out1)
