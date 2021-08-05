# mypy: ignore-errors
from typing import NamedTuple, Sequence, Tuple

import numpy as np
import tensorflow as tf

from rlo.graph_data import GraphData, EdgeType, expr_to_graph_data
from rlo.expression_util import ExprWithEnv
from . import Pipeline


BatchedGraphData = NamedTuple(
    "BatchedGraphData",
    [
        ("node_reps", Sequence[np.ndarray]),
        ("edge_lists", Tuple[Sequence[np.ndarray], ...]),
    ],
)


def _row_lengths_to_splits(row_lengths):
    """
    Convert row_lengths to row_splits.

    Args:
        row_lengths: [N] int array of row lengths

    Returns:
        row_splits: [N+1] int array of indices of start/end of each row.
    """
    return np.pad(np.cumsum(row_lengths), [1, 0])


def _sort_by_dst(adj):
    order = np.argsort(adj[:, 1])
    return adj[order]


class GraphPipeline(Pipeline):
    """Manages graph input pipelining."""

    def __init__(self, use_subtree_match_edges: bool):
        """
        Args:
            use_subtree_match_edges: Whether to add edges (of a distinct edge type) linking expression sub-trees
                that are equal.
        """
        self._use_subtree_match_edges = use_subtree_match_edges
        self._num_edge_types = EdgeType.num_edge_types(use_subtree_match_edges)

    def prepare_example(self, exprenv: ExprWithEnv) -> GraphData:
        """ Returns a GraphData where the edges are sorted by destination index,
            as required by the tf.math.segment_sum used in the network. """
        node_reps, edge_lists = expr_to_graph_data(
            exprenv.expr, self._use_subtree_match_edges
        )
        assert len(edge_lists) == self._num_edge_types
        return GraphData(node_reps, tuple(_sort_by_dst(edges) for edges in edge_lists))

    @staticmethod
    def zip_elements(examples: Sequence[GraphData]) -> BatchedGraphData:
        return BatchedGraphData(
            node_reps=[ex.node_reps for ex in examples],
            edge_lists=tuple(zip(*(ex.edge_lists for ex in examples))),
        )

    @staticmethod
    def _num_nodes(example):
        return example.node_reps.shape[0]


def ragged_components(arrays: Sequence[np.ndarray]):
    """
    Convert sequence of arrays with different leading dimensions to ragged components.

    See `tf.RaggedTensor` for definitions, though operates/returns numpy arrays rather
    than tensors.

    Args:
        arrays: sequence of arrays with dynamic leading dimensions. Must all be the same
            dtype.

    Returns:
        flat_values, row_splits, row_lengths.
    """
    row_lengths = np.array([arr.shape[0] for arr in arrays], dtype=np.int64)
    row_splits = _row_lengths_to_splits(row_lengths)
    flat_values = np.concatenate(arrays, axis=0)
    return flat_values, row_splits, row_lengths


class SparsePipeline(GraphPipeline):
    def post_batch_map(
        self, batched_data: BatchedGraphData
    ):  # pylint:disable=no-self-use
        node_reps, edge_lists = batched_data
        node_type, node_row_splits, _ = ragged_components(node_reps)
        # row_splits are also the offsets applied to block-diagonalize
        edge_lists = tuple(
            np.concatenate(
                [
                    np.array(indices_per_sample, dtype=np.int32) + offset
                    for offset, indices_per_sample in zip(
                        node_row_splits.astype(np.int32), indices_per_edge_type
                    )
                ],
                axis=0,
            )
            for indices_per_edge_type in edge_lists
        )
        return dict(
            node_type=node_type, node_row_splits=node_row_splits, adjacency=edge_lists,
        )

    @property
    def batched_spec(self):
        return dict(
            node_type=tf.TensorSpec(shape=(None,), dtype=tf.int32, name="node_type"),
            node_row_splits=tf.TensorSpec(
                shape=(None,), dtype=tf.int64, name="node_row_splits"
            ),
            adjacency=tuple(
                tf.TensorSpec(
                    shape=(None, 2), dtype=tf.int32, name="edge_lists{}".format(i)
                )
                for i in range(self._num_edge_types)
            ),
        )


class DensePipeline(GraphPipeline):
    def post_batch_map(self, batched_data: BatchedGraphData):
        node_reps, edge_lists = batched_data
        batch_size = len(node_reps)
        node_type, node_row_splits, valid_lengths = ragged_components(node_reps)
        num_nodes = np.max(valid_lengths)

        adjacency_matrices = np.zeros(
            (batch_size, self._num_edge_types, num_nodes, num_nodes), dtype=np.float32
        )
        for e, batch_indices in enumerate(edge_lists):
            for b, ij in enumerate(batch_indices):
                src, dst = ij.T
                adjacency_matrices[b, e, dst, src] = 1

        return dict(
            node_type=node_type,
            node_row_splits=node_row_splits,
            adjacency=adjacency_matrices,
        )

    @property
    def batched_spec(self):
        return dict(
            node_type=tf.TensorSpec(
                (None,), dtype=tf.int32, name="node_type",
            ),  # leading_dim == num_nodes
            node_row_splits=tf.TensorSpec(
                shape=(None,), dtype=tf.int64, name="node_row_splits"
            ),  # leading_dim == batch_size + 1
            adjacency=tf.TensorSpec(
                (None, self._num_edge_types, None, None),
                dtype=tf.float32,
                name="adjacency_matrices",
            ),  # [batch_size, num_edge_types, max_nodes, max_nodes]
        )
