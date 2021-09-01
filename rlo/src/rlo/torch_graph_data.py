from __future__ import annotations
from typing import (
    Generator,
    List,
    Iterable,
    Optional,
    Sequence,
    Tuple,
    Union,
    cast,
)
import functools
from dataclasses import dataclass

import numpy as np
import torch

from rlo.expression_util import ExprWithEnv
from rlo.dataset import RawValueExample
from rlo.cost_normalizers import cost_normalizer
from rlo.pipelines.pipeline import batch_with_max_nodes
from rlo.graph_data import expr_to_graph_data as expr_to_numpy_graph_data

# TODO use torchtyping to enforce that EdgeList is of shape [2, num_edges]
# 1st row holds the source node index, and the 2nd row the target node index
EdgeList = torch.LongTensor

# Note: RNGs
# We require RNGs to be passed for dataset shuffling. Code would still run with rng=None,
# but would have side-effects on the base random generator.


@dataclass(frozen=True)
class BatchedGraphData:
    node_type: torch.LongTensor  # [num_nodes,]
    edge_lists: Tuple[EdgeList, ...]  # One EdgeList per edge type.
    _graph_id: Optional[torch.LongTensor] = None  # [num_nodes, ]

    def __post_init__(self):
        # Check everything is on the same device, and that graph_ids contains a contiguous range of values starting at 0.
        device = self.node_type.device
        if self.edge_lists:
            assert self.edge_lists[0].device == device
        if self._graph_id is not None:
            assert self._graph_id.device == device
        assert torch.equal(
            torch.arange(self.num_graphs, dtype=torch.long, device=device),
            torch.unique_consecutive(self.graph_id),
        ), "The set of graph_ids must be a contiguous range starting at 0. Empty graphs are not supported."

    @property
    def graph_id(self):
        # graph_id[i] tells you which graph node i belongs to. If no _graph_id is passed to the constructor,
        # we assume the nodes and edges all belong to a single graph.
        return (
            self._graph_id
            if self._graph_id is not None
            else torch.zeros_like(self.node_type)
        )

    @property
    def num_edges(self) -> int:
        return sum((edge_index.size(-1) for edge_index in self.edge_lists))

    @property
    def num_edge_types(self) -> int:
        return len(self.edge_lists)

    @property
    def num_nodes(self) -> int:
        return len(self.node_type)

    @property
    def num_graphs(self):
        return len(torch.unique_consecutive(self.graph_id))

    @classmethod
    def collate(cls, batches: List[BatchedGraphData],) -> BatchedGraphData:
        """Combine multiple batches into one batch."""
        assert (
            len({b.num_edge_types for b in batches}) == 1
        ), "Every graph in the batch must have the same number of edge types"
        num_edge_types = batches[0].num_edge_types
        node_type = torch.cat([b.node_type for b in batches])

        # Accumulate num_nodes and add that to edge indices of each type.
        accumulated_num_nodes = np.cumsum([0] + [b.num_nodes for b in batches])
        accumulated_edge_lists = (
            torch.cat(
                [
                    b.edge_lists[edge_type] + batch_base_index
                    for b, batch_base_index in zip(batches, accumulated_num_nodes)
                ],
                dim=-1,
            )
            for edge_type in range(num_edge_types)
        )
        edge_lists = tuple(accumulated_edge_lists)

        # Accumulate num_graphs and add that to graph_id for each node.
        accumulated_graph_id = np.cumsum([0] + [b.num_graphs for b in batches])
        graph_id = torch.cat(
            [
                batch.graph_id + batch_base_graph_id
                for batch, batch_base_graph_id in zip(batches, accumulated_graph_id)
            ],
            dim=-1,
        )

        return cls(node_type=node_type, edge_lists=edge_lists, _graph_id=graph_id)  # type: ignore


@dataclass
class BatchedGraphDataWithTarget:
    """Represents a batch of training examples, consisting of graph 
    representation of expressions, target values for those expressions, 
    and weights to be used for e.g. importance weighting or ignoring padding.
    """

    graph: BatchedGraphData
    target: torch.Tensor  # Shape [num_graphs, ...]
    weight: torch.Tensor  # Same shape as target

    @classmethod
    def collate(cls, batches: List[BatchedGraphDataWithTarget]):
        return cls(
            graph=BatchedGraphData.collate([x.graph for x in batches]),
            target=torch.cat([x.target for x in batches], dim=0),
            weight=torch.cat([x.weight for x in batches], dim=0),
        )

    @property
    def num_nodes(self):
        return self.graph.num_nodes


class DataConverter:
    """
    Converts expressions to pytorch-geometric graphs and raw training examples to 
    pytorch-geometric graphs + target tensors.
    """

    def __init__(
        self,
        *,
        use_subtree_match_edges: bool,
        num_time_heads: int,
        cost_norm: str,
        device: torch.device,
    ):
        assert device is not None
        self._use_subtree_match_edges = use_subtree_match_edges
        self._num_time_heads = num_time_heads
        self._cost_norm = cost_normalizer(cost_norm)
        self._device = device

    def to(self, device: torch.device):
        """After calling this, graph data generated by this DataConverter will be on the specified device"""
        assert isinstance(device, torch.device)
        self._device = device

    def expr_to_graph(self, exprenv: ExprWithEnv) -> BatchedGraphData:
        """Convert one expression to graph data"""
        graph_data = expr_to_numpy_graph_data(
            exprenv.expr, use_subtree_match_edges=self._use_subtree_match_edges
        )
        # pylint: disable=not-callable
        node_type = torch.tensor(
            graph_data.node_reps, dtype=torch.long, device=self._device
        )
        return BatchedGraphData(
            node_type=cast(torch.LongTensor, node_type),
            edge_lists=tuple(
                cast(
                    torch.LongTensor,
                    torch.tensor(edge_index.T, dtype=torch.long, device=self._device),
                )
                for edge_index in graph_data.edge_lists
            ),
        )

    def denormalize_and_numpify(
        self, model_predictions: torch.Tensor, expr_batch: Iterable[ExprWithEnv]
    ):
        return self._cost_norm.denormalize_values(
            model_predictions.detach().cpu().numpy(), [e.cost() for e in expr_batch]
        )

    def prepare_training_example(
        self, raw_example: RawValueExample
    ) -> BatchedGraphDataWithTarget:
        """
        Prepare one training example so that it can be used to train the GNN:
        * Convert the expression to a graph.
        * Pad the target to the right shape
        * Weight entries in the target, so that padding has zero weight and so do zero-time-left values.
        """
        target, weight = self._prepare_target(raw_example)
        graph = self.expr_to_graph(raw_example.exprenv)
        return BatchedGraphDataWithTarget(graph=graph, target=target, weight=weight)

    def _prepare_target(
        self, raw_example: RawValueExample
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        target_np = raw_example.values
        valid_depth = min(len(target_np), self._num_time_heads)
        padding = self._num_time_heads - valid_depth
        if padding > 0:
            target_np = np.pad(
                target_np, (0, padding), mode="constant", constant_values=target_np[-1]
            )
        else:
            target_np = target_np[: self._num_time_heads]

        # pylint: disable=not-callable
        target = torch.tensor(
            [self._cost_norm.normalize_value(target_np, raw_example.exprenv.cost())],
            dtype=torch.float,
            device=self._device,
        )
        weight = torch.zeros_like(target)

        # Target at time 0 is always ignored
        weight[0, 1:valid_depth] = 1

        return target, weight


def create_dataloader(
    dataset: Union[Sequence[BatchedGraphData], Sequence[BatchedGraphDataWithTarget]],
    *,
    batch_size: Optional[int],
    max_nodes: Optional[int],
    rng: Optional[torch.Generator],
) -> torch.utils.data.DataLoader:
    """Create a pytorch DataLoader from some raw data.

    Args:
        examples: Raw data.
        batch_size: If specified, the number of examples to include in each batch.
        max_nodes: If specified, the max number of graph nodes to include in each batch.
          Exactly one of batch_size and max_nodes must be specified.
        rng: Random number generator to use for shuffling. If None, data will not be shuffled.
        
    """
    if (batch_size is None) == (max_nodes is None):
        raise ValueError("You must specify exactly one of batch_size or max_nodes")

    # See [Note: RNGs]
    shuffle = rng is not None

    def collate_fn(
        batch: Union[Sequence[BatchedGraphData], Sequence[BatchedGraphDataWithTarget]]
    ):
        return batch[0].__class__.collate(batch)  # type: ignore[arg-type]

    _get_dataloader = functools.partial(
        torch.utils.data.DataLoader, dataset=dataset, collate_fn=collate_fn
    )

    if batch_size is not None:
        # Fixed number of graphs per batch.
        return _get_dataloader(batch_size=batch_size, shuffle=shuffle, generator=rng)
    else:
        # Limit number of graph nodes per batch.
        assert max_nodes is not None
        return _get_dataloader(
            batch_sampler=MaxNodesBatchSampler(
                data=cast(Sequence, dataset),
                shuffle=shuffle,
                max_nodes=max_nodes,
                rng=rng,
            )
        )


class MaxNodesBatchSampler(torch.utils.data.Sampler):
    """
    Iterating over an instance of this class yields batches of graphs with no more than max_nodes 
    graph nodes in each batch. If a single graph has more than max_nodes, it will be returned in a batch of its own and a warning raised.
        
    When `shuffle=True`, data is reshuffled each time __iter__ is called, so that data will be reshuffled in each epoch of training.
    """

    def __init__(
        self,
        data: Sequence,
        shuffle: bool,
        max_nodes: int,
        rng: Optional[torch.Generator],
    ):
        """
        data: sequence of data samples of some type that implements a `num_nodes` method
        shuffle: if True, shuffle the data. If False, preserve the order of examples.
        max_nodes: Maximum number of nodes to include in a single batch.
        rng: random number generator to use for shuffling.
        """
        self._data = data
        self._shuffle = shuffle
        self._max_nodes = max_nodes
        if shuffle:
            # See [Note: RNGs]
            assert rng is not None

        self._rng = rng

    def __iter__(self) -> Generator:
        data: Union[Sequence, Generator]
        if self._shuffle:
            # Reshuffle every time __iter__ is called, so that we get data reshuffled for each epoch of training.
            data = (
                self._data[i]
                for i in torch.randperm(len(self._data), generator=self._rng)
            )
        else:
            data = self._data
        yield from batch_with_max_nodes(
            data,
            num_nodes_fn=lambda x: x.num_nodes,
            max_nodes=self._max_nodes,
            return_indices=True,
        )
