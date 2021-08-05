# pylint: disable=redefined-outer-name
import pytest
import numpy as np
import torch

from rlo.torch_graph_data import (
    DataConverter,
    BatchedGraphData,
    BatchedGraphDataWithTarget,
    create_dataloader,
)
from rlo.dataset import RawValueExample
from ksc.type import Type
from rlo.utils import torch_rng
from rlo.graph_data import EdgeType

from testutils import load_dataset, parse_expr_typed


@pytest.fixture(params=[True, False])
def use_subtree_match_edges(request) -> bool:
    return request.param


def get_data_converter(use_subtree_match_edges, num_time_heads: int = 1000):
    return DataConverter(
        num_time_heads=num_time_heads,
        use_subtree_match_edges=use_subtree_match_edges,
        cost_norm="none",
        device=torch.device("cpu"),
    )


# pylint: disable=not-callable
@pytest.mark.parametrize("batch_size", [10, 13, 1])
def test_batch_size(batch_size, use_subtree_match_edges):
    dataset = load_dataset("datasets/value_dataset.json")
    exprenvs = [
        parse_expr_typed(x, default_var_type=Type.Float)
        for _, x, _ in dataset[:batch_size]
    ]

    data_converter = get_data_converter(use_subtree_match_edges)
    batch = BatchedGraphData.collate(
        [data_converter.expr_to_graph(exprenv) for exprenv in exprenvs]
    )
    assert batch.num_graphs == batch_size


@pytest.mark.parametrize(
    "expr_str,num_nodes,num_edges", [("x", 1, 0), ("sub (mul a b) 3", 5, 8)]
)
def test_expr_to_graph(expr_str, num_nodes, num_edges, use_subtree_match_edges):
    exprenv = parse_expr_typed(expr_str, default_var_type=Type.Integer)
    graph_batch = get_data_converter(use_subtree_match_edges).expr_to_graph(exprenv)
    assert isinstance(graph_batch, BatchedGraphData)
    assert graph_batch.num_edges == num_edges
    assert graph_batch.num_nodes == num_nodes
    assert graph_batch.num_edge_types == EdgeType.num_edge_types(
        use_subtree_match_edges=use_subtree_match_edges
    )
    assert len(graph_batch.edge_lists) == EdgeType.num_edge_types(
        use_subtree_match_edges=use_subtree_match_edges
    )
    assert sum(x.shape[1] for x in graph_batch.edge_lists) == num_edges
    assert all(x.shape[0] == 2 for x in graph_batch.edge_lists)


@pytest.mark.parametrize(
    "expr_str,values,num_nodes,weight, target",
    [
        ("(sub (mul a b) 3)", [1, 2, 3], 5, [0, 1, 1, 0, 0], [1, 2, 3, 3, 3]),
        ("(mul (add (sub a b) a) b)", [10], 7, [0, 0, 0, 0, 0], [10, 10, 10, 10, 10]),
    ],
)
def test_prepare_training_example(
    expr_str, values, num_nodes, weight, target, use_subtree_match_edges
):
    num_time_heads = 5
    data_converter = get_data_converter(
        use_subtree_match_edges=use_subtree_match_edges, num_time_heads=num_time_heads
    )
    exprenv = parse_expr_typed(expr_str, default_var_type=Type.Integer)
    raw_example = RawValueExample(exprenv, values=np.array(values))
    graph_data = data_converter.prepare_training_example(raw_example)
    assert graph_data.graph.num_nodes == num_nodes
    assert graph_data.target.shape == (1, num_time_heads)  # pylint: disable=no-member
    assert graph_data.weight.shape == (1, num_time_heads)  # pylint: disable=no-member
    assert torch.equal(
        graph_data.target,  # pylint: disable=no-member
        torch.tensor([target], dtype=torch.float, device=graph_data.target.device),
    )
    assert torch.equal(
        graph_data.weight,  # pylint: disable=no-member
        torch.tensor([weight], dtype=torch.float, device=graph_data.target.device),
    )


def test_batching():
    # Check edge indices are correctly mangled when multiple graphs are batched together
    graph1 = BatchedGraphData(
        node_type=torch.tensor([0, 0, 0]),
        edge_lists=(
            torch.tensor([(1, 2, 0), (2, 0, 1)]),
            torch.tensor([(1, 0), (2, 0)]),
        ),
    )
    graph2 = BatchedGraphData(
        node_type=torch.tensor([1, 1]),
        edge_lists=(torch.tensor([(0,), (1,)]), torch.zeros(2, 0, dtype=torch.long)),
    )
    batch = BatchedGraphData.collate([graph1, graph2])
    assert torch.equal(batch.graph_id, torch.tensor([0, 0, 0, 1, 1]))
    assert torch.equal(batch.edge_lists[0], torch.tensor([(1, 2, 0, 3), (2, 0, 1, 4)]))
    assert torch.equal(batch.edge_lists[1], torch.tensor([(1, 0), (2, 0)]))


def test_create_batch_of_graphs(use_subtree_match_edges):
    expr_str = "mul (add (sub a b) a) b"
    num_time_heads = 5
    data_converter = get_data_converter(
        use_subtree_match_edges=use_subtree_match_edges, num_time_heads=num_time_heads
    )
    exprenv = parse_expr_typed(expr_str, default_var_type=Type.Integer)
    graph_batch = BatchedGraphData.collate(
        [data_converter.expr_to_graph(exprenv) for _ in range(2)]
    )
    assert graph_batch.num_edge_types == EdgeType.num_edge_types(
        use_subtree_match_edges
    )
    assert isinstance(graph_batch.edge_lists[0], torch.Tensor)
    assert all(x.shape[0] == 2 for x in graph_batch.edge_lists)


@pytest.mark.parametrize(
    "batch_size,max_nodes,len_dataloader", [(2, None, 30), (None, 20, None)]
)
def test_create_dataloader(
    batch_size, max_nodes, len_dataloader, raw_examples, use_subtree_match_edges
):
    num_time_heads = 5
    data_converter = get_data_converter(
        use_subtree_match_edges, num_time_heads=num_time_heads
    )
    prepared_examples = [
        data_converter.prepare_training_example(e) for e in raw_examples
    ]
    dataloader = create_dataloader(
        prepared_examples, batch_size=batch_size, max_nodes=max_nodes, rng=None,
    )
    if max_nodes is not None:
        for batch in dataloader:
            assert 0 < batch.num_nodes <= max_nodes or batch.num_graphs == 1
        with pytest.raises(TypeError):
            # batch sampler is a generator so this should not be allowed
            len(dataloader)
    else:
        assert len(dataloader) == len_dataloader
        for batch in dataloader:
            assert batch.graph.num_edge_types == EdgeType.num_edge_types(
                use_subtree_match_edges=use_subtree_match_edges
            )


@pytest.fixture
def rng():
    return torch_rng(0)


@pytest.fixture
def raw_examples():
    exprs = [
        parse_expr_typed(x, default_var_type=Type.Integer)
        for x in ["sub (mul a b) 3", "mul (add (sub a b) a) b"] * 30
    ]
    return [RawValueExample(e, values=np.array([1, 2, 3])) for e in exprs]


@pytest.mark.parametrize("batch_size, max_nodes", [(None, 100), (10, None)])
def test_shuffle(batch_size, max_nodes, raw_examples, use_subtree_match_edges):
    num_time_heads = 5
    data_converter = get_data_converter(
        use_subtree_match_edges=use_subtree_match_edges, num_time_heads=num_time_heads
    )
    prepared_examples = [
        data_converter.prepare_training_example(e) for e in raw_examples
    ]

    def first_two_epochs(seed):

        dataloader = create_dataloader(
            prepared_examples,
            batch_size=batch_size,
            max_nodes=max_nodes,
            rng=torch_rng(seed),
        )
        epoch0 = list(dataloader)
        epoch1 = list(dataloader)
        return epoch0, epoch1

    epoch0, epoch1 = first_two_epochs(seed=0)

    # We want fresh shuffle in each epoch
    assert not batch_equal(epoch0[0], epoch1[0])

    # Each batch within epoch should be different
    assert not batch_equal(epoch0[0], epoch0[1])

    # Data shuffle should be repeatable given same random seed
    epoch0_again, _ = first_two_epochs(seed=0)
    assert all(batch_equal(x, y) for x, y in zip(epoch0_again, epoch0)) and len(
        epoch0
    ) == len(epoch0_again)

    # Different seeds should give different batches
    epoch0_different_seed, _ = first_two_epochs(seed=1)
    assert not batch_equal(epoch0_different_seed[0], epoch0[0])


def batch_equal(
    batch_a: BatchedGraphDataWithTarget, batch_b: BatchedGraphDataWithTarget
) -> bool:
    # Test if two batches are equal
    return (
        type(batch_a) == type(batch_b)
        and len(batch_a.graph.edge_lists) == len(batch_b.graph.edge_lists)
        and all(
            torch.equal(a, b)
            for a, b in zip(batch_a.graph.edge_lists, batch_b.graph.edge_lists)
        )
        and torch.equal(batch_a.graph.node_type, batch_b.graph.node_type)
        and torch.equal(batch_a.graph.graph_id, batch_b.graph.graph_id)
        and torch.equal(batch_a.target, batch_b.target)
        and torch.equal(batch_a.weight, batch_b.weight)
    )
