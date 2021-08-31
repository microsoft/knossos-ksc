import pytest
import numpy as np
import functools

from rlo import cost_normalizers
from rlo.dataset import StateValueDataset
from rlo.pipelines import value_training_pipeline, policy_value_training_pipeline
from rlo.pipelines.pipeline import batch_with_max_nodes
from rlo import utils
from mock_dataset import mock_policy_value_dataset
from testutils import load_training_examples


def load_training_sequences(num_time_heads: int = 3):
    dataset = StateValueDataset.build_from_triples(load_training_examples())  # type: ignore
    return dataset.get_examples(num_time_heads)


def check_total_num_nodes(sequences, batches):
    expected_num_nodes = sum(example[0].expr.num_nodes for example in sequences)
    total_num_nodes = sum(batch[0]["node_type"].shape[0] for batch in batches)
    np.testing.assert_equal(total_num_nodes, expected_num_nodes)


@pytest.mark.parametrize("batch_size", [223])  # a large prime
def test_batch_size_batching(batch_size):
    num_time_heads = 3
    sequences = load_training_sequences(num_time_heads)
    pipeline = value_training_pipeline(True, True, num_time_heads, None)
    check_total_num_nodes(
        sequences, pipeline.as_regenerator(sequences, batch_size=batch_size)
    )


@pytest.mark.parametrize("max_nodes", [10])
def test_max_nodes_batching(max_nodes):
    num_time_heads = 3
    sequences = load_training_sequences(num_time_heads)
    pipeline = value_training_pipeline(True, True, num_time_heads, None)
    with pytest.warns(UserWarning):
        check_total_num_nodes(
            sequences, pipeline.as_regenerator(sequences, max_nodes=max_nodes)
        )


@pytest.mark.parametrize("max_nodes", [100])
def test_sparse_batcher(max_nodes):
    num_time_heads = 3
    sequences = load_training_sequences(num_time_heads)
    pipeline = value_training_pipeline(True, True, num_time_heads, None)
    check_total_num_nodes(
        sequences, pipeline.as_regenerator(sequences, max_nodes=max_nodes)
    )


def check_actions(actual, num_nodes, expected):
    node_ids = [i for n in num_nodes for i in range(n)]
    np.testing.assert_equal([[node_ids[n], a] for n, a in actual], expected)


@pytest.mark.parametrize("seed", [0, 1])
@pytest.mark.parametrize("sparse", [False, True])
@pytest.mark.parametrize("normalizer", [None, "log", "prop", "logspeedup"])
def test_policy_value_training_pipeline(sparse, normalizer, seed):
    num_time_heads = 10
    dataset, rules = mock_policy_value_dataset()
    cost_norm = cost_normalizers.cost_normalizer(normalizer)
    pipeline = policy_value_training_pipeline(
        sparse, True, num_time_heads, rules, cost_norm=cost_norm
    )
    sequences = dataset.get_examples()
    kwargs = (
        {"batch_size": None, "max_nodes": 100}
        if sparse
        else {"batch_size": 5, "max_nodes": None}
    )
    print(f"Total number of sequences: {len(sequences)}")
    rng = utils.rng(seed)
    batches = list(pipeline.as_regenerator(sequences, shuffle_rng=rng, **kwargs))
    print(f"Number of batches: {len(batches)}")
    check_total_num_nodes(sequences, batches)
    expected_order = utils.rng(seed).permutation(range(len(sequences)))

    all_actions = [
        [action for action, _ in action_advs] for _, _, action_advs in sequences
    ]
    all_num_nodes = np.array([exprenv.expr.num_nodes for exprenv, _, _ in sequences])

    istart = 0
    for batch in batches:
        graphs, value_targets, value_weights, actions, advantages = batch
        num_graphs = len(graphs["node_row_splits"]) - 1
        num_nodes = len(graphs["node_type"])
        num_actions = len(actions)
        expected_indices = expected_order[istart : istart + num_graphs]
        istart += num_graphs
        expected_actions = [a for i in expected_indices for a in all_actions[i]]
        check_actions(actions, all_num_nodes[expected_indices], expected_actions)
        if sparse:
            assert len(graphs["node_type"]) <= kwargs["max_nodes"]
        else:
            assert len(graphs["adjacency"]) == num_graphs
            assert len(graphs["adjacency"]) <= kwargs["batch_size"]
        assert len(graphs["actions_mask"]) == num_nodes
        assert graphs["actions_mask"].shape[1] == len(list(rules))
        assert len(value_targets) == num_graphs
        assert len(value_weights) == num_graphs
        assert len(advantages) == num_actions
        assert actions.shape[1] == 2


def test_batch_with_max_nodes__preserve_order():
    num_examples = 1000
    max_nodes = 50

    examples = list(range(num_examples))

    def mock_example_length_fn(example: int) -> int:
        return 3 * max_nodes if example % 10 == 0 else 1

    with pytest.warns(UserWarning):
        batched_data = batch_with_max_nodes(
            examples, num_nodes_fn=mock_example_length_fn, max_nodes=max_nodes,
        )

        # Join the batches and assert that the order is the same
        batched_and_unbatched_examples = functools.reduce(
            lambda l1, l2: l1 + l2, batched_data, []
        )
        batched_and_unbatched_examples = sum(batched_data, [])

    for example1, example2 in zip(examples, batched_and_unbatched_examples):
        assert (
            example1 == example2
        ), "Examples have to be in the same order after unbatching"
