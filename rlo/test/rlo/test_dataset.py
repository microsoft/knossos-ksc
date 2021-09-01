import numpy as np

from rlo.analytics import LogEventsToList
from rlo.dataset import StateValueDataset, PolicyNetDataset, RewriteId
from rlo.expression import Expression
from testutils import make_toplevel as MT
from rlo.policy_value_model import RawPolicyValueEvaluation
from ksc.type import Type
import pytest
from rlo import utils
from testutils import make_rng, random_increasing, assert_sequences_equal


def check_log_empirical_predicted(ds, tl_cache):
    with LogEventsToList(verbosity_limit=1) as log:
        ds.log_empirical_predicted(tl_cache)
    log = utils.single_elem(log.log_items)
    assert log["empirical_value"] == sorted(log["empirical_value"])  # monotone
    assert all(v >= 0 for v in log["empirical_value"])
    assert len(log["empirical_value"]) == len(log["predicted_value"])


def test_add_expr():
    ds = StateValueDataset()
    x = Expression.Variable("x")
    ds.add_expr(x, [(3, 1)])
    assert list(ds._max_values[x].critical_values.items()) == [(0, 0), (3, 1)]
    assert_sequences_equal(ds.get_examples(), [(x, [0, 0, 0, 1])])
    assert_sequences_equal(ds.get_examples(max_depth=4), [(x, [0, 0, 0, 1, 1])])
    with pytest.raises(ValueError):
        ds.add_expr(x, [(5, 1)])


def test_merge_dataset():
    x = Expression.Variable("x")
    ds = StateValueDataset.build_from_triples([(3, x, 1)])
    assert list(ds._max_values[x].critical_values.items()) == [(0, 0), (3, 1)]
    assert_sequences_equal(ds.get_examples(), [(x, [0, 0, 0, 1])])

    ds.merge_dataset(StateValueDataset.build_from_triples([(5, x, 1)]))
    assert list(ds._max_values[x].critical_values.items()) == [(0, 0), (3, 1)]
    assert_sequences_equal(ds.get_examples(), [(x, [0, 0, 0, 1, 1, 1])])

    ds.merge_dataset(StateValueDataset.build_from_triples([(5, x, 3)]))
    assert list(ds._max_values[x].critical_values.items()) == [(0, 0), (3, 1), (5, 3)]
    assert_sequences_equal(ds.get_examples(), [(x, [0, 0, 0, 1, 1, 3])])
    ds.merge_dataset(StateValueDataset.build_from_triples([(2, x, 2)]))
    assert list(ds._max_values[x].critical_values.items()) == [(0, 0), (2, 2), (5, 3)]
    assert_sequences_equal(ds.get_examples(), [(x, [0, 0, 2, 2, 2, 3])])


def test_add_merge_unsuccessful_results():
    d = StateValueDataset()
    x = Expression.Variable("x")
    d.add_expr(x, [(3, 0)])
    assert_sequences_equal(d.get_examples(), [(x, [0, 0, 0, 0])])

    d2 = StateValueDataset()
    d2.merge_dataset(d)
    assert_sequences_equal(d2.get_examples(), [(x, [0, 0, 0, 0])])


def test_policy_net_dataset():
    x = MT(Expression.Variable("x", type=Type.Float))
    y = MT(Expression.Variable("y", type=Type.Integer))
    value_dataset = StateValueDataset()
    value_dataset.add_expr(x, [(2, 2), (3, 2), (5, 3)])
    value_dataset.add_expr(y, [(2, 2)])
    ds = PolicyNetDataset(value_dataset)

    assert not ds.has_points()
    ds.add_point(3, x, RewriteId(1, 1), 1.0)
    ds.add_point(2, y, RewriteId(0, 1), 2.0)
    assert ds.has_points()
    assert_sequences_equal(
        ds.get_examples(),
        [
            (x, [0.0, 0.0, 2.0, 2.0, 2.0, 3.0], [((1, 1), [0.0, 0.0, 0.0, 1.0]),]),
            (y, [0.0, 0.0, 2.0], [((0, 1), [0.0, 0.0, 2.0])]),
        ],
    )

    ds2 = PolicyNetDataset()
    for point in [
        (4, x, RewriteId(1, 0), 1.0),
        (5, x, RewriteId(0, 2), 1.0),
    ]:
        # (2, x, 2.0, (1, 1), 2.0)]:
        ds2.add_point(*point)
    assert ds2.has_points()
    ds.merge_dataset(ds2)

    assert_sequences_equal(
        ds.get_examples(),
        [
            (
                x,
                [0.0, 0.0, 2.0, 2.0, 2.0, 3.0],
                [
                    ((0, 2), [0.0, 0.0, 0.0, 0.0, 0.0, 1.0]),
                    ((1, 0), [0.0, 0.0, 0.0, 0.0, 1.0]),
                    ((1, 1), [0.0, 0.0, 0.0, 1.0]),
                ],
            ),
            (y, [0.0, 0.0, 2.0], [((0, 1), [0.0, 0.0, 2.0])]),
        ],
    )

    rng = make_rng()
    tl_cache = {
        x: RawPolicyValueEvaluation(
            random_increasing(rng, 5.0, 6), np.random.normal(0, 1, (1, 6, 4))
        )
    }
    check_log_empirical_predicted(ds, tl_cache)
