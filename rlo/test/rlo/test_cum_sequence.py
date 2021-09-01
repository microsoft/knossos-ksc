import pytest
import numpy as np
from rlo.cum_sequence import CumMaxSequence, CumMinSequence


@pytest.mark.parametrize("factory", [CumMaxSequence, CumMinSequence])
def test_empty(factory):
    seq = factory()
    np.testing.assert_equal(seq.to_list(), [])
    np.testing.assert_equal(seq.to_list(0), [])
    np.testing.assert_equal(len(seq), 0)


@pytest.mark.parametrize("factory", [CumMaxSequence, CumMinSequence])
def test_past_end(factory):
    seq = factory()
    seq.update(0, 0)
    np.testing.assert_equal(seq[1], 0)
    np.testing.assert_equal(seq[10], 0)


@pytest.mark.parametrize("factory,mult", [(CumMaxSequence, 1), (CumMinSequence, -1)])
def test_insert(factory, mult: int):
    seq = factory()
    v = np.arange(11) * mult  # values, non-negative for MaxSeq, non-positive for MinSeq
    seq.update(0, v[2])
    np.testing.assert_equal(len(seq), 1)
    np.testing.assert_equal(list(seq), [v[2]])
    seq.update(2, v[3])
    np.testing.assert_equal(list(seq), v[[2, 2, 3]])
    seq.update(2, v[2])  # should not overwrite - 2 < 3
    np.testing.assert_equal(list(seq), v[[2, 2, 3]])
    seq.update(1, v[2])  # should not add - same as left
    np.testing.assert_equal(list(seq), v[[2, 2, 3]])
    seq.update(1, v[3])  # should delete right duplicate
    np.testing.assert_equal(list(seq), v[[2, 3]])
    seq.update(5, v[3])  # should not add - same as left value
    np.testing.assert_equal(list(seq), v[[2, 3]])
    seq.update(5, v[5])
    np.testing.assert_equal(list(seq), v[[2, 3, 3, 3, 3, 5]])

    seq = factory()
    seq.update(0, v[2])
    seq.update(2, v[4])
    seq.update(3, v[10])
    np.testing.assert_equal(list(seq), v[[2, 2, 4, 10]])
    seq.update(1, v[7])
    np.testing.assert_equal(list(seq), v[[2, 7, 7, 10]])
    assert dict(seq.critical_values) == {0: v[2], 1: v[7], 3: v[10]}


@pytest.mark.parametrize("factory", [CumMaxSequence, CumMinSequence])
def test_negative_indices(factory):
    seq = factory()
    with pytest.raises(IndexError):
        seq.update(-1, 3)
    with pytest.raises(IndexError):
        seq[-1]
    # same even when it is not empty
    seq.update(0, 0)
    with pytest.raises(IndexError):
        seq.update(-1, 3)
    with pytest.raises(IndexError):
        seq[-1]


@pytest.mark.parametrize(
    "factory,reducer", [(CumMaxSequence, np.maximum), (CumMinSequence, np.minimum)],
)
def test_random_updates(factory, reducer):
    n = 100
    x = np.random.uniform(size=n)
    y = np.random.uniform(size=n)

    actual = factory()
    for i, v in enumerate(x):
        actual.update(i, v)
    np.testing.assert_equal(
        actual.to_list(n), reducer.accumulate(x)  # pylint:disable=no-member
    )
    for i, v in enumerate(y):
        actual.update(i, v)
    np.testing.assert_equal(
        actual.to_list(n),
        reducer.accumulate(reducer(x, y)),  # pylint:disable=no-member
    )
