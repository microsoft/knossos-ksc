import pytest

from rlo.expression import Expression
from rlo.expr_shuffler import ExpressionShuffler
from rlo import utils


@pytest.mark.parametrize("exprs_per_generation", [2, 3])
def test_expression_shuffler(exprs_per_generation):
    exprs = tuple(
        [Expression.Variable(name) for name in ["a", "b", "c", "d", "e", "f"]]
    )
    # Sanity check test
    assert len(set(exprs)) == len(exprs)
    assert len(exprs) % exprs_per_generation == 0
    generations_per_shuffle = len(exprs) // exprs_per_generation

    shuf = ExpressionShuffler(utils.rng, exprs, exprs_per_generation)
    assert all(len(shuf[i]) == exprs_per_generation for i in range(10))

    for cycle in range(3):
        shuffled = [
            e
            for i in range(generations_per_shuffle)
            for e in shuf[i + cycle * generations_per_shuffle]
        ]
        assert len(shuffled) == len(exprs)
        assert set(shuffled) == set(exprs)

    # Check the first two cycles are different (and not merely inverted)
    generations = 2 * generations_per_shuffle
    assert len(set([frozenset(shuf[i]) for i in range(generations)])) == generations


def test_expression_shuffler_no_shuffling():
    exprs = tuple(
        [Expression.Variable(name) for name in ["a", "b", "c", "d", "e", "f"]]
    )
    shuf = ExpressionShuffler(utils.rng, exprs, 0)

    for i in range(10):
        assert shuf[i] == exprs
