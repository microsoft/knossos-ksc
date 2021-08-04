from typing import Iterable, Tuple
from rlo.accumulators import UpperBoundsAccumulator


def simple_bounds_accumulator(neighbors: Iterable[Tuple[int, Iterable[int]]]):
    acc = UpperBoundsAccumulator(float)
    for src, neigh in neighbors:
        for dst in neigh:
            acc.add_edge(src, dst)
    return acc


def test_forward_chain():
    acc = simple_bounds_accumulator(((0, []), (1, [0]), (2, [1])))
    bounds = acc.upper_bounds
    assert tuple(bounds[0].critical_values.items()) == ((0, 0),)
    assert tuple(bounds[1].critical_values.items()) == ((0, 1), (1, 0))
    assert tuple(bounds[2].critical_values.items()) == ((0, 2), (1, 1), (2, 0))


def test_backward_chain():
    # same as test_forward_chain but adding in reverse order
    acc = simple_bounds_accumulator(((0, []), (1, [0]), (2, [1]))[-1::-1])
    bounds = acc.upper_bounds
    assert tuple(bounds[0].critical_values.items()) == ((0, 0),)
    assert tuple(bounds[1].critical_values.items()) == ((0, 1), (1, 0))
    assert tuple(bounds[2].critical_values.items()) == ((0, 2), (1, 1), (2, 0))
