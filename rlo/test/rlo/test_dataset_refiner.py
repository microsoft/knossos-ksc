from rlo.dataset import StateValueDataset
from rlo.dataset_refiner import (
    construct_dataset_refiner_factory,
    ReuseRefiner,
    BestAcrossGenerationsRefiner,
)
from rlo.expression import Expression, EF
from testutils import make_toplevel
from ksc.type import Type
import testutils


def test_reuse_refiner():
    refiner = construct_dataset_refiner_factory([ReuseRefiner])()
    e = make_toplevel(EF.Stop(Expression.Variable("x", type=Type.Integer), cost=5))
    d1 = StateValueDataset.build_from_triples([(1, e, 0)])
    d2 = StateValueDataset.build_from_triples([(2, e, 1)])

    assert refiner(e, d1) is None  # First search found nothing => skip training.
    assert refiner(e, d2) is d2  # Some improvement - use it.
    d1 = StateValueDataset.build_from_triples([(1, e, 0), (2, e, 0.5)])
    assert refiner(e, d1) is d2  # Worse than previous - so use previous.

    d3 = StateValueDataset()
    d3.add_expr(e, [(2, 2)])
    assert refiner(e, d3) is d3  # Further improvement - use it.

    d4 = StateValueDataset()
    d4.add_expr(e, [(2, 0)])
    assert refiner(e, d4) is d3  # No improvement on starting expr - use previous best.
    # A dataset that also has a different expression, which should be ignored:
    d4 = StateValueDataset.build_from_triples([(2, e, 0), (1, EF.Stop("y", cost=5), 5)])
    assert refiner(e, d4) is d3  # Not as good as previous best - use previous best.
    d4.merge_dataset(StateValueDataset.build_from_triples([(5, e, 2)]))
    assert refiner(e, d4) is d4  # As good as previous best, so use more recent


def test_no_op_dataset_refiner():
    refiner = construct_dataset_refiner_factory([])()

    e = EF.Stop("y", cost=4)
    d1 = StateValueDataset()
    d2 = StateValueDataset()

    def check(dataset, cost, dataset_expected, cost_expected):
        dataset_result, cost_result = refiner(e, (dataset, cost))
        assert dataset_result is dataset_expected and cost_result is cost_expected

    check(d1, 5.0, d1, 5.0)  # First search fails to find anything, but train anyway.
    check(d2, 8.0, d2, 8.0)  # No improvement in the new dataset, but accept it anyway.


def assert_pairs_sort_equal(lst1, lst2):
    def sort_order(tup):
        fst, _snd = tup
        return str(fst)

    testutils.assert_sequences_equal(
        sorted(lst1, key=sort_order), sorted(lst2, key=sort_order)
    )


def test_best_across_generations_refiner():
    refiner = construct_dataset_refiner_factory([BestAcrossGenerationsRefiner])()

    e1 = EF.Stop("z", cost=4)
    e2 = EF.Let("y", EF.Mul("x", 2), EF.Add("y", "y"))
    d1 = StateValueDataset()
    d1.add_expr(e1, [(1, 2)])
    d1.add_expr(e2, [(1, 0)])
    orig_seqs = d1.get_examples()

    d = refiner(e1, d1)
    assert d is d1
    testutils.assert_sequences_equal(d.get_examples(), orig_seqs)

    d2 = StateValueDataset()
    d2.add_expr(e1, [(1, 1), (2, 3)])
    d2.add_expr(e2, [(1, 1)])
    d = refiner(e2, d2)
    assert_pairs_sort_equal(d.get_examples(), [(e1, [0, 2, 3]), (e2, [0, 1])])

    testutils.assert_sequences_equal(d1.get_examples(), orig_seqs)
    d = refiner(e2, d1)
    assert d is d1
    assert_pairs_sort_equal(d.get_examples(), [(e1, [0, 2]), (e2, [0, 1])])


def test_best_across_generations_refiner_perf(seed=None):
    rng = testutils.make_rng(seed)
    e1 = EF.Stop("z", cost=4)
    refiner = construct_dataset_refiner_factory([BestAcrossGenerationsRefiner])()

    def increasing_critical_values(max, num):
        val = 0
        res = []
        for t in range(num):
            res.append((t, val))
            # 50% probability of same, or increase
            val += rng.integers(2) * rng.uniform(0, max - val)
        return res

    for l in range(1, 200):
        d = StateValueDataset()
        d.add_expr(e1, increasing_critical_values(100000, l))
        refiner(e1, d)
