import random

from rlo.expression import Expression, EF
from ksc.type import Type


def _make_sequences(seed):
    """ Returns a list of optimal rewrite sequences for different starting expressions, each expression of the form
        (add (let (t (tuple cheap expensive expensive expensive ...)) (get$i$n t))
             (let (t (tuple expensive cheap expensive expensive ...)) (get$i$n t))))
        where - "cheap" and "expensive" are computations (not variables);
              - each tuple has exactly one "cheap" (the rest expensive)
              - the <i> and <n> are the same on both sides of the add
              - the <i> corresponds to the "cheap" element in exactly one of the two tuples.

      The rewrite sequences are limited to length 3, in which case the optimal sequence is:
      [inline_let, delete_let, select_of_tuple] all on whichever side of the "add" produces the "cheap" computation.
      The same sequence can be performed on the other side of the "add", producing the "expensive" operation;
      this produces some gain but not as much.
      One can also do rewrite sequences e.g. inline_let, inline_let, delete_let, but these will have a worse cost than
      the starting expression - producing something like:
      (add (let (t (tuple cheap expensive ....)) (get$i$n (tuple cheap expensive ....)))
           (get$i$n (tuple expensive cheap ....)))
      this has a worse cost than the starting expression, i.e., the inline_let's are sufficiently uphill that one needs
      to follow with both delete_let and select_of_tuple to improve on the starting cost.

      Although a search algorithm such as A*, or multiple rollouts, are likely to be able to find the best cost
      (by trying *both* inline_let + delete_let + select_of_tuple sequences), the *proportion* of rollouts that
      reaches the unique best cost is indicative: if the model can tell which element of the tuple will be
      extracted by each get$i$n, then it can choose to inline the correct let; if the model cannot tell, success
      (the chance of reaching the oracle best cost) will max out at 50%.
    """
    seqs = []
    a = Expression.Variable("a")
    expensive = (
        Expression.Variable("x", type=Type.Integer)
        * Expression.Variable("y", type=Type.Integer)
        * Expression.Variable("z", type=Type.Integer)
    )

    def _make_tuple(size, cheap_index):
        elems = [expensive] * size
        elems[cheap_index] = cheap
        return EF.Tuple(*elems)

    rng = random.Random(seed)
    for tuple_size in range(2, 8):
        for tuple_index in range(tuple_size):
            cheap = Expression.Constant(rng.randint(0, 100))
            # Make two expressions "let (x (tuple ...)) (get$<tuple_index>$<tuple_size> x)" where
            # tuple_index identifies the cheap element in one expr and an expensive element in the other.
            good_tup = _make_tuple(tuple_size, tuple_index)
            bad_tup = _make_tuple(
                tuple_size,
                rng.choice([i for i in range(tuple_size) if i != tuple_index]),
            )
            assert good_tup.children[tuple_index] == cheap
            assert bad_tup.children[tuple_index] == expensive

            # Make the optimal sequence selecting from the tuple that should be inlined.
            good_seq = [
                EF.Let(a, good_tup, EF.Select(a, tuple_index, size=tuple_size)),
                EF.Let(a, good_tup, EF.Select(good_tup, tuple_index, size=tuple_size)),
                EF.Select(good_tup, tuple_index, size=tuple_size),
                cheap,
            ]
            # Following that sequence above should be the best we can do in three steps (and leaving the "bad" alone)
            # (Else, we can inline the bad variable, hillclimbing, and then delete the bad let, saving 0.1; then simplify the bad select,
            # for a smaller saving as the selected element is bigger than the "good" sequence)
            bad_exp = EF.Let(a, bad_tup, EF.Select(a, tuple_index, size=tuple_size))
            # Build two expressions containing both lets by adding the two together in either order, and their optimal sequences.
            seqs.extend(
                [[(g + bad_exp) for g in good_seq], [(bad_exp + g) for g in good_seq],]
            )
    return seqs


train_sequences = _make_sequences(seed=0)
test_sequences = _make_sequences(seed=1)
