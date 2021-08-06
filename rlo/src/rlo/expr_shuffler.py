import numpy
from typing import Callable, Sequence

from rlo.expression_util import NamedExprWithEnv
from rlo import utils


class ExpressionShuffler:
    """
    A potentially-infinite sequence of tuples of Expressions, each being a batch of length
    <exprs_per_generation> drawn from an input set/list, drawing (deterministically)
    without replacement until the input sequence is exhausted before refilling.
    """

    def __init__(
        self,
        rng_factory: Callable[[int], numpy.random.Generator],
        exprs: Sequence[NamedExprWithEnv],
        exprs_per_generation: int,
    ):
        """
        Args:
            rng_factory: A function that returns a random number generation for a given generation 
            exprs: Expressions to shuffle.
            exprs_per_generation: Number of expressions to return each generation. This number MUST evenly
                divide the total number of expressions. If the number is <= 0, all expressions return for every
                generation (equivalent to exprs_per_generation == len(exprs))

        Raises:
            ValueError: If the number of expressions to return per generation doesn't evenly divide the total
                number of expressions (can't evenly split expressions into batches of fixed size).
        """
        self._rng_factory = rng_factory
        self._exprs = tuple(exprs)

        # If expressions per generation <= 0, yield _all_ expressions for every generation
        if exprs_per_generation <= 0:
            exprs_per_generation = len(self._exprs)
        if len(self._exprs) % exprs_per_generation != 0:
            raise ValueError(
                "Exprs_per_generation {} must be exact divisor of training set length {}".format(
                    exprs_per_generation, len(self._exprs)
                )
            )
        self._generations_per_shuffle = len(self._exprs) // exprs_per_generation
        self._exprs_per_generation = exprs_per_generation

    def __getitem__(self, generation: int) -> Sequence[NamedExprWithEnv]:
        if self._generations_per_shuffle == 1:
            # Since we are training on all the expressions every generation,
            # the order isn't important (it changes only the seeding for each).
            return self._exprs
        # We shuffle the Expressions only after a complete pass through,
        # so drawing without replacement until we've exhausted the set.
        shuffled = utils.permutation(
            self._rng_factory(generation // self._generations_per_shuffle), self._exprs
        )
        first_expr = (
            generation % self._generations_per_shuffle
        ) * self._exprs_per_generation
        return shuffled[first_expr : first_expr + self._exprs_per_generation]
