"""
Pipelining functions for graph data.

Included prebatch preprocessing, batching and post-batch manipulations. These will be
replaced by tf operations once we transition to eager mode first.
"""
from __future__ import annotations

import abc
import functools
import itertools

import warnings
from typing import (
    Callable,
    Iterable,
    List,
    Literal,
    Sequence,
    TypeVar,
    Union,
    cast,
    overload,
)

import tensorflow as tf

from rlo.dataset import RawValueExample, RawPolicyValueTrainingExample
from rlo.regenerator import Regenerator, Shuffled

RawExample = Union[RawValueExample, RawPolicyValueTrainingExample]


T = TypeVar("T")


def batch_with_batch_size(
    examples: Iterable[T], batch_size: int
) -> Iterable[Sequence[T]]:
    iterator = iter(examples)
    while True:
        out: List[T] = list(itertools.islice(iterator, batch_size))
        if len(out) == 0:
            break
        yield out


class BatchSizeChunker(Regenerator[Sequence[T]]):
    """Standard fixed-sized chunker."""

    def __init__(self, examples: Iterable[T], batch_size: int):
        self._examples = examples
        self._batch_size = batch_size

    def gen(self):
        return batch_with_batch_size(self._examples, self._batch_size)


@overload
def batch_with_max_nodes(
    examples: Iterable[T],
    num_nodes_fn: Callable[[T], int],
    max_nodes: int,
    return_indices: Literal[False] = False,
) -> Iterable[Sequence[T]]:
    # if return_indices is false, return batches with elements of type T
    pass


@overload
def batch_with_max_nodes(
    examples: Iterable[T],
    num_nodes_fn: Callable[[T], int],
    max_nodes: int,
    return_indices: Literal[True],
) -> Iterable[Sequence[int]]:
    # if return_indices is true, return batches with elements of type int (indices)
    pass


def batch_with_max_nodes(
    examples: Iterable[T],
    num_nodes_fn: Callable[[T], int],
    max_nodes: int,
    return_indices: bool = False,
) -> Union[Iterable[Sequence[T]], Iterable[Sequence[int]]]:
    """Split examples into batches with up to max_nodes nodes in each batch. 

    This method preserves the ordering of the examples in the returned batches.
    
    If one example is bigger than max_nodes, return it alone in a batch and raise a warning.

    Args:
        examples: Samples to be batched.
        num_nodes_fn: Returns number of nodes in an example.
        max_nodes: Maximum nodes per batch.
        return_indices: If True, return the indices of examples. If False, return the examples themselves.
          return_indices=True makes this generator usable as the `batch_sampler` argument to a pytorch DataLoader

    """
    batch: List[Union[T, int]] = []
    num_nodes_in_batch = 0  # Keep track of number of nodes in batch being built
    for i, example in enumerate(examples):
        num_nodes = num_nodes_fn(example)

        if num_nodes > max_nodes:
            # expression too large (will lead to a batch with more than max_nodes nodes)
            # -> raise a warning
            warnings.warn(
                "Too many nodes, {} > {}".format(num_nodes, max_nodes), UserWarning
            )

        if num_nodes_in_batch + num_nodes > max_nodes:
            # Flush the batch currently being accumulated
            yield batch
            batch = []
            num_nodes_in_batch = 0
        batch.append(i if return_indices else example)

        num_nodes_in_batch += num_nodes
    # Flush the last batch after all examples have been added
    if num_nodes_in_batch > 0:
        yield batch


class MaxNodesChunker(Regenerator[Sequence[T]]):
    """Chunking regenerator that accumulates up to `max_nodes` nodes per batch."""

    def __init__(
        self, examples: Iterable[T], num_nodes_fn: Callable[[T], int], max_nodes: int
    ):
        self._examples = examples
        self._num_nodes_fn = num_nodes_fn
        self._max_nodes = max_nodes

    def gen(self):
        return batch_with_max_nodes(self._examples, self._num_nodes_fn, self._max_nodes)


class Pipeline(abc.ABC):
    """Abstract base class for creating input generators/datasets."""

    @abc.abstractproperty
    def batched_spec(self):
        """`TensorSpec` structure corresponding to post_batch_map outputs."""
        raise NotImplementedError("Abstract method")

    @abc.abstractmethod
    def zip_elements(self, examples: Sequence):
        """
        Convert a sequence of structures into a structure of sequences.

        More flexible version of zip(*(sequence)) for nested tuple/dicts of numpy
        arrays.

        Will be automatically performed by batching with `tf.data.Dataset`s.
        """
        raise NotImplementedError("Abstract method")

    def prepare_example(self, example):  # pylint:disable=no-self-use
        """Optional pre-batch mapping."""
        return example

    def post_batch_map(self, batched_data):  # pylint:disable=no-self-use
        """Optional mapping function applied after batching/zip_elements."""
        return batched_data

    def prepare_batch(self, examples: Iterable):
        return self.post_batch_map(
            self.zip_elements([self.prepare_example(e) for e in examples])
        )

    def as_regenerator(
        self,
        examples: Iterable[RawExample],
        shuffle_rng=None,
        max_nodes=None,
        batch_size=None,
    ) -> Regenerator:
        """
        Get a regenerator associated with batches of examples.

        Chains the following operations:
            * `prepare_example`
            * optional shuffling
            * caching (either before shuffling or at the end)
            * chunking
            * element_zipping
            * `post_batch_map`ping

        Exactly one of max_nodes and batch_size must be provided.

        Args:
            examples: raw examples to be iterated through.
            shuffle_rng: random number generator used by shuffle. If None, no shuffling
                is performed.
            max_nodes: maximum number of nodes in each batch.
            batch_size: number of examples in each batch.

        Returns:
            Regenerator yielding (depth, graph_data, target) tuples.
        """

        def post_batch_map(arg):
            return self.post_batch_map(self.zip_elements(arg))

        if max_nodes is None == batch_size is None:
            raise ValueError("Exactly one of max_nodes or batch_size must be given")
        if max_nodes is None:
            batch_fn = functools.partial(BatchSizeChunker, batch_size=batch_size)
        else:
            chunker = cast(Callable, MaxNodesChunker)
            batch_fn = functools.partial(
                chunker, num_nodes_fn=self._num_nodes, max_nodes=max_nodes
            )

        prepared = (self.prepare_example(example) for example in examples)
        if shuffle_rng is None:
            return batch_fn(prepared).cache().map(post_batch_map)
        else:
            return batch_fn(Shuffled(prepared, shuffle_rng)).map(post_batch_map)

    def as_dataset(self, *args, **kwargs) -> tf.data.Dataset:
        """
        Get the dataset equivalent of `self.as_regenerator`.

        See `as_regenerator` for *args, **kwargs.
        """
        regen = self.as_regenerator(*args, **kwargs)

        spec = self.batched_spec
        return tf.data.Dataset.from_generator(
            lambda: regen,
            tf.nest.map_structure(lambda s: s.dtype, spec),
            tf.nest.map_structure(lambda s: s.shape, spec),
        ).prefetch(tf.data.experimental.AUTOTUNE)

    @abc.abstractmethod
    def _num_nodes(self, example):
        """ Get the number of nodes in an example (an Expression graph, or a tuple  of Expression graph and labels) """
        raise NotImplementedError("Abstract method")
