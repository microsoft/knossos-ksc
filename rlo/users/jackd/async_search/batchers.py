import abc
from collections import deque
from typing import (
    TypeVar,
    Generic,
    Sequence,
    Iterable,
    List,
    Optional,
    Tuple,
)

import numpy as np

I = TypeVar("I")
O = TypeVar("O")


class Batcher(Generic[I]):
    @abc.abstractmethod
    def append(self, inp: I):
        """Append a single element."""

    def extend(self, inputs: Iterable[I]):
        """Extend by multiple elements."""
        for inp in inputs:
            self.append(inp)

    @abc.abstractmethod
    def pop(self) -> Sequence[I]:
        """Return a batch."""

    @abc.abstractmethod
    def has_full_batch(self) -> bool:
        """Indicates whether or not a full batch is present."""

    @abc.abstractmethod
    def empty(self) -> bool:
        """Indicates if there is any data at all."""


class BatchSizeBatcher(Batcher[I]):
    def __init__(self, batch_size: int):
        self._batch_size = batch_size
        self._queue = deque()

    def append(self, inp: I):
        self._queue.append(inp)

    def extend(self, inputs: Iterable[I]):
        self._queue.extend(inputs)

    def pop(self) -> List[I]:
        if self._batch_size >= len(self._queue):
            out = list(self._queue)
            self._queue = deque()
            return out
        return [self._queue.popleft() for _ in range(self._batch_size)]

    def has_full_batch(self) -> bool:
        return len(self._queue) >= self._batch_size

    def empty(self) -> bool:
        return not bool(self._queue)


class Unbatcher(Generic[O]):
    """Class for converting a stream of batched data into a stream of responses."""

    def __init__(self):
        self._queue = deque()
        self._partial = None

    @abc.abstractmethod
    def _concat(self, partial: Optional[O], outputs: O):
        """Get the result of concatenating `partial` with `outputs`."""
        raise NotImplementedError

    @abc.abstractmethod
    def _split(self, outputs: O, n: int) -> Tuple[O, O]:
        """Split the first `n` elements off outputs."""
        raise NotImplementedError

    def register(self, input_length: int):
        """
        Register the callback to run when `input_length` elements have been processed.
        """
        assert isinstance(input_length, int)
        self._queue.append(input_length)

    def unbatch(self, outputs: O) -> Iterable[O]:
        self._partial = self._concat(self._partial, outputs)
        while self._queue and self._queue[0] <= len(self._partial):
            n = self._queue.popleft()
            result, self._partial = self._split(self._partial, n)
            yield result


class NumpyUnbatcher(Unbatcher[np.ndarray]):
    def __init__(self):
        self._queue = deque()
        self._partial = None

    def _concat(self, partial: Optional[np.ndarray], outputs: np.ndarray):
        return (
            outputs if partial is None else np.concatenate((partial, outputs), axis=0)
        )

    def _split(self, outputs: np.ndarray, n: int) -> Tuple[np.ndarray, np.ndarray]:
        return outputs[:n], outputs[n:]


class BatchManager(Generic[I, O]):
    """Combines a Batcher and Unbatcher."""

    def __init__(self, batcher: Batcher[I], unbatcher: Optional[Unbatcher[O]] = None):
        self._batcher = batcher
        self._unbatcher = NumpyUnbatcher() if unbatcher is None else unbatcher

    def add_inputs(self, inputs: Sequence[I]):
        self._unbatcher.register(len(inputs))
        self._batcher.extend(inputs)

    def process_output(self, outputs: O) -> Iterable[O]:
        return self._unbatcher.unbatch(outputs)

    def pop_batch(self) -> Sequence[I]:
        return self._batcher.pop()

    def empty(self) -> bool:
        return self._batcher.empty()

    def has_full_batch(self) -> bool:
        return self._batcher.has_full_batch()
