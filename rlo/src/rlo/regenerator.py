"""
Regenerators are generators that can be called multiple times.

The outputs may be different e.g. due to shuffling or non-deterministic mappings.

There are direct equivalents in tf.data and will be superseded by those operations once
eager-mode is the norm.
"""
from typing import Callable, Generic, Iterable, Iterator, TypeVar

S = TypeVar("S")
T = TypeVar("T")


class Regenerator(Generic[T]):
    def __iter__(self) -> Iterator[T]:
        return iter(self.gen())

    def gen(self) -> Iterable[T]:
        raise NotImplementedError("Absract method")

    def map(self, map_func: Callable[[T], S]) -> "Regenerator[S]":
        return Mapped(self, map_func)

    def shuffle(self, rng) -> "Regenerator[T]":
        return Shuffled(self, rng)

    def cache(self) -> "Regenerator[T]":
        return Cached(self)


class Mapped(Regenerator[T]):
    def __init__(self, base: Iterable[S], map_func: Callable[[S], T]):
        self._base = base
        self._map_func = map_func

    def gen(self):
        for s in self._base:
            yield self._map_func(s)


class Shuffled(Regenerator[T]):
    """Includes caching."""

    def __init__(self, base: Iterable[T], rng):
        self._base = tuple(base)
        self._rng = rng

    def gen(self):
        perm = self._rng.permutation(len(self._base))
        return (self._base[i] for i in perm)


class Cached(Regenerator[T]):
    def __init__(self, base: Iterable[T]):
        self._base = tuple(base)

    def gen(self):
        return self._base
