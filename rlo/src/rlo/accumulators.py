from collections import defaultdict
from typing import (
    Callable,
    Collection,
    Dict,
    Generic,
    Iterable,
    Mapping,
    Set,
    Tuple,
    TypeVar,
)


from rlo.cum_sequence import CumMinSequence

N = TypeVar("N")  # Node


class UpperBoundsAccumulator(Generic[N]):
    """Class for accumulating upper bounds on future minimum cost achievable in `t` rewrites
    (lower bounds on `V`)
    """

    def __init__(self, value_fn: Callable[[N], float], max_time_left=None):
        self._inbound_nodes: Dict[N, Set[N]] = defaultdict(set)
        self._upper_bounds: Dict[N, CumMinSequence[float]] = {}
        self._value_fn = value_fn
        self._max_time_left = max_time_left

    @property
    def upper_bounds(self) -> Mapping[N, CumMinSequence[float]]:
        """
        Get a mapping from nodes to upper bounds.

        For efficiency, we avoid defensively copying the returned mapping. It may be
        mutable, though applying mutations directly may break this instance.
        """
        return self._upper_bounds

    @property
    def inbound_nodes(self) -> Mapping[N, Collection[N]]:
        """
        Get a mapping from nodes to a collection of nodes with recorded inbound edges.

        For efficiency, we avoid defensively copying the returned mapping. It may be
        mutable, though applying mutations directly may break this instance.
        """
        return self._inbound_nodes

    def _get_upper_bounds(self, node: N) -> CumMinSequence[float]:
        bounds = self._upper_bounds.get(node)
        if bounds is None:
            bounds = CumMinSequence()
            bounds.update(0, self._value_fn(node))
            self._upper_bounds[node] = bounds
        return bounds

    def add_edge(self, src: N, dst: N) -> None:
        """
        Add an edge from `src -> dst`.

        This adds the associated inbound edge and propagates upper bound improvements.

        Args:
            src: source node
            dst: destination node
        """
        if src != dst:  # no need to add self-edges
            self._inbound_nodes[dst].add(src)
            dst_bounds = self._get_upper_bounds(dst)
            self._update_edge(src, dst_bounds.critical_values.items())

    def _update_node(self, node: N, updates: Iterable[Tuple[int, float]]):
        for src in self._inbound_nodes[node]:
            self._update_edge(src, updates)

    def _update_edge(self, src: N, updates: Iterable[Tuple[int, float]]):
        src_bounds = self._get_upper_bounds(src)
        if self._max_time_left is not None:
            # This prevents recursing as deep, and computing improvement in as many steps,
            # as the longest (acyclic) path in the graph of Expressions.
            updates = ((t, v) for t, v in updates if t < self._max_time_left)
        updates = tuple((t + 1, v) for t, v in updates if src_bounds.update(t + 1, v))
        if len(updates) > 0:
            self._update_node(src, updates)
