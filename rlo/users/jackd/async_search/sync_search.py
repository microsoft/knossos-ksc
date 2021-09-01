"""
A synchronous port of async_search with synchronous advantage function (adv_fn).

These are very minimal changes to async versions - mostly just removing `async` from
definitions and removing `await` keywords. They are provided to ease debugging and
aid testing.
"""
import queue
from typing import Callable, Sequence

import numpy as np

from async_search import N, SearchStatus, AStarResult, get_priority
from rlo import utils


def simple_rollout(
    adv_fn: Callable[[Sequence[N]], np.ndarray],
    neighbors_fn: Callable[[N], Sequence[N]],
    probs_fn: Callable[[Sequence[N], np.ndarray], np.ndarray],
    start_node: N,
    depth: int,
    rng: np.random.Generator,
):
    """Perform a single rollout."""
    node = start_node
    out = [node]
    for steps_remaining in range(depth - 1, -1, -1):
        neighbors = neighbors_fn(node)
        if len(neighbors) == 0:
            break
        adv = adv_fn(neighbors)
        probs = probs_fn(neighbors, adv[:, steps_remaining])
        node = rng.choice(neighbors, p=probs)
        out.append(node)
    return tuple(out)


def multi_rollout(
    start_node: N,
    adv_fn: Callable[[Sequence[N]], np.ndarray],
    neighbors_fn: Callable[[N], Sequence[N]],
    probs_fn: Callable[[Sequence[N], np.ndarray], np.ndarray],
    depth: int,
    seed: int,
    num_rollouts: int,
):
    """Perform multiple rollouts."""
    rng = np.random.default_rng(seed)
    kwargs = dict(
        start_node=start_node,
        adv_fn=adv_fn,
        neighbors_fn=neighbors_fn,
        depth=depth,
        probs_fn=probs_fn,
    )
    return [simple_rollout(rng=utils.rng(rng), **kwargs) for i in range(num_rollouts)]


def batched_astar_search(
    start_node: N,
    adv_fn: Callable[[Sequence[N]], np.ndarray],
    neighbors_fn: Callable[[N], Sequence[N]],
    simulation_depth: int,
    gnn_budget: int,
    buffer_size: int,
) -> AStarResult:
    """See async_search.batched_astar_search."""
    pq = queue.PriorityQueue()
    enqueued = set()
    count = 0
    # start_node will be popped first
    pq.put_nowait((0, 0, 0, start_node))
    enqueued.add(start_node)

    while not pq.empty():
        buffer = []
        depths = []
        while not pq.empty() and len(buffer) < buffer_size:
            _, depth, _, node = pq.get_nowait()
            if depth < simulation_depth:
                depth = depth + 1
                for neigh in neighbors_fn(node):
                    if neigh not in enqueued:
                        depths.append(depth)
                        buffer.append(neigh)
                        enqueued.add(neigh)
        advs = adv_fn(buffer)
        for i, (node, depth, adv) in enumerate(zip(buffer, depths, advs)):
            priority = get_priority(node, adv, depth)
            pq.put_nowait((priority, depth, count + i, node))
        num_enqueued = len(depths)
        count += num_enqueued

        if count >= gnn_budget:
            status = SearchStatus.BUDGET_EXHAUSTED
            break
    else:
        assert pq.empty()
        status = SearchStatus.SEARCH_SPACE_EXHAUSTED
    return AStarResult(status, enqueued)
