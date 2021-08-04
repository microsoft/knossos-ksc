"""Asynchronous implementations of astar / rollout seach."""

import asyncio
from enum import Enum
from typing import Callable, Set, Sequence, TypeVar, NamedTuple

import numpy as np

from rlo import utils

N = TypeVar("N")


def get_priority(node, adv, depth):
    time_left = adv.shape[-1] - depth - 1
    assert time_left > 0
    return node.cost() - adv[..., time_left]


class SearchStatus(Enum):
    SEARCH_SPACE_EXHAUSTED = 0
    BUDGET_EXHAUSTED = 1


class AStarResult(NamedTuple):
    status: SearchStatus
    visited: Set


async def simple_rollout(
    adv_fn: Callable[[Sequence[N], int], asyncio.Future],
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
        adv = await adv_fn(neighbors)
        probs = probs_fn(neighbors, adv[:, steps_remaining])
        node = rng.choice(neighbors, p=probs)
        out.append(node)
    return tuple(out)


async def multi_rollout(
    start_node: N,
    adv_fn: Callable[[Sequence[N], int], asyncio.Future],
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
    return await asyncio.gather(
        *(simple_rollout(rng=utils.rng(rng), **kwargs) for i in range(num_rollouts))
    )


async def batched_astar_search(
    start_node: N,
    adv_fn: Callable[[Sequence[N]], Sequence[asyncio.Future]],
    neighbors_fn: Callable[[N], Sequence[N]],
    simulation_depth: int,
    gnn_budget: int,
    buffer_size: int,
) -> AStarResult:
    """
    asynchronous port approximating behaviour of `AStarSearcher._search_to_tree`.

    For reasons currently unknown this doesn't perform -exactly- the same transitions,
    but it's the same in spirit.

    Compared to AStarSearcher, buffer_size is equivalent to batch_size, though actual
    batching is left to the `adv_fn` implementation.

    Args:
        start_node: node to begin search from.
        adv_fn: asynchronous function approximating advantage at each step.
        neighbors_fn: function giving the set of neighboring nodes.
        simulation_depth: depth of simulation.
        gnn_budget: network evaluation limit. Unless the search space is exhausted,
            search will terminate once this value has been reached/exceeded.
        buffer_size: number of nodes we buffer before making a network request via
            `adv_fn`.

    Returns:
        AStarResult
    """
    pq = asyncio.PriorityQueue()
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
        advs = await adv_fn(buffer)
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


async def continuous_astar_search(
    start_node: N,
    neighbors_fn: Callable[[N], Sequence[N]],
    cost_fn: Callable[[N], float],
    adv_fn: Callable[[Sequence[N]], asyncio.Future],
    simulation_depth: int,
    gnn_budget: int,
    loop=None,
):
    """
    Continuous / asynchronous version of AStar search.

    This implementation makes requests to `adv_fn` as fast as possible. An
    `asyncio.sleep` call is made after each request.
    """
    if loop is None:
        loop = asyncio.get_event_loop()
    enqueued = set()  # visited + frontier + awaiting evaluation to put onto frontier
    # pq items are `(priority, depth, count, node)`. `count` is purely for tie-breaks
    pq = asyncio.PriorityQueue()
    enqueued.add(start_node)
    num_tasks = 0
    pq.put_nowait((0, 0, num_tasks, start_node))
    min_cost = cost_fn(start_node)

    async def enqueue(nodes, depth, count: int):
        advs = await adv_fn(nodes)
        for i, (node, adv) in enumerate(zip(nodes, advs)):
            priority = get_priority(node, adv, simulation_depth - depth)
            pq.put_nowait((priority, depth, count + i, node))

    # we should end up popping gnn_budget + 1 times, since we don't eval start_node
    for _ in range(gnn_budget + 1):
        _, depth, _, node = await pq.get()  # may be empty until futures are completed
        depth = depth + 1
        neighbors = [neigh for neigh in neighbors_fn(node) if neigh not in enqueued]
        enqueued.update(neighbors)
        for neigh in neighbors:
            min_cost = min(cost_fn(neigh), min_cost)
        if depth < simulation_depth:
            if num_tasks < gnn_budget:
                loop.create_task(enqueue(neighbors, depth, num_tasks))
                num_tasks += len(neighbors)
            else:
                return min_cost
        # give others a chance. These include:
        #   - `enqueue` tasks a chance to run
        #   - other search algs in the same process a chance to clog `adv_fn` buffer
        #   - flush
        await asyncio.sleep(0)
    assert pq.empty()
    return min_cost
