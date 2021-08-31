from enum import Enum
from typing import Union

import numpy as np

from rlo.astar_search import AStarSearcher, EarliestEnqueuingCache, Expander
from rlo import utils
from rlo.expression_util import ExprWithEnv
from rlo.tf_model import ModelWrapper


class MergeHandling(Enum):
    """ What to do if a rollout hits a merge node (already reached by another route) """

    # Choose a successor of the merge node via normal softmax and carry on rolling out. This is the closest
    # to the original rollout algorithm, but can waste time (dierolls) repeatedly exploring the same nodes
    CONTINUE = 1
    # End rollout; assume we've explored children of the merge node enough unless one reaches top of Priority Queue
    STOP = 2
    # Never go down the edge to the merge node from its parent; choose an alternative sibling.
    # (If all children of current node are merge nodes, end rollout.)
    AVOID = 3


class HybridSearcher(AStarSearcher):
    """ A search algorithm that maintains a priority queue of unexplored nodes, like A*.
        After popping the highest-priority node from the queue, performs a rollout of some
        probabilistically-chosen length (or until hitting a merge/pruned node).
        Thus subsumes A* (rollout one step) and the Worklist-Rollout algorithm (rollout all the way)
        as extremes of a spectrum of hybrid algorithms. """

    def __init__(
        self,
        *args,
        alpha: float = float("inf"),
        prob_rollout: float = 1.0,
        merge_handling: MergeHandling = MergeHandling.STOP,
        **kwargs,
    ):
        """
        Args:
            alpha: Alpha value for softmax when doing rollouts only. Note that whereas for traditional rollout+caching,
                alpha values below INF were necessary to explore at all (beyond the one greedy rollout),
                and high alpha values meant we traversed the same path many times, Hybrid Search uses a worklist
                to ensure it always explores new nodes, even with alpha==INF.
            prob_rollout: Probability of performing a rollout step from an expanded node. (That is, of overriding
                the usual function of the priority queue, and "promoting" a softmax-policy-chosen successor
                of the expanded node to be chosen instead).
                0.0 makes HybridSearcher equivalent to AStarSearcher; 1.0 does a whole rollout (the "Worklist-Rollout" algorithm).
                Other values lead to exponentially-distributed lengths of rollout.
                Note that (only) value 1.0 ensures every explored node has a path to a descendant with time_left==0
                (except for the last rollout, truncated by search budget).
            merge_handling: What to do when a rollout hits a merge node (same Expression previously seen at same time_left).
                See MergeHandling. Note that when alpha==INF, MergeHandling.CONTINUE may not be used, as it would be
                equivalent (but less efficient) to MergeHandling.STOP (repeating *the same* greedy rollout and not exploring).

        Raises:
            ValueError: [description]
        """
        super().__init__(*args, **kwargs)
        if np.isinf(alpha) and merge_handling == MergeHandling.CONTINUE:
            raise ValueError(
                "At infinite alpha, MergeHandling.CONTINUE is equivalent to STOP"
            )
        self._alpha = alpha
        self._prob_rollout = prob_rollout
        self._merge_handling = merge_handling

    def _search_to_tree(  # type: ignore[override]
        self,
        model_wrapper: ModelWrapper,
        random_source: Union[int, np.random.Generator],
        expander: Expander,
        start_expr: ExprWithEnv,
        **_kwargs,
    ):
        open_list = utils.UrgencyQueue()
        cache = EarliestEnqueuingCache(
            model_wrapper,
            self._batch_size,
            lambda node: open_list.put(self.urgency(node), node),
        )

        rng = utils.rng(random_source)

        paused_rollouts = []

        def maybe_rollout(node):
            if rng.uniform(0, 1) < self._prob_rollout:
                do_rollout(node)

        def do_rollout(node):
            if not all(c.has_evaluation() for c in node.children):
                paused_rollouts.append(node)
                return
            children = [
                c
                for c in node.children
                # Somewhat arbitrarily, make AVOID also avoid pruned states - even tho those expressions may not have been expanded (at any time-left).
                # An alternative would be to check cache.earliest_existing_node(c.expr).is_expanded().
                if not (
                    self._merge_handling == MergeHandling.AVOID
                    and (c._pruned or c.is_expanded())
                )
            ]
            if len(children) == 0:
                return
            selected_child = rng.choice(
                children,
                p=utils.softmax(
                    [self.urgency(c) for c in children], alpha=self._alpha,
                ),
            )
            # Allow rollout to go through pruned states.
            # Even with an oracle value function, the greedy rollout may have to do so to reach its optimal cost.
            # (And, the greedily-chosen/best child of a pruned state may be different from the higher-time-left equivalent.)
            if selected_child._pruned:
                earliest = cache.earliest_existing_node(selected_child.exprenv)
                if not earliest.is_expanded():
                    # Expression has not been expanded at any time-left. Expand the highest-known time-left now,
                    # so we put onto the open_list those highest-time-left states.
                    expander(earliest, cache)
            if not selected_child.is_expanded():
                # If selected_child has been pruned, expanding it will not put any new states onto the open list.
                expander(selected_child, cache)
            elif self._merge_handling == MergeHandling.STOP:
                return
            maybe_rollout(selected_child)

        start_posn = cache.get_node(start_expr, self._simulation_depth)

        while cache.total_posns_evaluated < self._max_gnn:
            if open_list.empty() and cache.eval_queue_length == 0:
                break  # Nothing left we can do

            if open_list.empty() or cache.has_sufficient_posns_to_eval():
                cache.process_batches()
                # We've just evaluated every child for which a rollout might be waiting (to determine node policy)
                to_resume = paused_rollouts
                paused_rollouts = []
                for node in to_resume:
                    do_rollout(node)
            else:
                # All rollouts paused (waiting to fill a batch). Start a new one from a point chosen with the highest urgency (like A*)
                node = open_list.get()
                if node._pruned or node.is_expanded():
                    # Might have children if a rollout has passed through here *while*
                    # the node was on the open list (the open_list doesn't support removal).
                    # Node has already been explored and it's children enqueued too, so skip.
                    continue
                expander(node, cache)
                maybe_rollout(node)
        return start_posn, cache, {"unexplored": len(open_list)}
