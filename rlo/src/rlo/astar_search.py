# mypy: ignore-errors
from collections import Counter
from typing import Callable, Tuple, Dict, Set, Iterator, Sequence, Any, List

from rlo import analytics
from rlo import rewrites
from rlo.node_evaluation_cache import NodeEvaluationCache
from rlo.search_ops import AbstractSearcher
from rlo.tf_model import ModelWrapper
from rlo.expression_util import ExprWithEnv
from rlo.search_tree import (
    Episode,
    StateValueSearchTreeNode,
    Transition,
    TransitionList,
)
from rlo.utils import UrgencyQueue


def _convert_to_episodes(
    node: StateValueSearchTreeNode,
    route: TransitionList,
    seen: Set[StateValueSearchTreeNode],
) -> Iterator[Episode]:
    r"""
    Convert a search tree (DAG) into a list of episodes so that it is compatible with
    the dataset-building algorithm in search_ops.py.
    This function is a recursive function starting from the root node in the search tree.

    All the returned episodes will start at the same node (start expression). As an example, if the
    search tree looks like:
                  (A, 3)
                  /    \
             (B, 2)    (C, 2)
             /    \    /    \
        (C, 1)    (D, 1)    (E, 1)
                                 \
                                 (F, 0)
    The returned episodes will be:
     - A, B, C
     - A, B, D
     - A, C, E, F

    Args:
        node: root of the tree to be converted to episodes.
        route: route taken so far to reach this node.
        seen: set of nodes from which we've already called _convert_to_episodes.  If a node is in this set, 
          we won't expand it. Thus, if multiple paths reach the same expression in the same number of rewrites, only one 
          of those paths will be represented in the iterator that this function returns.

    """
    if node in seen:
        # Return an empty iterator
        return
    seen.add(node)

    child_episodes = [
        e
        for t in node.transitions
        for e in _convert_to_episodes(t.next_node, route + [t], seen)
    ]
    if len(child_episodes) > 0:
        yield from child_episodes
    else:
        # No episodes from children. Ensure there is an episode with this node in it by
        # generating whichever trajectory (from the root to here) that we traversed first.
        # (This depends upon the order of the rules in the ruleset, and locations, and is
        #  rather arbitrary, but stable across multiple searches and generations.
        #  Random order was tried, but did not work as well; this may mean we need to train longer,
        #  or we may want an order that's the same across multiple datasets being merged
        #  from different starting expressions but the same model).

        yield route.to_episode()  # Turn into a sequence of NodeAction instead of Transition


class EarliestEnqueuingCache(NodeEvaluationCache):
    """ A cache that
          - knows the earliest (greatest-time-left) Node for each Expression;
          - automatically fills in the children of greater-time-left nodes with transitions matching those of lower-time-left nodes for the same Expression,
          - enqueues the earliest node for any Expression that is evaluated
          - sets a field _pruned on all nodes it creates, that is True if another node for the same Expression but higher time-left is (ever) created. """

    def __init__(
        self,
        model_wrapper: ModelWrapper,
        batch_size: int,
        enqueue_fn: Callable[[StateValueSearchTreeNode], None],
    ):
        super().__init__(
            model_wrapper, batch_size, search_tree_node_class=StateValueSearchTreeNode
        )
        self._greatest_time_left: Dict[ExprWithEnv, int] = {}
        self._pruned_count = 0
        self._merges = 0
        self._enqueue_fn = enqueue_fn

    def get_node(
        self, exprenv: ExprWithEnv, time_left: int
    ) -> StateValueSearchTreeNode:
        node = super().get_node(exprenv, time_left)
        if hasattr(node, "_pruned"):
            # Existing node.
            assert time_left <= self._greatest_time_left[exprenv]
            assert node._pruned == (time_left < self._greatest_time_left[exprenv])
            self._merges += 1
        else:
            # New node.
            node._pruned = False
            if exprenv not in self._greatest_time_left:
                # First depth at which we have seen this Expression.
                self._greatest_time_left[exprenv] = time_left
            elif time_left < self._greatest_time_left[exprenv]:
                # We've already seen expression at greater time_left.
                # This node exists only to record the edge and allow maxing; it should not be expanded.
                self._pruned_count += 1
                node._pruned = True
            elif time_left > self._greatest_time_left[exprenv]:
                # Found a shorter route to the same Expression as before. (Must have had to climb over more of a hill.)
                prev_greatest = self.earliest_existing_node(exprenv)
                assert not prev_greatest._pruned
                self._greatest_time_left[exprenv] = time_left
                prev_greatest._pruned = True
                # Record the pruning now (rather than waiting until the node is ever "popped")
                self._pruned_count += 1

                # If the Expression had previously been expanded, we should be able to do the same for this node, using values in the cache.
                if prev_greatest.is_expanded():
                    # Make our list similarly. This may expand further (recursively),
                    # but won't queue any further evals as we only expand what's already within the cache/waitlist.
                    if __debug__:
                        n_evals, n_wait = (
                            self.total_posns_evaluated,
                            self.eval_queue_length,
                        )
                    node.transitions = list(
                        Transition(a, self.get_node(c.exprenv, time_left - 1))
                        for a, c in prev_greatest.transitions
                    )
                    assert (n_evals, n_wait) == (
                        self.total_posns_evaluated,
                        self.eval_queue_length,
                    )
                elif node.has_evaluation():
                    self._enqueue_fn(node)
        return node

    def process_batches(self) -> Sequence[ExprWithEnv]:
        exprs_evaluated = super().process_batches()
        for exprenv in exprs_evaluated:
            self._enqueue_fn(self.earliest_existing_node(exprenv))
        return exprs_evaluated

    def rollout_end_log_entries(self) -> Dict[str, Any]:
        return {
            **super().rollout_end_log_entries(),
            "pruned": self._pruned_count,
            "merges": self._merges,
        }

    def earliest_existing_node(self, exprenv: ExprWithEnv) -> StateValueSearchTreeNode:
        return self.get_existing_node(exprenv, self._greatest_time_left[exprenv])


class Expander:
    def __init__(self, rules: rewrites.RuleMatcher):
        self._expanded = 0
        self._branching_factor = Counter()
        self._rules = rules

    def __call__(
        self, node: StateValueSearchTreeNode, cache: NodeEvaluationCache
    ) -> None:
        """ Expands a StateValueSearchTreeNode.
            Note nodes can be expanded without using an Expander, e.g. Rollout,
            but we do not expect this to occur during an A* search. """
        assert not node.is_expanded()
        if node.time_left > 0:
            # Note: rollouts caches the child_exprs (which depends only on node.exprenv, not node.time_left)
            # and could thus be requested at different time_left. Here, we expect that caching to be done by get_node.
            child_expr_actions = [
                (rewrite, rewrite.apply(node.exprenv))
                for rewrite in self._rules.get_all_rewrites(node.exprenv)
            ]

            node.transitions = list(
                Transition(a, cache.get_node(c, node.time_left - 1))
                for a, c in child_expr_actions
            )
            self._expanded += 1
            self._branching_factor[len(node.children)] += 1
        else:
            node.transitions = []

    def log_entries(self):
        return {"expanded": self._expanded, "branching_factor": self._branching_factor}

    @staticmethod
    def verbose_log_entries():
        """ Subclasses can extend this method to return entries to be logged in
            rollout_end_verbose event (verbosity=1)
        """
        return {}


class LoggingExpander(Expander):
    """ Functionally the same as Expander but logs out the number of GNN evaluations, time_left
    and cost of the expanded node in rollout_end_verbose event.
    """

    def __init__(self, rules: rewrites.RuleMatcher):
        super().__init__(rules)
        self._expanded_nodes = []

    def __call__(
        self, node: StateValueSearchTreeNode, cache: NodeEvaluationCache
    ) -> None:
        num_evals = cache.total_posns_evaluated
        super().__call__(node, cache)
        self._expanded_nodes.append((num_evals, node))

    def verbose_log_entries(self):
        return {
            "expansions": [
                (num_evals, n.time_left, n.exprenv.cost())
                for num_evals, n in self._expanded_nodes
            ]
        }


class AStarSearcher(AbstractSearcher):
    def __init__(
        self,
        max_gnn: int,
        batch_size: int = 16,
        expander_factory: Callable[[rewrites.RuleMatcher], Expander] = LoggingExpander,
        cost_per_step=None,
        **kwargs
    ):
        super().__init__(**kwargs)
        if max_gnn < 1:
            raise ValueError("max_gnn {} must be a positive integer".format(max_gnn))
        self._max_gnn = max_gnn
        self._batch_size = batch_size
        self._expander_factory = expander_factory
        self._cost_per_step = cost_per_step

    def urgency(self, node: StateValueSearchTreeNode) -> float:
        # Negate expected final cost as higher urgency is better
        return node.evaluation(cost_per_step=self._cost_per_step) - node.exprenv.cost()

    def _search_impl(
        self, model_wrapper: ModelWrapper, random_source, *args, **kwargs
    ) -> Tuple[List[Episode], NodeEvaluationCache, Dict]:
        expander = self._expander_factory(self._rules)
        start_posn, cache, log_entries = self._search_to_tree(
            model_wrapper, random_source, expander, *args, **kwargs
        )

        episodes = list(
            _convert_to_episodes(
                node=start_posn, route=TransitionList(start_posn), seen=set()
            )
        )

        # Log final_costs as the minimum cost only, it's the only cost that matters
        log_entries.update(
            {
                "final_costs": Counter({cache.best_cost_seen: 1}),
                **expander.log_entries(),
                **cache.rollout_end_log_entries(),
            }
        )
        analytics.event(
            "rollout_end_verbose", verbosity=1, **expander.verbose_log_entries()
        )
        return (episodes, cache, log_entries)

    def _search_to_tree(
        self,
        model_wrapper: ModelWrapper,
        _random_source,
        expander,
        start_expr,
        **_kwargs
    ) -> Tuple[StateValueSearchTreeNode, EarliestEnqueuingCache, Dict[str, int]]:
        """ Performs a search, returning the start position, cache, and log entries.
            Mostly this exists as a hook for subclasses (specifically BeamSearcher)
            to override and keep the other parts of _search. """
        open_list = UrgencyQueue()  # values are SearchTreeNodes
        cache = EarliestEnqueuingCache(
            model_wrapper,
            self._batch_size,
            lambda node: open_list.put(self.urgency(node), node),
        )

        # Note this will cause the root Expression to be evaluated, which isn't strictly necessary (if there are no routes back to it).
        # But it'll help detect/handle routes back to the start node better if such are found.
        start_posn = cache.get_node(start_expr, self._simulation_depth)

        while cache.total_posns_evaluated < self._max_gnn:
            if open_list.empty() and cache.eval_queue_length == 0:
                break  # Nothing left we can do

            if open_list.empty() or cache.has_sufficient_posns_to_eval():
                cache.process_batches()

            node = open_list.get()
            # If the node has smaller time left than the best in the closed list, then ignore the node as it is a duplicate.
            if not node._pruned:
                expander(node, cache)

        return start_posn, cache, {"unexplored": len(open_list)}

    def _build_dataset(self, seed: int, episodes: List[Episode], **_kwargs):
        # Right now dataset is constructed from a set of episodes which is not compatible with A*'s way of generating data.
        # TODO We should investigate different ways to build the dataset differently.
        # In the meantime we hack the number of episodes to be enough to pass assertions in kmeans.
        if len(episodes) < self._num_episode_clusters:
            episodes = episodes * (self._num_episode_clusters // len(episodes))
            episodes.extend(episodes[: (self._num_episode_clusters - len(episodes))])
            assert len(episodes) == self._num_episode_clusters
        return super()._build_dataset(seed, episodes)
