# fmt: off
from typing import Dict

import numpy as np

from rlo.agent import NodeNotReady
from rlo.rewrites import RuleMatcher
from rlo.search_tree import Transition
from rlo import utils

class StateValueSoftmaxPolicy:
    def __init__(self, rules: RuleMatcher, alpha: float, rng: np.random.Generator):
        self._rules = rules
        self._alpha = alpha
        self._rng = rng
        self._child_expr_action_map: Dict = dict() # from Expression to list of (Rewrite, Expression)

    def try_sample_child(self, node, cache):
        """ Tries to sample a child of a node

        Args:
            node: parent node (with time_left greater than 0)
            cache: an instance of StateValueCache

        Returns:
            (1) an empty list, if there is no possible next step;
            (2) a singleton list containing a Transition, otherwise.

        raises NodeNotReady if the probability is not ready (need to wait for process_batches)
        """
        assert node.time_left > 0
        if not node.is_expanded(): # This happens once only the first time a rollout goes through this node
            if node.exprenv not in self._child_expr_action_map:
                child_expr_actions = [(rewrite, rewrite.apply(node.exprenv)) for rewrite in self._rules.get_all_rewrites(node.exprenv)]
                self._child_expr_action_map[node.exprenv] = child_expr_actions
            # Note that if get_node evaluates a batch, the current rollout is *not* in rollouts_waiting_for_values yet
            node.transitions = list(
                Transition(a, cache.get_node(c, node.time_left - 1))
                for a, c in self._child_expr_action_map[node.exprenv]
            )

            # If we can compute child probs now, do so. (This happens if every child has been reached by some other route,
            # or if the last child enqueued by get_node filled a minibatch and was immediately evaluated).
            if len(node.children)>0 and all(c.has_evaluation() for c in node.children):
                self.compute_probs(node)
            # Then fall through. The common case is that node.probs is None and the rollout enters rollouts_waiting_for_values.
        if len(node.children)==0:
            # No possible next steps, i.e. no applicable rules
            return []
        if node.probs is None: # May happen many times
            # Still waiting for child estimates; rollout will become pending.
            raise NodeNotReady
        transitions = list(node.transitions)
        index = self._rng.choice(len(transitions), p=node.probs)
        return [transitions[index]]

    def compute_probs(self, node):
        # This will raise if any children have None _evaluation (value estimate).
        action_values = [c.evaluation() + node.exprenv.cost() - c.exprenv.cost() for c in node.children]
        node.probs = utils.softmax(np.array(action_values), alpha=self._alpha)
