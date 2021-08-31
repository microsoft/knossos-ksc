from abc import ABC, abstractmethod


from rlo.node_evaluation_cache import NodeEvaluationCache
from rlo.rewrites import RuleMatcher
from rlo.search_tree import ActionSearchTreeNode, Transition, NodeAction


class NodeNotReady(Exception):
    pass


class Agent(ABC):
    """ A base class for experts and a user-input-based agent. It has the same interface
    as StateValueSoftmaxPolicy so that in the future we can unify them in RolloutSearcher (#19824)
    """

    def __init__(self, rules: RuleMatcher):
        self._rules = rules
        self._steps = 0

    def try_sample_child(self, node, cache):
        """ Takes a node and (depending on the policy implemented in choose_action) returns
        (1) an empty list if there is no more action to be taken, or
        (2) a single element list containing a Transition

        raises NodeNotReady if evaluation is needed
        """
        assert node.time_left > 0
        if node.actions is None:
            # This happens the first time a rollout goes through each node. Multiple nodes
            # for the same expression (at different time-left) will compute repeatedly.
            node.actions = list(self._rules.get_all_rewrites(node.exprenv))
        if len(node.actions) == 0:
            # No possible next steps, i.e. no applicable rules
            return []
        action = self.choose_action(node)
        if action is None:
            return []
        child_node = cache.get_node(action.apply(node.exprenv), node.time_left - 1)
        node.set_child(action, child_node)
        self._steps += 1
        return [Transition(action, child_node)]

    @abstractmethod
    def choose_action(self, node):
        """ Returns the next action, None if no action to be taken, or raises NodeNotReady if evaluation is needed
        """

    def optimize(
        self, expr, truncate=False, time_left=float("inf"),
    ):
        """ Optimize an expression greedily and returns a sequence of NodeAction

        Args:
            expr: expression to be optimized
            truncate: if true, return a truncated sequence up to the
            minimum cost expression.
        """
        from rlo.rollouts import Rollout

        cache = NodeEvaluationCache(
            model_wrapper=None,
            batch_size=1,
            search_tree_node_class=ActionSearchTreeNode,
        )
        episode = Rollout(cache.get_node(expr, time_left), self).advance(cache)
        assert episode is not None
        if truncate:
            _, index = min((n.exprenv.cost(), i) for i, (n, a) in enumerate(episode))
            episode = episode[: index + 1]
            #  we truncated the sequence, the last action should be None now
            episode[-1] = NodeAction(episode[-1].node, None)
        return episode
