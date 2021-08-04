import numpy as np

from rlo.agent import Agent, NodeNotReady
from rlo.rewrites import RuleSet
from rlo import utils


class PolicyNetPolicy(Agent):
    def __init__(self, rules: RuleSet, alpha, rng):
        super().__init__(rules)
        self._alpha = alpha
        self._rng = rng

    def choose_action(self, node):
        if node.probs is None:
            if node.has_evaluation():
                self.compute_probs(node)
            else:
                # Still waiting for evaluation; rollout will become pending.
                raise NodeNotReady
        return self._rng.choice(node.actions, p=node.probs)

    def compute_probs(self, node):
        # This will raise if any children don't yet have an evaluation (log_probs estimate).
        log_probs = [
            node.evaluation().log_probs[action.node_id][
                self._rules.id_for_rule(action.rule)
            ]
            for action in node.actions
        ]
        node.probs = utils.softmax(np.array(log_probs), alpha=self._alpha)
