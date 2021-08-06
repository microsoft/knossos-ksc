import numpy as np

from rlo.policy_value_model import RawPolicyValueEvaluation
from rlo import utils
from testutils import random_increasing


class RandomStateValueModel:
    """A mock ModelWrapper which gives random value estimates. Use with StateValueSoftmaxPolicy for rollout."""

    def __init__(self, random_source, num_time_heads: int):
        self._num_time_heads = num_time_heads
        self._rng = utils.rng(random_source)

    def evaluate_all_time_left(self, exprenvs):
        return np.array(
            [
                random_increasing(self._rng, e.cost(), self._num_time_heads)
                for e in exprenvs
            ]
        )

    def evaluate_loss(self, _batch):
        # Return sum over whole batch
        return self._rng.uniform(0, 1)

    def train_on_batch(self, _batch):
        # Return loss summed over whole batch; do not do any mutation
        return self._rng.uniform(0, 1)


class RandomPolicyModel:
    """A mock ModelWrapper which gives random values and action probabilities."""

    def __init__(self, random_source, num_rules, num_time_heads: int):
        self._num_rules = num_rules
        self._num_time_heads = num_time_heads
        self._rng = utils.rng(random_source)

    def evaluate_all_time_left(self, exprenvs):
        V = max(exprenv.expr.num_nodes for exprenv in exprenvs)
        R = self._num_rules
        return [
            RawPolicyValueEvaluation(
                values=random_increasing(self._rng, e.cost(), self._num_time_heads),
                log_probs=self._rng.normal(0, 1, (V, R, self._num_time_heads)),
            )
            for e in exprenvs
        ]
