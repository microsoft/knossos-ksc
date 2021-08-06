# mypy: ignore-errors
from collections import defaultdict
from typing import (
    Iterable,
    List,
    Optional,
    Tuple,
    Union,
    Dict,
    DefaultDict,
    Sequence,
    Mapping,
    NamedTuple,
)

import numpy as np

from rlo import analytics
from rlo.cum_sequence import CumMaxSequence
from rlo.expression_util import ExprWithEnv
from rlo.utils import group_snds_by_fst


def evaluation_to_list(evaluation):
    from rlo.policy_value_model import RawPolicyValueEvaluation

    if isinstance(evaluation, RawPolicyValueEvaluation):
        return evaluation.values.tolist()
    return evaluation.tolist()


class RawValueExample(NamedTuple):
    exprenv: ExprWithEnv
    values: np.ndarray  # 1D, indexed by time_left


class StateValueDataset:
    """
    A dataset of state-values with items of the form (expression, values) where
    values is a (monotonically increasing) sequence of values for different time-left
    values.

    If multiple examples with same expression are added to this dataset, this
    class will update the sequence of values to reflect the cumulative maximum of
    the values in each example passed. For example:
    
        Example 1: (expr1, [(1, 5), (2, 7), (5, 11)])
        Example 2: (expr1, [(1, 3), (2, 8), (3, 9)])
        Final entry in dataset:
                   (expr1, CumMaxSeq([(0,0), (1, 5), (2, 8), (3, 9), (5, 11)]))
    """

    def __init__(self):
        self._max_values: Dict[ExprWithEnv, CumMaxSequence] = {}
        self._max_time: Dict[ExprWithEnv, int] = {}

    def add_expr(
        self, exprenv: ExprWithEnv, items: Iterable[Tuple[int, float]]
    ) -> bool:
        """
        Add multiple (time_left, value) points for a single new expression.

        Return a bool indicating whether any of the values in the cumulative sequence
        for the given expression had been updated.
        """
        items = tuple((t, v) for t, v in items if t > 0)
        if len(items) == 0:
            return []
        if exprenv in self._max_values:
            raise ValueError("Expression already present: {}".format(exprenv))
        self._max_time[exprenv] = max(t for t, _v in items)
        max_values = CumMaxSequence()
        max_values.update(0, 0.0)
        self._max_values[exprenv] = max_values
        return any([max_values.update(i, v) for i, v in items])

    def get_examples(self, max_depth: Optional[int] = None) -> List[RawValueExample]:
        """
        Get a deterministically ordered list of (expression, values).

        `values[0]` is the value at time-left 0, and these go up to the highest time_left for which a value
        has been added for that expression: `len(values) == max_time_left + 1`.
        """
        # Make a deterministic ordering of datapoints.
        res = [
            RawValueExample(
                e,
                np.array(
                    v.to_list(
                        (self._max_time[e] if max_depth is None else max_depth) + 1
                    )
                ),
            )
            for e, v in sorted(self._max_values.items(), key=lambda tup: str(tup[0]))
        ]
        assert all(len(vs) > 1 for _, vs in res)
        return res

    @property
    def exprenvs(self) -> Iterable[ExprWithEnv]:
        return self._max_values.keys()

    def get_value(
        self, exprenv: ExprWithEnv, time_left: Optional[int] = None
    ) -> Optional[float]:
        """ Returns the greatest value known for the specified ExprWithEnv at or less than the specified time_left.
            (If time_left is None, returns the greatest value at any known time-left). """
        if exprenv in self._max_values:
            return self._max_values[exprenv][
                float("inf") if time_left is None else time_left
            ]
        return None

    def merge_dataset(self, other) -> None:
        """ Merges other into this dataset
        """
        for exprenv, cum_seq in other._max_values.items():
            if exprenv not in self._max_values:
                # The common case in most scenarios. Use the same CumMaxSequence as we are treating as immutable.
                self._max_values[exprenv] = cum_seq
                self._max_time[exprenv] = other._max_time[exprenv]
            else:
                # Merge values with existing.  CumMaxSequences must be immutable once in a dataset, as
                # the same CumMaxSequence object may be held by multiple datasets.  Hence, we must copy.
                # Note: since we know the new items come *from* another CumMaxSequence, then they are sorted,
                # so could merge in O(n) time rather than this O(n.log(n)) if this becomes important.
                max_values = self._max_values[exprenv].copy()
                if max_values.update_from_seq(cum_seq):
                    # New values increased some existing bounds, so keep
                    self._max_values[exprenv] = max_values
                self._max_time[exprenv] = max(
                    self._max_time[exprenv], other._max_time[exprenv]
                )

    def copy_sequence_from(
        self, other: "StateValueDataset", exprenv: ExprWithEnv
    ) -> bool:
        """ Replaces the values for one expression in this dataset with the values
            for the same expression from another dataset. Does not update the max_time in this dataset.
            Returns true if any values were increased; throws if any were decreased. """
        old_seq = self._max_values[exprenv]
        new_seq = other._max_values[exprenv]
        if old_seq is new_seq:
            # Common case, where one dataset was formed by merging from the other,
            # with no other values for the same expr
            return False
        # Note, we could truncate the other sequence to our max_time, possibly saving storage.
        # I don't expect that to make much difference.
        self._max_values[exprenv] = new_seq
        # If we didn't want to measure/check whether any values had increased/decreased,
        # we could return now (faster!).
        for t, v in new_seq.critical_values.items():
            if t > self._max_time[exprenv]:
                break
            if v < old_seq[t]:
                raise ValueError("At time {t}, old value {old[t]} decreased to {v}")
            if v > old_seq[t]:
                return True
        return False

    def num_expressions(self) -> int:
        return len(self._max_values)

    def num_points(self) -> int:
        return sum(self._max_time.values())

    def has_points(self) -> bool:
        return self.num_expressions() > 0

    def log_empirical_predicted(
        self, tl_cache: Mapping[ExprWithEnv, np.ndarray]
    ) -> None:
        """
        Given a dict from ExprWithEnv to [predicted value at time 0, ... at time 1, ...], output logs
        comparing these predicted values against the empirical values for all time-left for points in this dataset.
        """
        for e in set(self._max_values).intersection(tl_cache.keys()):
            max_tl = self._max_time[e]
            seq = self._max_values[e].to_list(max_tl + 1)[1:]

            analytics.event(
                "plot_value_comparison",
                verbosity=1,
                datapoint=e.expr,
                predicted_value=evaluation_to_list(tl_cache[e])[1 : max_tl + 1],
                empirical_value=seq,
            )

    @staticmethod
    def build_from_triples(points: Sequence[Tuple[int, ExprWithEnv, float]]):
        res = StateValueDataset()
        for exprenv, pts in group_snds_by_fst(
            (e, (t, v)) for t, e, v in points
        ).items():
            res.add_expr(exprenv, pts)
        return res


class RewriteId(NamedTuple):
    node_id: int
    rule_id: int


class RewriteAdvantages(NamedTuple):
    rewrite_id: RewriteId
    advantages: np.ndarray  # 1D, indexed by time_left


class RawPolicyValueTrainingExample(NamedTuple):
    exprenv: ExprWithEnv
    values: np.ndarray  # 1D, indexed by time_left
    action_advantages: Sequence[RewriteAdvantages]


class PolicyNetDataset:
    """
    A Dataset that produces triplets (expression, values, sequence of (action, advantages) pairs).

    Similarly to StateValueDataset, values is a sequence containing empirical values for different
    time_lefts. Each triplet can contain varying number of (action, advantages) pairs, in which
    advantages is a sequence containing empirically observed advantages for the action and different
    time_lefts, similarly to values. Here action is a particular action taken at a point in a
    rollout.  advantage is the reward-to-go minus the previous value estimate of state
    (time_left, expr) and reward-to-go is the sum of future rewards in the rollout following
    the action.

    This class separately maintains a StateValueDataset instance (value_dataset) and a dict keyed
    by expression, action, and time_left that contains a list of advantages. The inner-most list
    stores all the (different) advantages observed for the (expr, action, time_left) combination.
    """

    def __init__(self, value_dataset: Optional[StateValueDataset] = None):
        """
        Args:
            value_dataset: a StateValueDataset instance created by maxing over episodes.
        """
        self._value_dataset = (
            value_dataset if value_dataset is not None else StateValueDataset()
        )
        # This dictionary stores for each expression and RewriteId, a list of dicts in which
        # each dict is keyed by time_left and stores the advantage.
        self._dict: DefaultDict[
            ExprWithEnv, DefaultDict[RewriteId, DefaultDict[int, List[float]]]
        ] = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))

    def add_point(self, t, e, rewrite, adv):
        """ Adds a point.

        Args:
            t: time left
            e: expression
            reward_to_go: reward-to-go (the sum of future rewards), which lower bounds the value
                of (t, e).
            rewrite: a rewrite
            adv: advantage (rewards-to-go minus previous value estimate)
        """
        assert isinstance(t, int)
        assert isinstance(e, ExprWithEnv)
        assert isinstance(rewrite, RewriteId)
        assert isinstance(adv, float)
        self._dict[e][rewrite][t].append(adv)

    def get_examples(self) -> List[RawPolicyValueTrainingExample]:
        # mapping from expression to target value sequence
        value_dict: Dict[ExprWithEnv, Sequence[float]] = dict(
            self._value_dataset.get_examples()
        )
        assert len(value_dict) == len(self._dict)
        dataset_list = []
        for e, advs_by_action in self._dict.items():
            action_advs = []
            for action, advs_by_time in advs_by_action.items():
                num_points = max(len(v) for v in advs_by_time.values())
                for d in range(num_points):
                    # Each element in action_advs should contain at most one instance of each time_left
                    tvs = [
                        (t, advs[d])
                        for t, advs in advs_by_time.items()
                        if d < len(advs)
                    ]
                    size = max(tvs, key=lambda tv: tv[0])[0] + 1
                    # zero advantage does not contribute to the loss / gradient
                    seq = np.zeros((size,), dtype=np.float32)
                    for t, adv in tvs:
                        seq[t] = adv
                    action_advs.append((action, seq))
            assert (
                e in value_dict
            ), f"Cannot find {e} in value dataset (size: {len(value_dict)}) keys={[str(e) for e in value_dict.keys()]}"
            dataset_list.append(
                RawPolicyValueTrainingExample(
                    e, value_dict[e], sorted(action_advs, key=lambda ra: ra[0])
                )
            )
        analytics.event(
            "dataset_get_points",
            dataset_type="PolicyNetDataset",
            size=len(dataset_list),
            num_expressions=len(self._dict),
            num_expr_rewrites=sum(len(v) for v in self._dict.values()),
        )
        return dataset_list

    def merge_dataset(self, other):
        self._value_dataset.merge_dataset(other._value_dataset)
        for e, advs_by_action in other._dict.items():
            for action, advs_by_time in advs_by_action.items():
                for t, advs in advs_by_time.items():
                    for adv in advs:
                        self.add_point(t, e, action, adv)

    def num_expressions(self) -> int:
        return self._value_dataset.num_expressions()

    def has_points(self) -> bool:
        return len(self._dict) > 0

    def num_points(self) -> int:
        return sum(
            len(advs_by_time)
            for advs_by_action in self._dict.values()
            for advs_by_time in advs_by_action.values()
        )

    def log_empirical_predicted(self, tl_cache) -> None:
        self._value_dataset.log_empirical_predicted(tl_cache)

    def get_value(
        self, exprenv: ExprWithEnv, time_left: Optional[int] = None
    ) -> Optional[float]:
        return self._value_dataset.get_value(exprenv, time_left)


# Dataset type for argument / return type declarations.
Dataset = Union[StateValueDataset, PolicyNetDataset]
