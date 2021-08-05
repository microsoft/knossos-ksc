from typing import Any, Counter, Dict, Optional, Tuple, List, Union
from dataclasses import dataclass
import numpy as np

from rlo.agent import NodeNotReady
from rlo import analytics
from rlo.dataset import Dataset, PolicyNetDataset, RewriteId
from rlo.tf_model import ModelWrapper
from rlo.policy_net_policy import PolicyNetPolicy
from rlo.search_ops import (
    AbstractSearcher,
    SearcherState,
    min_cost_achieved,
)
from rlo.search_tree import (
    TransitionList,
    StateValueSearchTreeNode,
    ActionSearchTreeNode,
    Episode,
)
from rlo.node_evaluation_cache import NodeEvaluationCache
from rlo.state_value_softmax_policy import StateValueSoftmaxPolicy
from rlo import utils
from rlo.expression_util import ExprWithEnv
from rlo.agent import Agent


@dataclass
class AlphaSchedule:
    init_value: float
    max_value: float
    scale_factor_success: float
    scale_factor_fail: float


class SearchController:
    def __init__(self, max_num_episodes: int):
        self._max_num_episodes = max_num_episodes
        self._num_episodes = 0  # Could compute as sum(self._final_costs.values())
        self._final_costs: Counter[float] = Counter()

    def add_episode(self, episode: List[ExprWithEnv]) -> None:
        self._num_episodes += 1
        final_cost = min([e.cost() for e in episode])
        self._final_costs[final_cost] += 1
        self._add_episode(episode, final_cost)

    def _add_episode(self, episode: List[ExprWithEnv], final_cost) -> None:
        pass

    @property
    def min_cost(self):
        return min(self._final_costs.keys())

    @property
    def event_fields(self):
        return {"final_costs": self._final_costs}

    def has_sufficient(self):
        # An algorithm (rollout) using has_sufficient should also be checking has_capacity
        assert self._num_episodes <= self._max_num_episodes
        return self._num_episodes == self._max_num_episodes

    def has_capacity(self, num_extra: int) -> bool:
        return self._num_episodes + num_extra <= self._max_num_episodes

    @property
    def final_costs(self):
        return self._final_costs


class StopOnSuccessController(SearchController):
    """
    Stops early if num_positive_examples episodes have reached target_cost (if None => have
    improved on the starting expression).
    """

    def __init__(self, max_num_episodes, num_positive_examples, target_cost=None):
        super().__init__(max_num_episodes)
        self._target_cost = target_cost
        self._num_positive_examples = num_positive_examples
        self._num_successful = 0

    def _add_episode(self, episode, final_cost):
        is_successful = (
            (final_cost <= self._target_cost)
            if self._target_cost is not None
            else (final_cost < episode[0].cost())
        )
        if is_successful:
            self._num_successful += 1

    def has_sufficient(self):
        return (
            super().has_sufficient()
            or self._num_successful >= self._num_positive_examples
        )

    @property
    def event_fields(self):
        fields = {**super().event_fields, "target_cost": self._target_cost}
        if self._num_episodes > 0:
            fields["success_rate"] = self.success_rate
        return fields

    @property
    def success_rate(self):
        return np.float64(self._num_successful) / self._num_episodes


class Rollout(TransitionList):
    """
    A class representing a sequence of Transitions chosen probabilistically.

    Many Rollouts may share some or all of the same nodes, but each Rollout chooses exactly
    one successor=child node, so multiple Rollouts at the same node may yet diverge. When
    the rollout is finished, it returns Episode (a list of NodeAction), which is more convenient
    for downstream processing.
    """

    def __init__(
        self,
        initial_node: Union[StateValueSearchTreeNode, ActionSearchTreeNode],
        policy: Union[StateValueSoftmaxPolicy, Agent],
    ):
        super().__init__(initial_node)
        self._policy = policy

    def advance(self, cache: NodeEvaluationCache) -> Episode:
        """ Advances the current rollout

        Args:
            cache: an instance of cache object that can be passed to the policy

        Returns:
            a sequence of NodeAction if rollout is finished, otherwise raises NodeNotReady
        """
        while self.current_node.time_left > 0:
            next_steps = self._policy.try_sample_child(self.current_node, cache)
            if len(next_steps) == 0:  # No possible rewrites
                break
            self.extend(next_steps)
        # Rollout has finished
        return self.to_episode()

    @property
    def current_node(self):
        return self._steps[-1].next_node


class RolloutSearcher(AbstractSearcher):
    """
       A base class for ValueBasedRolloutSearcher and PolicyBasedRolloutSearcher.
    """

    def __init__(
        self,
        max_num_episodes: int,
        num_positive_examples: int,
        alpha_test: float,
        batch_size: int = 16,
        **kwargs
    ):
        super().__init__(**kwargs)
        self._alpha_test = alpha_test
        self._batch_size = batch_size
        self._max_num_episodes = max_num_episodes
        self._num_positive_examples = num_positive_examples

    def _eval_search(
        self,
        model_wrapper: ModelWrapper,
        random_source,
        exprenv: ExprWithEnv,
        untrained_model=False,
    ) -> Tuple[List[Episode], NodeEvaluationCache]:
        alpha = 0.0 if untrained_model else self._alpha_test

        ctrl = SearchController(self._max_num_episodes)
        return self._search(
            model_wrapper,
            utils.seed(random_source),
            exprenv,
            search_ctrl=ctrl,
            alpha=alpha,
        )

    @staticmethod
    def _search_internal(
        seed: int,
        start_posn: Union[StateValueSearchTreeNode, ActionSearchTreeNode],
        policy: Union[StateValueSoftmaxPolicy, PolicyNetPolicy],
        cache: NodeEvaluationCache,
        search_ctrl: SearchController,
    ) -> Tuple[List[Episode], NodeEvaluationCache, Dict[str, Any]]:
        """
        Performs a number of rollouts, controlled by search_ctrl, batching model evaluations together and hashing/caching for speed.
        """
        finished_rollouts: List[Episode] = []

        # Keep track of all the Rollouts that are in progress.

        # Rollouts waiting for values to be computed in the next minibatch;
        # A rollout enters this queue only when all the SearchTreeNodes it needs have been enqueued into posns_waiting_for_value.
        rollouts_waiting_for_probs: List[Rollout] = []
        # Those which we can advance (we do so only in main loop).
        rollouts_to_advance: List[Rollout] = []

        # Tuning param: e.g. we need to evaluate all the child SearchTreeNodes of the start node before any rollout can proceed,
        # but if there are <batch_size children we will never process a minibatch to evaluate any; so, trigger this on the total
        # number of pending rollouts.
        max_rollouts_waiting = 100

        while True:
            while len(rollouts_to_advance) > 0:
                rollouts, rollouts_to_advance = rollouts_to_advance, []
                for r in rollouts:
                    try:
                        steps = r.advance(cache)
                        current_episode = [s.node.exprenv for s in steps]
                        search_ctrl.add_episode(current_episode)
                        finished_rollouts.append(steps)
                    except NodeNotReady:
                        rollouts_waiting_for_probs.append(r)

            if (
                not cache.has_sufficient_posns_to_eval()
                and len(rollouts_waiting_for_probs)
                < max_rollouts_waiting  # Tuning param
                and not search_ctrl.has_sufficient()
                and search_ctrl.has_capacity(len(rollouts_waiting_for_probs) + 1)
            ):  # Room for at least one more
                rollouts_to_advance.append(Rollout(start_posn, policy))
            elif len(rollouts_waiting_for_probs) > 0:
                # If we have sufficient rollouts already, we end up here, so that we can tidy up.
                # We must finish all Rollouts that we start, to avoid a bias whereby Rollouts visiting
                # popular (previously seen) Positions would tend to overtake those stalled at new Positions.
                analytics.event(
                    "rollout_iter",
                    verbosity=1,
                    batches=cache.num_batches,
                    posns_evaluated=cache.total_posns_evaluated,
                    pending_posns=cache.eval_queue_length,
                    pending_rollouts=len(rollouts_waiting_for_probs),
                    **search_ctrl.event_fields
                )
                # Evaluate the pending positions
                cache.process_batches()
                # All rollouts that were in rollouts_waiting_for_values, must now have all the _evaluation (value estimate) they need.
                for rollout in rollouts_waiting_for_probs:
                    # No need to compute probs more than once
                    if rollout.current_node.probs is None:
                        # Many rollouts may have been blocked at the same point
                        policy.compute_probs(rollout.current_node)
                rollouts_to_advance.extend(rollouts_waiting_for_probs)
                rollouts_waiting_for_probs.clear()
            else:  # we know len(rollouts_to_advance) == 0
                break

        log_entries = {
            "seed": seed,
            **cache.rollout_end_log_entries(),
            **search_ctrl.event_fields,
        }
        # Note that the rollouts may not include all the nodes in the cache, at least for ValueBasedRolloutSearcher:
        # the cache includes sibling nodes used to generate policy probabilities even if no rollout ever chooses them.
        # This means that the min-cost found in the rollouts may not be as low==good as the
        # cache.best_cost_seen() or the min-cost among the start_posn.all_nodes. (The latter two should match.)
        return finished_rollouts, cache, log_entries

    def training_search(self, model_wrapper, seed, exprenv, alpha, **kwargs):
        with analytics.Scope(alpha=alpha):
            # Extra args passed on to _search
            return super().training_search(
                model_wrapper, seed, exprenv, alpha=alpha, **kwargs
            )


class ValueBasedRolloutSearcher(RolloutSearcher):
    """ A class for performing rollouts based on a value function. The probability of taking action a at
        state (time_left, expr) can be described as follows:

        P(a | time_left, expr) ∝ exp(alpha * (cost(expr) - cost(expr') + V(time_left - 1, expr'))),

        where V(t, e) is the value of the state (t, e) and expr' := T_a(expr) is the new expression after
        taking action a on expression expr; alpha is the (inverse) temperature parameter.

        Inverse temperature alpha controls the softmax used to compute probability distributions;
        alpha=0.0 thus means to ignore the value estimates obtained from the model and use uniform probabilities;
        higher alpha comes closer to just taking the max.

        See also: StateValueSoftmaxPolicy, StateValueSearchTreeNode
    """

    def __init__(
        self,
        *args,
        alpha_schedule: AlphaSchedule = AlphaSchedule(1.0, 10.0, 1.1, 1.0),
        **kwargs
    ):
        self._alpha_schedule = alpha_schedule
        super().__init__(*args, **kwargs)

    def _search_impl(  # type: ignore[override]
        self,
        model_wrapper: ModelWrapper,
        seed: int,
        exprenv: ExprWithEnv,
        search_ctrl: SearchController,
        alpha: float,
        **_kwargs
    ) -> Tuple[List[Episode], NodeEvaluationCache, Dict]:
        """ Performs value-based rollout search. """
        rng = utils.rng(seed)
        cache = NodeEvaluationCache(
            model_wrapper,
            self._batch_size,
            search_tree_node_class=StateValueSearchTreeNode,
        )
        policy = StateValueSoftmaxPolicy(self._rules, alpha, rng)
        # Enqueuing here slightly changes the behavior of StateValueSoftmaxPolicy.
        # The root node will have it's value computed even tho that's not really necessary.
        start_posn = cache.get_node(exprenv, self._simulation_depth)
        return self._search_internal(seed, start_posn, policy, cache, search_ctrl)

    def _build_dataset(self, seed, episodes, target_cost, **_kwargs):
        ep_costs = [
            min(node.exprenv.cost() for node, _action in episode)
            for episode in episodes
        ]
        # If all episodes found the target_cost,
        if target_cost is None:
            target_cost = utils.single_elem(
                set([ep[0].node.exprenv for ep in episodes])
            ).cost()
        if all(ep_cost == target_cost for ep_cost in ep_costs):
            # All rollouts were equally (un)successful and failed to produce anything new to learn.
            # Possibly we should reduce alpha to explore more (?)
            return None
        return super()._build_dataset(seed, episodes)

    def training_search(self, model_wrapper, seed, exprenv, target_cost, alpha):
        ctrl = StopOnSuccessController(
            self._max_num_episodes, self._num_positive_examples, target_cost
        )
        return super().training_search(
            model_wrapper,
            seed,
            exprenv,
            search_ctrl=ctrl,
            target_cost=target_cost,
            alpha=alpha,
        )  # Extra args passed onto _search

    def initial_state(self):
        return AlphaSchedulingRun(self, self._alpha_schedule)


class PolicyBasedRolloutSearcher(RolloutSearcher):
    """ A class for performing rollouts based on a policy function. The probability of taking action a at
        state (time_left, expr) is described as follows:

        P(a | time_left, expr) ∝ exp(alpha * log_prob(action, time_left, expr))

        where log_prob is produced by an appropriate policy model.

        Inverse temperature alpha should be set to 1.0 during training. It can be set to a different value
        at evaluation to make the policy behave more / less stochastic.

        See also: PolicyNetPolicy, ActionSearchTreeNode
    """

    def _search_impl(  # type: ignore[override]
        self,
        model_wrapper: ModelWrapper,
        seed: int,
        exprenv: ExprWithEnv,
        search_ctrl,
        alpha: float,
        **_kwargs
    ) -> Tuple[List[Episode], NodeEvaluationCache, Dict]:
        """ Performs policy-based rollout search. """
        rng = utils.rng(seed)
        cache = NodeEvaluationCache(
            model_wrapper, self._batch_size, search_tree_node_class=ActionSearchTreeNode
        )
        policy = PolicyNetPolicy(self._rules, alpha, rng)
        # Enqueuing here slightly changes the behavior of StateValueSoftmaxPolicy
        start_posn = cache.get_node(exprenv, self._simulation_depth)
        return self._search_internal(seed, start_posn, policy, cache, search_ctrl)

    @staticmethod
    def get_empty_dataset():
        return PolicyNetDataset()

    def _build_dataset(self, seed, episodes, **_kwargs):
        """ Convert the search results from search into a PolicyNetDataset.
            The dataset consists of (time_left, expr, value, action, advantage)-quintuples.
        """
        # First construct an ordinary (time, expr, value) dataset through the base class
        selected_episodes = self._preprocess_episodes(seed, episodes)
        value_dataset = super()._build_state_value_dataset(selected_episodes)
        dataset = PolicyNetDataset(value_dataset)

        # Add every node except the last (time=0) from every episode
        for episode in selected_episodes:
            final_cost = episode[-1][0].exprenv.cost()
            for node, action in episode[:-1]:
                # sum of future rewards
                reward_to_go = float(node.exprenv.cost() - final_cost)
                # advantage is the difference between the reward_to_go and the value estimate.
                advantage = reward_to_go - node.evaluation().value
                dataset.add_point(
                    node.time_left,
                    node.exprenv,
                    RewriteId(action.node_id, self._rules.id_for_rule(action.rule)),
                    advantage,
                )
        return dataset

    def training_search(  # type: ignore[override]
        self, model_wrapper: ModelWrapper, seed, exprenv: ExprWithEnv, target_cost
    ):
        # Use StopOnSuccessController but with num_positive_examples == max_num_episodes
        # target_cost is used in the controller to compute the success_rate metric
        ctrl = StopOnSuccessController(
            self._max_num_episodes, self._max_num_episodes, target_cost
        )
        # target_cost is provided but ignored by _search / _build_dataset; it is only for logging
        # in base class training_search()
        return super().training_search(
            model_wrapper,
            seed,
            exprenv,
            target_cost=target_cost,
            search_ctrl=ctrl,
            alpha=1.0,
        )

    def initial_state(self):
        return SearcherState(self)


class AlphaSchedulingRun(SearcherState):
    def __init__(
        self, rollout_searcher: ValueBasedRolloutSearcher, alpha_schedule: AlphaSchedule
    ):
        super().__init__(rollout_searcher)
        self._alphas: Dict[ExprWithEnv, float] = {}
        self._alpha_schedule = alpha_schedule

    def get_state_for_expr(self, exprenv: ExprWithEnv) -> Dict[str, Any]:
        return {
            "alpha": self._alphas.get(exprenv, self._alpha_schedule.init_value),
            **super().get_state_for_expr(exprenv),
        }

    # Note this overrides superclass version
    def update_state_for_expr(self, exprenv: ExprWithEnv, dataset: Optional[Dataset]):
        min_cost = min_cost_achieved(exprenv, dataset)
        alpha = self._alphas.get(exprenv, self._alpha_schedule.init_value)

        target_cost = self._target_cost.get(exprenv, None)
        success = (
            (min_cost < exprenv.cost())
            if target_cost is None
            else (min_cost <= target_cost)
        )
        # Generally (according to parameters), we decrease alpha upon failure, whereas upon success we increase
        # alpha to follow the model more (concentrate effort where it's less certain?).
        # Increase quickly if we've reduced it to something silly (near uniform), but keep to reasonable bound
        self._alphas[exprenv] = min(
            self._alpha_schedule.max_value,
            max(
                self._alpha_schedule.init_value,
                alpha * self._alpha_schedule.scale_factor_success,
            )
            if success
            else (alpha * self._alpha_schedule.scale_factor_fail),
        )
        analytics.event(
            "alpha_schedule",
            action=("inc" if success else "dec"),
            old_alpha=alpha,
            new_alpha=self._alphas[exprenv],
        )
        if dataset is None:
            # This return value from training_search indicates all rollouts were successful (but failed to find anything new).
            # So assume nothing to learn, and skip superclass recording.
            analytics.event("skip_all_successes")
            return

        # Call superclass to update the state.
        super().update_state_for_expr(exprenv, dataset)
