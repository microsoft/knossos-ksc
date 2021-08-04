from collections import Counter
from dataclasses import dataclass
from typing import (
    Any,
    Callable,
    Dict,
    Iterable,
    NamedTuple,
    List,
    Tuple,
    Union,
    Optional,
)
from abc import ABC, abstractmethod

from rlo import analytics
from rlo.best_results import best_cost_for_exp
from rlo.clustering import cluster_episodes, sample_clusters
from rlo.costs import compare_epsilon
from rlo.dataset import Dataset, StateValueDataset
from rlo.expression_util import ExprWithEnv, NamedExprWithEnv
from rlo.node_evaluation_cache import NodeEvaluationCache
from rlo import rewrites
from rlo.search_tree import NodeAction, Episode
from rlo import utils
from rlo.tf_model import ModelWrapper


class ActionCost(NamedTuple):
    action: Union[Tuple[int, str], Tuple[None, None]]
    cost: float


class ExprRewriteSeqCost(NamedTuple):
    init_expr: Union[str, ExprWithEnv]
    rewrite_seq_costs: List[ActionCost]


def episode_to_log(episode: Episode) -> ExprRewriteSeqCost:
    """
    Convert an Episode to another representation for logging, namely
    (initial expression, list of (node_id, rule_name, cost of expression before rewrite))
    """
    episode = list(episode)
    # Discard any final action (performed on the last expr in the episode)
    return ExprRewriteSeqCost(
        str(episode[0].node.exprenv.expr),
        [
            ActionCost((s.action.node_id, s.action.rule_name), s.node.exprenv.cost())
            for s in episode[:-1]
            if s.action is not None
        ]
        + [ActionCost((None, None), episode[-1].node.exprenv.cost())],
    )


def best_episode_to_log(episodes: Iterable[List[NodeAction]]) -> ExprRewriteSeqCost:
    """Returns an (init_expr, [(action, cost) ... sequence]) that
    has the minimal final cost and (picking the shortest one when there are ties).
    """
    episode, idx = min(
        ((episode, i) for episode in episodes for i in range(len(episode))),
        key=utils.star(lambda episode, idx: (episode[idx].node.exprenv.cost(), idx)),
    )
    return episode_to_log(episode[: idx + 1])


@dataclass(frozen=True)
class SearchSummary:
    expr_name: str
    start_cost: float
    final_cost: float
    best_episode: ExprRewriteSeqCost

    @staticmethod
    def from_episodes(
        expr_name: str, start_cost: float, episodes: Iterable[List[NodeAction]]
    ):
        best_ep = best_episode_to_log(episodes)
        final_cost = best_ep.rewrite_seq_costs[-1].cost
        return SearchSummary(expr_name, start_cost, final_cost, best_ep)


class AbstractSearcher(ABC):
    """
    A class for performing actions involving searching: evaluation and building datasets.
    """

    def __init__(
        self,
        rules: rewrites.RuleSet,
        simulation_depth: int,
        maxing: Callable,
        num_episode_clusters: int = 5,
    ):
        self._rules = rules
        self._num_episode_clusters = num_episode_clusters
        self._simulation_depth = simulation_depth
        self._maxing_algorithm = maxing

    def eval(
        self,
        model_wrapper: ModelWrapper,
        random_source,
        named_exprenv: NamedExprWithEnv,
        untrained_model: bool = False,
    ) -> SearchSummary:
        """ Searches in evaluation mode and returns the result """
        expr_name, exprenv = named_exprenv
        start_cost = exprenv.cost()
        with analytics.Scope(eval_expr=expr_name, eval_exp_cost=start_cost):
            episodes, cache = self._eval_search(
                model_wrapper, random_source, exprenv, untrained_model
            )
            search_result = SearchSummary.from_episodes(expr_name, start_cost, episodes)
            min_cost = cache.best_cost_seen
            best_cost = best_cost_for_exp(exprenv, self._rules.rules_name)
            if best_cost and min_cost < best_cost.cost - compare_epsilon:
                # The episode/rewrite-sequence is logged out as part of the best_ep in the log,
                # But this makes it easier to find
                raise ValueError(
                    f"Oracle cost {best_cost.cost} beaten with cost {min_cost}, best_ep {search_result.best_episode}"
                )
            return search_result

    def _eval_search(
        self,
        model_wrapper: ModelWrapper,
        random_source,
        exprenv: ExprWithEnv,
        untrained_model: bool = False,
    ) -> Tuple[List[Episode], NodeEvaluationCache]:
        """The default implementation ignores `untrained_model`, which is only used by RolloutSearcher.

        Subclasses can override to implement a behaviour (e.g., handling untrained model or
        deciding how to stop) different from training search.
        """
        del untrained_model
        return self._search(model_wrapper, random_source, exprenv)

    @abstractmethod
    def _search_impl(
        self,
        model_wrapper: ModelWrapper,
        random_source,
        exprenv: ExprWithEnv,
        *args,
        **kwargs,
    ) -> Tuple[List[Episode], NodeEvaluationCache, Dict[str, Any]]:
        """Subclasses should override, taking whatever parameters are required,
        and return a triple of episodes, NodeEvaluationCache, and a dictionary containing
        entries to be logged.
        """

    def _search(self, *args, **kwargs) -> Tuple[List[Episode], NodeEvaluationCache]:
        episodes, cache, log_entries = self._search_impl(*args, **kwargs)

        best_ep_log = best_episode_to_log(episodes)
        analytics.event(
            "rollout_end",
            best_ep=best_ep_log,
            episode_lengths=Counter([len(e) for e in episodes]),
            **log_entries,
        )
        return episodes, cache

    def _preprocess_episodes(self, seed: int, episodes: List[Episode]) -> List[Episode]:
        self._maxing_algorithm(episodes, self._simulation_depth)
        if self._num_episode_clusters > 0:
            clusters = cluster_episodes(episodes, seed, self._num_episode_clusters)
            # Ignore error because mypy does not believe that dict.values() returns an Iterator
            return sample_clusters(clusters.values(), utils.rng(seed), num_episodes=100)  # type: ignore[arg-type]
        else:
            return episodes

    @staticmethod
    def _build_state_value_dataset(episodes: List[Episode]) -> StateValueDataset:
        dataset = StateValueDataset()
        nodes_and_steps_lefts_by_expr = [
            (n.exprenv, (n, len(episode) - step))
            for episode in episodes
            for step, (n, _action) in enumerate(episode)
        ]
        for nodes_and_steps in utils.group_snds_by_fst(
            nodes_and_steps_lefts_by_expr
        ).values():
            nodes, steps = zip(*nodes_and_steps)
            # <nodes> all have the same Expression, so we expect the maxing algorithm to have filled in every node with the same min_cost_in_steps
            node = max(nodes, key=lambda node: node._time_left)
            max_steps = max(steps)
            dataset.add_expr(
                node.exprenv,
                (
                    (t, node.exprenv.cost() - node.min_cost_in_steps(t))
                    for t in range(1, max_steps)
                ),
            )
        return dataset

    def _build_dataset(
        self, seed: int, episodes: List[Episode], **_kwargs
    ) -> Optional[Dataset]:
        """Convert the results of a search into a Dataset.
        Subclasses may override to deal with their own form of search results and to construct
        other forms of Dataset, or None.
        This implementation deals with episodes, and always returns a non-None StateValueDataset.
        _kwargs will contain any extra parameters provided by the SearcherState, but is ignored in this implementation.
        """
        selected_episodes = self._preprocess_episodes(seed, episodes)
        return self._build_state_value_dataset(selected_episodes)

    @staticmethod
    def get_empty_dataset():
        """ Returns an empty dataset as a initial state for merge_dataset(). """
        return StateValueDataset()

    def initial_state(self):
        """Subclasses should override to return an appropriate SearcherState.
        The default is to return a standard SearcherState which stores only target-cost."""
        return SearcherState(self)

    def training_search(
        self, model_wrapper: ModelWrapper, seed, exprenv, **kwargs
    ) -> Optional[Dataset]:
        """Does a search, and builds a dataset from the search results,
            as can be passed to SearcherState.update_state_for_expr.
        This is the remote function, sent to a Worker via a RunOnGPU object; this means that
        for Ray to work, the arguments cannot contain references to any mutable data, e.g. the SearcherState.
        Any extra values from the SearcherState may be passed into kwargs; these will be passed onto the
        _search and _build_dataset methods. In this (super)class the only kwarg is target_cost,
        but subclasses may add extra fields e.g. alpha.
        """
        with analytics.Scope(target_cost=kwargs["target_cost"]):
            analytics.event("search")
            search_res, cache = self._search(model_wrapper, seed, exprenv, **kwargs)
        # We turn the search results into a dataset during the same call to the remote machine.
        # Other possible strategies include turning search_results into dataset on the headnode (not using model),
        # or making a second remote call to turn them into a dataset, if required (better but more complicated).
        dataset = self._build_dataset(seed, search_res, **kwargs)
        if dataset is not None:
            dataset.log_empirical_predicted(cache.tl_cache)
        return dataset


def min_cost_achieved(exprenv: ExprWithEnv, dataset: Optional[Dataset]) -> float:
    # Deal with empty or no dataset, if no rewrites applied or no optimization found.
    # Any non-empty dataset should contain the starting expression.
    return (
        exprenv.cost()
        if dataset is None or not dataset.has_points()
        else exprenv.cost() - dataset.get_value(exprenv)
    )


class SearcherState:
    """Stores mutable state, e.g. hyperparameters such as alpha, particular to
    the search algorithm being used for training, across multiple generations.
    The default implementation tracks only the 'target_cost' (the best ever seen
    for each starting expression); this is actually only used for Rollout, but it's
    interesting to have around."""

    def __init__(self, searcher: AbstractSearcher):
        self._searcher = searcher
        self._target_cost: Dict[ExprWithEnv, float] = {}

    def get_state_for_expr(self, exprenv: ExprWithEnv) -> Dict[str, Any]:
        """Return a dict of kwargs to pass to the Searcher's training_search method
        (and thus _search and _build_dataset).
        Subclasses may override to pass any additional kwargs.
        The default implementation returns only the target_cost."""
        return {"target_cost": self._target_cost.get(exprenv, None)}

    def update_state_for_expr(
        self, exprenv: ExprWithEnv, dataset: Optional[Dataset]
    ) -> None:
        """Args:
           expr - a starting expression
           dataset - the return value from training_search
        Updates the state.
        """
        self._target_cost[exprenv] = min(
            min_cost_achieved(exprenv, dataset),
            self._target_cost.get(exprenv, exprenv.cost()),
        )
