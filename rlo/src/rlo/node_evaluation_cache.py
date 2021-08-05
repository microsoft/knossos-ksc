from typing import Dict, Generic, Hashable, List, Tuple, Type, Any, Mapping, Sequence

import numpy as np

from rlo.expression_util import ExprWithEnv
from rlo import utils
from rlo.search_tree import NodeType
from rlo.tf_model import ModelWrapper


class NodeEvaluationCache(Generic[NodeType]):
    """ This class stores both node and model evaluation cache used in Searchers.
    """

    def __init__(
        self,
        model_wrapper: ModelWrapper,
        batch_size: int,
        search_tree_node_class: Type[NodeType],
    ):
        self._model_wrapper = model_wrapper
        self._batch_size = batch_size
        self._search_tree_node_class = search_tree_node_class
        # The position map. Maps hashes of positions (search_tree_node) to evaluation caches
        self._posn_map: Dict[
            Hashable, NodeType
        ] = dict()  # from search_tree_node_class.cache_key() to search_tree_node_class
        # These usually/eventually have a value estimate from the model, and may have children (next steps).
        # They proceed through the following states:
        # (1) Initially created without any children or any value estimate, and put into posns_waiting_for_value.
        # (2) value estimate has been supplied by process_batch; still no children. Node may stay in this state
        #     indefinitely, unless/until some rollout first decides to advance through this node.
        # (3) When a rollout selects this node, children are filled in; typically the children have no values themselves,
        #     so the node stays in this state with probs==None until all children are evaluated.
        #     The/all rollout(s) waiting for this node are put into rollouts_waiting_for_values.
        # (3) When all *children* have their values, fills in node.probs.

        # The "time-left" cache:
        self._tl_cache: Dict[ExprWithEnv, np.ndarray] = dict()
        self._tl_cache_hits = 0
        self._posns_waiting_for_eval: Dict[ExprWithEnv, List[NodeType]] = {}
        self._best_traj: List[Tuple[str, float, int]] = []
        self._cur_best = float("+inf")
        self._total_posns_evaluated = 0
        self._iter_counter = 0
        self._generated = 0
        # total seconds spent / number of expression nodes processed in model evaluation
        self._total_gnn_eval_time = 0
        self._total_gnn_eval_nodes = 0

    def _to_cache_key(self, *args, **kwargs):
        """ Return something hashable that can be used as a dict key."""
        return self._search_tree_node_class.to_cache_key(*args, **kwargs)

    def check_existing_node(self, *args, **kwargs) -> bool:
        """
        Check if a node already exists in the cache.
        """
        cache_key = self._to_cache_key(*args, **kwargs)
        return cache_key in self._posn_map

    def get_existing_node(self, *args, **kwargs):
        """ Get the search tree node corresponding to, say, a given expression and time left 
        (if the nodes are instances of TimeLeftSearchTreeNode).
        Throws an exception if the specified node does not already exist. """
        cache_key = self._to_cache_key(*args, **kwargs)
        return self._posn_map[cache_key]

    def get_node(self, exprenv: ExprWithEnv, *args, **kwargs) -> NodeType:
        """ Flyweight pattern - retrieves the Position for an expr + time_left, which may already have its _evaluation
            (and even children) set; or creates anew, and registers as needing its value to be calculated by process_batches.
            Thus, every node returned from get_node will eventually be evaluated (assuming process_batches is called) """
        cache_key = self._to_cache_key(exprenv, *args, **kwargs)
        if cache_key in self._posn_map:
            return self._posn_map[cache_key]
        posn = self._search_tree_node_class(exprenv, *args, **kwargs)  # type: ignore[call-arg]
        self._generated += 1
        self._posn_map[cache_key] = posn
        if exprenv.cost() < self._cur_best:
            self._best_traj.append(
                (str(exprenv.expr), exprenv.cost(), self._total_posns_evaluated)
            )
            self._cur_best = exprenv.cost()
        if exprenv in self._tl_cache:
            # The meaning of _evaluation depends on the policy that is used.
            # See compute_probs function.
            posn.set_evaluation(self._tl_cache[exprenv])
            self._tl_cache_hits += 1
        else:
            self.enqueue(posn)
        return posn

    def enqueue(self, node: NodeType) -> None:
        """ Add an expression to the queue for evaluation.
        """
        # Using dict here makes sure that we don't queue the same expression again.
        self._posns_waiting_for_eval.setdefault(node.exprenv, []).append(node)

    def _process_batch(
        self,
        expr_batch: Sequence[ExprWithEnv],
        posns_to_update_batch: Sequence[Sequence[NodeType]],
    ) -> None:
        """ Evaluate all the expressions that are queued for evaluation."""
        assert len(expr_batch) > 0 and len(expr_batch) <= self._batch_size

        def update_gnn_eval_stats(t0, t1):
            self._total_gnn_eval_nodes += sum(et.expr.num_nodes for et in expr_batch)
            self._total_gnn_eval_time += t1 - t0

        with utils.elapsed_time(update_gnn_eval_stats):
            values = self._model_wrapper.evaluate_all_time_left(expr_batch)
        self._total_posns_evaluated += len(expr_batch)

        for expr, val, posns in zip(expr_batch, values, posns_to_update_batch):
            self._tl_cache[expr] = val
            for node in posns:
                node.set_evaluation(val)
        self._iter_counter += 1

    def process_batches(self) -> List[ExprWithEnv]:
        """ Evaluate all expressions in self._posns_waiting_for_eval.

        Returns:
            the expressions that were evaluated.
        """
        # TODO #19061 allow batching by number of nodes, not number of graphs (and then deduplicate this code with log_fitted_vals in training.py)

        # Use dict insertion order (guaranteed since python3.7, accidental in CPython3.6).
        keys = list(self._posns_waiting_for_eval.keys())
        for i in range(0, len(keys), self._batch_size):
            expr_batch = keys[i : i + self._batch_size]
            posns_to_update_batch = [
                self._posns_waiting_for_eval[e] for e in expr_batch
            ]
            self._process_batch(expr_batch, posns_to_update_batch)
        self._posns_waiting_for_eval.clear()
        return keys

    def has_sufficient_posns_to_eval(self) -> bool:
        """ If true, RolloutSearcher will not add new rollouts
        """
        return len(self._posns_waiting_for_eval) >= self._batch_size

    @property
    def eval_queue_length(self) -> int:
        return len(self._posns_waiting_for_eval)

    def rollout_end_log_entries(self) -> Dict[str, Any]:
        """
        Helper method that returns entries to log in the end-of-search `rollout_end` event
        """
        return {
            "best_traj": self._best_traj,
            "posns_evaluated": self.total_posns_evaluated,
            "tl_cache_hits": self._tl_cache_hits,
            "batches": self._iter_counter,
            "generated": self._generated,
            "gnn_eval_nodes": self._total_gnn_eval_nodes,
            "gnn_eval_time": self._total_gnn_eval_time,
        }

    @property
    def total_posns_evaluated(self) -> int:
        return self._total_posns_evaluated

    @property
    def tl_cache(self) -> Mapping[ExprWithEnv, np.ndarray]:
        """ For expression e, tl_cache[e] is a numpy array of state values for every time left."""
        return self._tl_cache

    @property
    def num_batches(self):
        """ Number of batches that have been evaluated."""
        return self._iter_counter

    @property
    def best_cost_seen(self):
        return self._cur_best
