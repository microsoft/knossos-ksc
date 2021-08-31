# mypy: ignore-errors
from typing import NamedTuple, Sequence, Union

import numpy as np
import tensorflow as tf

from rlo.cost_normalizers import CostNormalizer
from rlo.dataset import RawPolicyValueTrainingExample, RewriteId
from rlo.expression_util import ExprWithEnv
from . import (
    Pipeline,
    GraphPipeline,
    SparsePipeline,
    DensePipeline,
    TrainingPipeline,
    ValueTargetPipeline,
)
from .graph_pipeline import (
    ragged_components,
    GraphData,
    BatchedGraphData,
)
from rlo.rewrites import RuleSet


class GraphActionsData(NamedTuple):
    graph_data: GraphData
    actions_mask: np.ndarray


class BatchedGraphActionsData(NamedTuple):
    graph_data: BatchedGraphData
    actions_mask: Sequence[np.ndarray]


class PolicyValueTrainingExample(NamedTuple):
    graph_actions_data: GraphActionsData
    values: np.ndarray
    value_weights: np.ndarray
    actions: Sequence[RewriteId]
    advantages: np.ndarray


class GraphActionsPipeline(Pipeline):
    def __init__(self, rules: RuleSet, graph_pipeline: GraphPipeline):
        self._rules = rules
        self._num_rules = len(rules)
        self._graph_pipeline = graph_pipeline

    def prepare_example(self, exprenv: ExprWithEnv) -> GraphActionsData:
        actions_mask = np.zeros(
            (exprenv.expr.num_nodes, self._num_rules), dtype=np.bool
        )
        for action in self._rules.get_all_rewrites(exprenv):
            actions_mask[action.node_id, self._rules.id_for_rule(action.rule)] = True
        return GraphActionsData(
            self._graph_pipeline.prepare_example(exprenv), actions_mask
        )

    def zip_elements(
        self, examples: Sequence[GraphActionsData]
    ) -> BatchedGraphActionsData:
        return BatchedGraphActionsData(
            self._graph_pipeline.zip_elements([ex.graph_data for ex in examples]),
            actions_mask=[ex.actions_mask for ex in examples],
        )

    def post_batch_map(self, batched_data: BatchedGraphActionsData):
        d = self._graph_pipeline.post_batch_map(batched_data.graph_data)
        # row splits are the same as node_row_splits
        flat_actions_mask, _, _ = ragged_components(batched_data.actions_mask)
        d["actions_mask"] = flat_actions_mask
        return d

    @property
    def batched_spec(self):
        return dict(
            **self._graph_pipeline.batched_spec,
            actions_mask=tf.TensorSpec(
                (None, None), dtype=tf.bool, name="actions_mask",
            ),
        )

    def _num_nodes(self, example):
        return self._graph_pipeline._num_nodes(example.graph_data)


class PolicyValueTrainingPipeline(TrainingPipeline):
    def __init__(
        self,
        graph_pipeline: GraphActionsPipeline,
        value_target_pipeline: ValueTargetPipeline,
    ):
        super().__init__(graph_pipeline, value_target_pipeline)
        self._num_time_heads = value_target_pipeline._num_time_heads

    def prepare_example(
        self, example: RawPolicyValueTrainingExample
    ) -> PolicyValueTrainingExample:
        exprenv, value_targets, action_advs = example
        actions = [action for action, _ in action_advs]
        graph_actions_data = self._graph_pipeline.prepare_example(exprenv)
        values, value_weights = self._value_target_pipeline.prepare_example(
            exprenv, value_targets
        )
        advantages = np.zeros((len(actions), self._num_time_heads,), dtype=np.float32)
        for i, (_, advs) in enumerate(action_advs):
            valid_depth = min(len(advs), self._num_time_heads)
            advantages[i, :valid_depth] = advs[:valid_depth]
        return PolicyValueTrainingExample(
            graph_actions_data, values, value_weights, actions, advantages
        )

    def zip_elements(self, examples: Sequence[PolicyValueTrainingExample]):
        graph_actions_data, values, value_weights, actions, advantages = zip(*examples)
        graph_actions_data = self._graph_pipeline.zip_elements(graph_actions_data)
        return graph_actions_data, values, value_weights, actions, advantages

    def post_batch_map(self, batched_data):
        (
            graph_actions_data,
            values,
            value_weights,
            batched_actions,
            advantages,
        ) = batched_data
        graph_actions_data = self._graph_pipeline.post_batch_map(graph_actions_data)
        # Each element of batched_actions contains variable number of actions
        actions = np.array(
            [
                [action.node_id + offset, action.rule_id]
                for actions, offset in zip(
                    batched_actions,
                    graph_actions_data["node_row_splits"].astype(np.int32),
                )
                for action in actions
            ],
            dtype=np.int32,
        )
        # advantages is a sequences of (num_actions, num_time_heads) arrays.
        # just flatten them (no need to store the row splits)
        advantages = np.concatenate(advantages, axis=0)
        return (
            graph_actions_data,
            np.array(values, dtype=np.float32),
            np.array(value_weights, dtype=np.float32),
            actions,
            advantages,
        )

    @property
    def batched_spec(self):
        return (
            self._graph_pipeline.batched_spec,
            tf.TensorSpec(
                shape=(None, self._num_time_heads), dtype=tf.float32, name="values"
            ),
            tf.TensorSpec(
                shape=(None, self._num_time_heads),
                dtype=tf.float32,
                name="value_weights",
            ),
            tf.TensorSpec((None, 2), dtype=tf.int32, name="actions"),
            tf.TensorSpec(
                shape=(None, self._num_time_heads), dtype=tf.float32, name="advantages",
            ),
        )

    def _num_nodes(self, example):
        return self._graph_pipeline._num_nodes(example.graph_actions_data)


def policy_value_training_pipeline(
    sparse: bool,
    use_subtree_match_edges: bool,
    num_time_heads: int,
    rules: RuleSet,
    cost_norm: Union[None, str, CostNormalizer] = None,
) -> TrainingPipeline:
    graph_pipeline = (SparsePipeline if sparse else DensePipeline)(
        use_subtree_match_edges
    )
    return PolicyValueTrainingPipeline(
        GraphActionsPipeline(rules, graph_pipeline),
        ValueTargetPipeline(num_time_heads, cost_norm),
    )
