# mypy: ignore-errors
from typing import Iterable, NamedTuple, Union, Sequence

import numpy as np
import tensorflow as tf

from rlo.cost_normalizers import CostNormalizer, cost_normalizer
from rlo.dataset import RawValueExample
from rlo.expression_util import ExprWithEnv
from . import Pipeline, GraphPipeline, SparsePipeline, DensePipeline
from .graph_pipeline import GraphData


class ValueTrainingExample(NamedTuple):
    graph_data: GraphData
    values: np.ndarray
    sample_weights: np.ndarray


Number = Union[int, float]

# Don't subclass Pipeline because we cannot call ValueTargetPipeline.as_regenerator()
class ValueTargetPipeline:
    def __init__(
        self, num_time_heads: int, cost_norm: Union[None, str, CostNormalizer] = None
    ):
        self._num_time_heads = num_time_heads
        self._cost_norm = (
            cost_norm
            if isinstance(cost_norm, CostNormalizer)
            else cost_normalizer(cost_norm)
        )

    def get_hypers_to_save(self):
        return {
            "num_time_heads": self._num_time_heads,
            "cost_norm": self._cost_norm.name,
        }

    def prepare_example(self, exprenv: ExprWithEnv, targets: Sequence[Number]):
        valid_depth = len(targets)
        padding = self._num_time_heads - valid_depth
        if padding > 0:
            temp = targets
            targets = np.full((self._num_time_heads,), targets[-1], dtype=np.float32)
            targets[: len(temp)] = temp
            del temp
        else:
            targets = targets[: self._num_time_heads]
        weights = np.zeros((self._num_time_heads,), dtype=np.float32)
        weights[1 : min(valid_depth, self._num_time_heads)] = 1
        targets = self._cost_norm.normalize_value(targets, exprenv.cost())
        return targets, weights

    def denormalize_values(
        self, values_pred: np.ndarray, expressions: Iterable[ExprWithEnv]
    ) -> np.ndarray:
        """
        Take predicted (normalised) values and denormalise them (possibly using the
        cost of the corresponding expression).

        Args:
            values_pred: Predicted values to denormalize
            expressions: An iterable of expressions corresponding to the predicted
                values, the cost of which will be used to denormalize said values

        Returns:
            Denormalized values
        """
        return self._cost_norm.denormalize_values(
            values_pred, [e.cost() for e in expressions]
        )


def _spec_to_input(spec: tf.TensorSpec) -> tf.Tensor:
    """Get a keras input tensor from a tensor spec."""
    return tf.keras.Input(
        shape=spec.shape[1:], dtype=spec.dtype, batch_size=spec.shape[0], name=spec.name
    )


class TrainingPipeline(Pipeline):
    """
    Pipeline combining cost normalization and graph pipeline.

    Implementations should implement:
        * prepare_example
        * zip_elements
        * post_batch_map
        * batched_spec
    """

    def __init__(
        self, graph_pipeline: GraphPipeline, value_target_pipeline: ValueTargetPipeline,
    ):
        self._graph_pipeline = graph_pipeline
        self._value_target_pipeline = value_target_pipeline

    def get_hypers_to_save(self):
        return {
            "graph_pipeline": self._graph_pipeline.get_hypers_to_save(),
            **self._value_target_pipeline.get_hypers_to_save(),
        }

    def inputs(self):
        """Get structure of `tf.keras.Input`s consistent with this pipeline."""
        return tf.nest.map_structure(_spec_to_input, self.batched_spec[0])

    def denormalize_values(
        self, values_pred: np.ndarray, expressions: Iterable[ExprWithEnv]
    ) -> np.ndarray:
        return self._value_target_pipeline.denormalize_values(values_pred, expressions)

    def _num_nodes(self, example):
        return self._graph_pipeline._num_nodes(example.graph_data)


class ValueTrainingPipeline(TrainingPipeline):
    def __init__(
        self, graph_pipeline: GraphPipeline, value_target_pipeline: ValueTargetPipeline,
    ):
        super().__init__(graph_pipeline, value_target_pipeline)

    def prepare_example(self, example: RawValueExample) -> ValueTrainingExample:
        exprenv, targets = example
        graph_data = self._graph_pipeline.prepare_example(exprenv)
        targets, weights = self._value_target_pipeline.prepare_example(exprenv, targets)
        return ValueTrainingExample(graph_data, targets, weights)

    def zip_elements(self, examples: Sequence[ValueTrainingExample]):
        graph_data, targets, weights = zip(*examples)
        graph_data = self._graph_pipeline.zip_elements(graph_data)
        return graph_data, targets, weights

    def post_batch_map(self, batched_data):
        graph_data, targets, weights = batched_data
        return (
            self._graph_pipeline.post_batch_map(graph_data),
            np.array(targets, dtype=np.float32),
            np.array(weights, dtype=np.float32),
        )

    @property
    def batched_spec(self):
        num_time_heads = self._value_target_pipeline._num_time_heads
        return (
            self._graph_pipeline.batched_spec,
            tf.TensorSpec(
                shape=(None, num_time_heads), dtype=tf.float32, name="target"
            ),
            tf.TensorSpec(
                shape=(None, num_time_heads), dtype=tf.float32, name="weights"
            ),
        )


def value_training_pipeline(
    sparse: bool,
    use_subtree_match_edges: bool,
    num_time_heads: int,
    cost_norm: Union[None, str, CostNormalizer],
) -> ValueTrainingPipeline:
    graph_pipeline = (SparsePipeline if sparse else DensePipeline)(
        use_subtree_match_edges
    )
    return ValueTrainingPipeline(
        graph_pipeline=graph_pipeline,
        value_target_pipeline=ValueTargetPipeline(num_time_heads, cost_norm),
    )
