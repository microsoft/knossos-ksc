# mypy: ignore-errors
import abc
import numpy as np
import tensorflow as tf
from typing import NamedTuple, Iterable, Dict, Optional, Union

from rlo.cost_normalizers import CostNormalizer
from rlo.expression import Expression
from rlo.layers import (
    DenseGNNEncoder,
    SparseGNNEncoder,
    GatedRegression,
    segment_log_softmax,
)
from rlo.tf_model import (
    KerasModel,
    ModelWrapper,
    _deserialize_loss_and_metric,
    _get_optimizer,
)
from rlo.pipelines import policy_value_training_pipeline
from rlo.rewrites import RuleSet


class RawPolicyValueEvaluation(NamedTuple):
    values: np.ndarray  # float, [num_time_heads,]
    log_probs: np.ndarray  # float, [num_nodes, num_time_heads, num_rules]


class PolicyLoss(tf.keras.losses.Loss):
    @staticmethod
    def call(advantages, log_probs_selected):
        return -tf.reduce_mean(
            advantages * log_probs_selected
        )  # negate because we maximize


def negative_entropy_regularizer(log_probs, row_splits):
    segment_ids = tf.ragged.row_splits_to_segment_ids(row_splits)
    # The gradient of tf.where involves evaluation of the inactive branch.
    # https://github.com/tensorflow/tensorflow/issues/38349
    # The recommended workaround is to use where to prevent inf entering
    # the unsafe computation. See:
    # https://stackoverflow.com/questions/33712178/tensorflow-nan-bug/42497444#42497444
    # This is unbiased because tf.exp(log_probs) == 0 if log_probs=-inf
    # and gradient will flow into safe_log_probs only when log_probs is finite.
    safe_log_probs = tf.where(tf.math.is_finite(log_probs), log_probs, -1.0)
    # sum over nodes and rules, then average over graphs and time_left
    return tf.reduce_mean(
        # reduce over rules
        tf.reduce_sum(
            # log_probs: num_nodes x num_rules x num_time_heads
            # segment_sum: num_graphs x num_rules x num_time_heads
            tf.math.segment_sum(tf.exp(log_probs) * safe_log_probs, segment_ids,),
            axis=1,
        ),
        axis=[0, 1],
    )


class PolicyAndValueLoss(NamedTuple):
    value_loss: tf.keras.losses.Loss
    policy_loss: tf.keras.losses.Loss


class PolicyValueModel(KerasModel):
    def __init__(
        self,
        sparse: bool,
        hidden_dim: int,
        output_hidden_dim: int,
        num_time_heads: int,
        num_propagations: int,
        one_hot_embedding: bool = False,
        recurrent_dropout: float = 0.5,
        output_dropout: float = 0.5,
        cumsum: Optional[float] = None,
        value_loss: str = "Huber",
        aux_loss_coeff: float = 0.5,
        entropy_reg_coeff: float = 2.0,
        lr: float = 1e-4,
        nonlinear_messages: bool = False,
        aggregation_over_edge_types: str = "sum",
        decoder_readout: str = "mean",
        message_from_sender_receiver=False,
        grad_clip_value=0,
    ):
        super().__init__()

        if message_from_sender_receiver and not sparse:
            raise NotImplementedError(
                "Using receiver features in message computation is not implemented for DenseGNN."
            )

        encoder_args = {
            "hidden_dim": hidden_dim,
            "one_hot_embedding": one_hot_embedding,
            "num_propagations": num_propagations,
            "recurrent_dropout": recurrent_dropout,
            "nonlinear_messages": nonlinear_messages,
            "aggregation_over_edge_types": aggregation_over_edge_types,
        }
        if sparse:
            encoder_args["message_from_sender_receiver"] = message_from_sender_receiver
            encoder_class = SparseGNNEncoder
        else:
            encoder_class = DenseGNNEncoder
        self.encoder = encoder_class(**encoder_args)
        self.value_regression = GatedRegression(
            hidden_dim=output_hidden_dim,
            num_outputs=num_time_heads,
            dropout_rate=output_dropout,
            cumsum=cumsum,
            readout=decoder_readout,
        )
        self._value_loss_name = value_loss
        self._aux_loss_coeff = aux_loss_coeff
        self._entropy_reg_coeff = entropy_reg_coeff
        self._lr = lr
        self._grad_clip_value = grad_clip_value

    @abc.abstractmethod
    def _compute_scores(self, features: tf.Tensor) -> tf.Tensor:
        """ Computes action propensity scores from GNN features.

        Args:
            features: feature representation of nodes
                      (total_num_nodes x num_features tensor)

        Returns:
            total_num_nodes x num_rules x num_time_heads tensor
            containing unnormalized / unfiltered propensity scores
            (the higher is more likely) of applying some rule on
            some node given some time_left.
        """
        raise NotImplementedError

    def call(self, graphs: Dict[str, tf.Tensor]):
        """ Forward pass """
        flat_features = self.encoder(graphs)
        segment_ids = tf.ragged.row_splits_to_segment_ids(graphs["node_row_splits"])
        values = self.value_regression(flat_features, graphs["node_row_splits"])

        scores = self._compute_scores(flat_features)
        # Filter out illegal actions and turn scores into log probabilities
        log_probs = segment_log_softmax(
            tf.where(tf.expand_dims(graphs["actions_mask"], axis=-1), scores, -np.inf),
            segment_ids,
            additional_axes=1,  # reduce over rules
        )

        return values, log_probs

    def _get_loss_optimizer_metric(self):
        value_loss, metric = _deserialize_loss_and_metric(self._value_loss_name.lower())
        policy_loss = PolicyLoss()
        optimizer = _get_optimizer(self._lr, self._grad_clip_value)
        return PolicyAndValueLoss(value_loss, policy_loss), optimizer, metric

    def _compute_loss(
        self,
        graphs: Dict[str, tf.Tensor],
        value_targets: tf.Tensor,
        value_weights: tf.Tensor,
        actions: tf.Tensor,
        advantages: tf.Tensor,
        training: bool,
    ):
        values, log_probs = self(graphs, training=training)
        selected_log_probs = tf.gather_nd(log_probs, actions)
        loss = (
            self.loss.value_loss(value_targets, values, value_weights)
            + (
                self._aux_loss_coeff
                * self.loss.policy_loss(advantages, selected_log_probs)
            )
            + (
                self._entropy_reg_coeff
                * negative_entropy_regularizer(log_probs, graphs["node_row_splits"])
            )
            + sum(self.losses)
        )
        return values, loss

    def train_on_batch(self, graphs, value_targets, value_weights, actions, advantages):
        with tf.GradientTape() as tape:
            values, loss = self._compute_loss(
                graphs, value_targets, value_weights, actions, advantages, training=True
            )
        grads = tape.gradient(loss, self.trainable_weights)
        self.optimizer.apply_gradients(zip(grads, self.trainable_weights))
        for m in self.compiled_metrics._metrics:
            m.update_state(value_targets, values, value_weights)
        return loss

    def test_on_batch(self, graphs, value_targets, value_weights, actions, advantages):
        values, loss = self._compute_loss(
            graphs, value_targets, value_weights, actions, advantages, training=False
        )
        for m in self.compiled_metrics._metrics:
            m.update_state(value_targets, values, value_weights)
        return loss

    @classmethod
    def create_wrapper(
        cls,
        rules: RuleSet,
        sparse: bool,
        model_hypers: dict,
        num_time_heads: int,
        use_subtree_match_edges: bool,
        cost_norm: Union[CostNormalizer, str, None] = None,
        **kwargs
    ):
        training_pipeline = policy_value_training_pipeline(
            sparse=sparse,
            use_subtree_match_edges=use_subtree_match_edges,
            num_time_heads=num_time_heads,
            rules=rules,
            cost_norm=cost_norm,
        )
        return PolicyValueModelWrapper(
            training_pipeline=training_pipeline,
            keras_model=cls(
                sparse=sparse, num_time_heads=num_time_heads, **model_hypers,
            ),
            num_time_heads=num_time_heads,
            **kwargs,
        )


class PolicyValueModelWrapper(ModelWrapper):
    def _evaluate_prepped(self, expr_batch: Iterable[Expression], prepared_data: Dict):
        """Computes RawPolicyValueEvaluation for every expression in expr_batch for
            all time_left.

        Args:
            expr_batch: a list of Expressions
            prepared_data: expressions prepared by self._graph_pipeline

        Returns:
            a list of RawPolicyValueEvaluation. Each RawPolicyValueEvaluation contains:
                - `values` of shape (max_depth,)
                - `log_probs` of shape (num_nodes, max_depth, num_rules)
        """
        values_batch, log_probs_batch = self._evaluate_all_time_left(
            tf.nest.map_structure(tf.convert_to_tensor, prepared_data)
        )
        values_batch = self._training_pipeline.denormalize_values(
            values_batch.numpy(), expr_batch
        )
        row_splits = prepared_data["node_row_splits"]
        assert len(row_splits) == len(values_batch) + 1  # number of graphs + 1
        return [
            RawPolicyValueEvaluation(
                values, log_probs_batch[row_splits[i] : row_splits[i + 1]]
            )
            for i, values in enumerate(values_batch)
        ]
