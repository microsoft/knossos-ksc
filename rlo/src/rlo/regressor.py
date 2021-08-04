# mypy: ignore-errors
from typing import Optional, Union

import tensorflow as tf

from rlo.cost_normalizers import CostNormalizer

from rlo.pipelines import (
    GraphPipeline,
    SparsePipeline,
    DensePipeline,
    ValueTrainingPipeline,
    ValueTargetPipeline,
)
from rlo.layers import (
    DenseGNNEncoder,
    SparseGNNEncoder,
    StackedGNNEncoder,
    GatedRegression,
)
from rlo.tf_model import (
    KerasModel,
    ModelWrapper,
    _deserialize_loss_and_metric,
    _get_optimizer,
)


class GNNStateValueRegressor(ModelWrapper):
    """ Base class for GNN-based state-value model wrapper. """

    def __init__(
        self,
        seed: int,
        graph_pipeline: GraphPipeline,
        keras_model: tf.keras.Model,
        num_time_heads: int,
        cost_norm: Union[CostNormalizer, str, None] = None,
        **kwargs
    ):
        super().__init__(
            seed,
            ValueTrainingPipeline(
                graph_pipeline,
                ValueTargetPipeline(num_time_heads=num_time_heads, cost_norm=cost_norm),
            ),
            keras_model,
            num_time_heads,
            **kwargs
        )


class GNNStateValueModel(KerasModel):
    """
    Base class for GNN State Value models implemented as keras Models.

    Implementations must implement `_compute_node_features`, which should map
    inputs specified by `graph_pipeline` to per-node features. These features are
    used in a per-node MLP and reduced to produce per-graph inferences.

    See SparseGNNStateValueModel and DenseGNNStateValueModel for example implementations.
    """

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
        loss: str = "Huber",
        lr: float = 1e-4,
        nonlinear_messages: bool = False,
        aggregation_over_edge_types: str = "sum",
        num_gnn_blocks=1,
        stacked_gnn_double_hidden=False,
        decoder_readout: str = "mean",
        message_from_sender_receiver: bool = False,
        grad_clip_value=0,
    ):
        super().__init__()
        # To support wrapping with tf.function, the call method should be pure (i.e. should
        # not create any TF resources). Thus, all the layers should be defined ahead of time.

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

        if num_gnn_blocks > 1:
            encoder_args["num_gnn_blocks"] = num_gnn_blocks
            encoder_args["stacked_gnn_double_hidden"] = stacked_gnn_double_hidden
            encoder_class = StackedGNNEncoder
        else:
            encoder_class = SparseGNNEncoder if sparse else DenseGNNEncoder

        self.encoder = encoder_class(**encoder_args)
        self.regression = GatedRegression(
            hidden_dim=output_hidden_dim,
            num_outputs=num_time_heads,
            dropout_rate=output_dropout,
            cumsum=cumsum,
            readout=decoder_readout,
        )
        self._loss_name = loss
        self._lr = lr
        self._grad_clip_value = grad_clip_value

    def _get_loss_optimizer_metric(self):
        loss, metric = _deserialize_loss_and_metric(self._loss_name.lower())
        optimizer = _get_optimizer(self._lr, self._grad_clip_value)
        return loss, optimizer, metric

    def call(self, graphs):
        """ Forward pass """
        features = self.encoder(graphs)
        return self.regression(features, graphs["node_row_splits"])

    def train_on_batch(self, inputs, labels, weights):
        with tf.GradientTape() as tape:
            predictions = self(inputs, training=True)
            loss = self.loss(labels, predictions, weights) + sum(self.losses)
        grads = tape.gradient(loss, self.trainable_weights)
        self.optimizer.apply_gradients(zip(grads, self.trainable_weights))
        for m in self.compiled_metrics._metrics:
            m.update_state(labels, predictions, weights)
        return loss

    def test_on_batch(self, inputs, labels, weights):
        predictions = self(inputs, training=False)
        loss = self.loss(labels, predictions, weights) + sum(self.losses)
        for m in self.compiled_metrics._metrics:
            m.update_state(labels, predictions, weights)
        return loss


class SparseGNNStateValueRegressor(GNNStateValueRegressor):
    """ Model wrapper that uses SparseGNNStateValueModel and SparsePipeline """

    def __init__(
        self,
        seed: int,
        num_time_heads: int,
        use_subtree_match_edges: bool,
        model_hypers: dict,
        **kwargs
    ):
        graph_pipeline = SparsePipeline(
            use_subtree_match_edges=use_subtree_match_edges,
        )
        model = GNNStateValueModel(
            sparse=True, num_time_heads=num_time_heads, **model_hypers
        )
        super().__init__(seed, graph_pipeline, model, num_time_heads, **kwargs)


class DenseGNNStateValueRegressor(GNNStateValueRegressor):
    """ Model wrapper that uses DenseGNNStateValueModel and DensePipeline """

    def __init__(
        self,
        seed: int,
        num_time_heads: int,
        use_subtree_match_edges: bool,
        model_hypers: dict,
        **kwargs
    ):
        if (
            model_hypers["num_gnn_blocks"] > 1
            or model_hypers["stacked_gnn_double_hidden"]
        ):
            raise NotImplementedError("StackedGNN is not implemented for DenseGNN.")
        graph_pipeline = DensePipeline(use_subtree_match_edges=use_subtree_match_edges,)
        model = GNNStateValueModel(
            sparse=False, num_time_heads=num_time_heads, **model_hypers
        )
        super().__init__(seed, graph_pipeline, model, num_time_heads, **kwargs)
