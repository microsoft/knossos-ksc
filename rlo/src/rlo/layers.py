import abc
from typing import (
    Any,
    Callable,
    Dict,
    Iterable,
    NamedTuple,
    Optional,
    Sequence,
    Tuple,
    Union,
)

import numpy as np
import tensorflow as tf

from rlo.expression import Expression

K = tf.keras.backend

SparseInput = NamedTuple(
    "SparseInput", [("node_features", tf.Tensor), ("adjacencies", Sequence[tf.Tensor])]
)
SparseInputLike = Union[SparseInput, Tuple[tf.Tensor, Sequence[tf.Tensor]]]
DenseInput = NamedTuple(
    "DenseInput", [("node_features", tf.Tensor), ("adjacencies", tf.Tensor)]
)
DenseInputLike = Union[DenseInput, Tuple[tf.Tensor, tf.Tensor]]

available_aggregations = [
    "sum",
    "mean",
    "min",
    "max",
    "concatenate",
    "concatenate_by_agg_types",
]


def initial_node_embedding(
    one_hot_embedding: bool, hidden_dim: int
) -> tf.keras.layers.Layer:
    if one_hot_embedding:
        assert Expression.num_node_types <= hidden_dim
        return tf.keras.layers.Lambda(
            lambda node_type: tf.one_hot(node_type, hidden_dim, dtype=tf.float32)
        )
    return tf.keras.layers.Embedding(Expression.num_node_types, hidden_dim)


def mlp(
    out_size: int,
    hid_sizes: Iterable[int],
    dropout_rate: float = 0.0,
    final_activation=None,
    name: str = None,
) -> tf.keras.Model:
    """
    Multi-layer perceptron model.

    Arg:
        out_size: number of channels of output.
        hid_sizes: number of channels of each hidden layer.
        dropout_rate: rate used in dropout layers, fraction of input units to
            drop.
        final_activation: used in final Dense layer.

    Returns:
        keras model
    """

    def add_dropout():
        if dropout_rate > 0:
            layers_list.append(tf.keras.layers.Dropout(dropout_rate))

    layers_list = []

    for h in hid_sizes:
        add_dropout()
        layers_list.append(tf.keras.layers.Dense(h, activation="relu"))
    add_dropout()
    layers_list.append(tf.keras.layers.Dense(out_size, activation=final_activation))

    return tf.keras.Sequential(layers_list, name=name)


def _generate_dropout_mask(ones, rate, training=None, count=1, seed=None):
    def dropped_inputs():
        return K.dropout(ones, rate, seed=seed)

    if count > 1:
        return [
            K.in_train_phase(dropped_inputs, ones, training=training)
            for _ in range(count)
        ]
    return K.in_train_phase(dropped_inputs, ones, training=training)


def get_aggregation(agg_type):
    error_message = f"Edge aggregation of type {agg_type} is not supported!"
    assert agg_type in available_aggregations, error_message

    if agg_type == "sum":
        return tf.add_n
    elif agg_type == "mean":
        return lambda msgs: tf.add_n(msgs) / len(msgs)
    elif agg_type == "min":
        return lambda msgs: tf.math.reduce_min(tf.convert_to_tensor(msgs), axis=0)
    elif agg_type == "max":
        return lambda msgs: tf.math.reduce_max(tf.convert_to_tensor(msgs), axis=0)
    elif agg_type == "concatenate":
        return lambda msgs: tf.concat(msgs, axis=-1)
    elif agg_type == "concatenate_by_agg_types":
        return lambda msgs: tf.concat(
            [
                get_aggregation("sum")(msgs),
                get_aggregation("mean")(msgs),
                get_aggregation("min")(msgs),
                get_aggregation("max")(msgs),
            ],
            axis=-1,
        )


class SeededGRUCell(tf.keras.layers.GRUCell):
    """
    GRUCell which uses dropout masks with seeds generated during construction.

    Unlike GRUCell, this implementation takes optional `recurrent_dropout_seed` and
    `dropout_seed` constructore kwargs. If not provided, they are sampled from
    `np.random.randint` during `__init__`. This differs from `GRUCell`, which samples
    a seed during execution, which can be difficult to predict when using `tf.function`.
    """

    def __init__(self, *args, **kwargs):
        self._recurrent_dropout_seed = kwargs.pop("recurrent_dropout_seed", None)
        self._dropout_seed = kwargs.pop("dropout_seed", None)
        if self._recurrent_dropout_seed is None:
            self._recurrent_dropout_seed = np.random.randint(10e6)
        if self._dropout_seed is None:
            self._dropout_seed = np.random.randint(10e6)
        super().__init__(*args, **kwargs)

    def get_config(self):
        config = super().get_config()
        config.update(
            dict(
                dropout_seed=self._dropout_seed,
                recurrent_dropout_seed=self._recurrent_dropout_seed,
            )
        )
        return config

    def _create_recurrent_dropout_mask(self, inputs, training, count=1):
        return _generate_dropout_mask(
            tf.ones_like(inputs),
            self.recurrent_dropout,
            training=training,
            count=count,
            seed=self._recurrent_dropout_seed,
        )

    def _create_dropout_mask(self, inputs, training, count=1):
        return _generate_dropout_mask(
            tf.ones_like(inputs),
            self.dropout,
            training=training,
            count=count,
            seed=self._dropout_seed,
        )


class GNN(tf.keras.layers.Layer):
    """
    An implementation of the method described in: https://arxiv.org/abs/1511.05493

    Compute new graph states by neural message passing and gated units on the nodes.
    For this, we assume existing node states h^t_v and a list of per-edge-type adjacency
    matrices A_ell.

    We compute new states as follows:
        h^{t+1}_v := Cell(h^t_v, sum_ell
                                 sum_{(u, v) in A_ell}
                                     W_ell * h^t_u + b_ell)

    The learnable parameters of this are
        * those in the recurrent Cell
        * List(L) of W_ell in R^{D, D}.
        * List(L) of b_ell in R^{D}.

    Constructor parameters are:
        num_propagations: number of iterations over the formula above.
        recurrent_dropout: used in `tf.keras.layers.RNNCell`.
        **kwargs: kernel / bias regularizers, constraints and additional
            kwargs passed to `tf.keras.layers.Layer.__init__`.

    This is the base class which manages kernel/bias/GRUCell creation. See
    implementations DenseGNN and SparseGNN for instance function signatures.

    Implementations must implement call and _get_sizes.
    """

    def __init__(
        self,
        num_propagations: int,
        recurrent_dropout: float = 0.0,
        kernel_initializer="glorot_uniform",
        kernel_regularizer=None,
        kernel_constraint=None,
        bias_initializer="zeros",
        bias_regularizer=None,
        bias_constraint=None,
        nonlinear_messages=False,
        aggregation_over_edge_types="sum",
        message_from_sender_receiver=False,
        **kwargs,
    ):
        self._num_propagations = num_propagations
        self._recurrent_dropout = recurrent_dropout
        self._kernel_initializer = tf.keras.initializers.get(kernel_initializer)
        self._kernel_regularizer = tf.keras.regularizers.get(kernel_regularizer)
        self._kernel_constraint = tf.keras.constraints.get(kernel_constraint)
        self._bias_initializer = tf.keras.initializers.get(bias_initializer)
        self._bias_regularizer = tf.keras.regularizers.get(bias_regularizer)
        self._bias_constraint = tf.keras.constraints.get(bias_constraint)
        self._nonlinear_messages = nonlinear_messages
        self._aggregation_over_edge_types = aggregation_over_edge_types
        self._edge_aggregation = get_aggregation(aggregation_over_edge_types)
        self._message_from_sender_receiver = message_from_sender_receiver

        super().__init__(**kwargs)

    def get_config(self) -> Dict[str, Any]:
        config = super().get_config()
        config.update(
            dict(
                num_propagations=self._num_propagations,
                recurrent_dropout=self._recurrent_dropout,
                kernel_initializer=tf.keras.initializers.serialize(
                    self._kernel_initializer
                ),
                kernel_regularizer=tf.keras.regularizers.serialize(
                    self._kernel_regularizer
                ),
                kernel_constraint=tf.keras.constraints.serialize(
                    self._kernel_constraint
                ),
                bias_initializer=tf.keras.initializers.serialize(
                    self._bias_initializer
                ),
                bias_regularizer=tf.keras.regularizers.serialize(
                    self._bias_regularizer
                ),
                bias_constraint=tf.keras.constraints.serialize(self._bias_constraint),
                nonlinear_messages=self._nonlinear_messages,
                aggregation_over_edge_types=self._aggregation_over_edge_types,
                message_from_sender_receiver=self._message_from_sender_receiver,
            )
        )
        return config

    @abc.abstractmethod
    def _get_sizes(self, input_shape) -> Tuple[int, int]:
        """
        Get sizes based on input shapes.

        Args:
            input_shape: list/tuple of input shapes

        Returns:
            input_dim: int, number of input node channels
            num_edge_types: int, number of edge types.
        """
        raise NotImplementedError("Abstract method")

    def build(self, input_shape) -> None:
        if self.built:
            return
        input_dim, num_edge_types = self._get_sizes(input_shape)
        # could be `tf.Dimension`s, so we cast these.
        # avoid tf.compat.v1.enable_v2_tensorshape
        # - can be change behaviour elsewhere based on imports, hard to debug
        # can be removed once in 2.x
        input_dim = int(input_dim)
        self._num_edge_types = int(num_edge_types)

        gru_input_dim = (
            input_dim * num_edge_types
            if self._aggregation_over_edge_types == "concatenate"
            else input_dim * 4
            if self._aggregation_over_edge_types == "concatenate_by_agg_types"
            else input_dim
        )

        # create a tuple so initialization is consistent with dense layers
        self.kernels = tuple(
            self.add_weight(
                "kernel{}".format(i),
                shape=[
                    input_dim * 2 if self._message_from_sender_receiver else input_dim,
                    input_dim,
                ],
                initializer=self._kernel_initializer,
                regularizer=self._kernel_regularizer,
                constraint=self._kernel_constraint,
                dtype=self.dtype,
                trainable=True,
            )
            for i in range(self._num_edge_types)
        )

        self.bias = self.add_weight(
            "bias",
            shape=[self._num_edge_types, input_dim],
            initializer=self._bias_initializer,
            regularizer=self._bias_regularizer,
            constraint=self._bias_constraint,
            dtype=self.dtype,
            trainable=True,
        )

        self.gru_cell = SeededGRUCell(
            input_dim,
            recurrent_dropout=self._recurrent_dropout,
            recurrent_activation="sigmoid",
            reset_after=False,
        )
        with tf.keras.backend.name_scope(self.gru_cell.name):
            self.gru_cell.build(tf.TensorShape((None, gru_input_dim)))

        super().build(input_shape)


class DenseGNN(GNN):
    """
    Dense GNN implementation.

    Instances of DenseGNN have the following signature:

    Args: (node_features, adjacency_matrix) tuple
        node_features: [B, V, D] float32 input node features.
        adjacency_matrix: [L, B, V, V] float adjacency matrices.
            Entry `(l, b, dst, src)` indicates a possible edge for:
                l:   edge type
                b:   batch
                dst: destination node
                src: source node
    Returns:
        [B, V, D] float32 output node features.
    
    In the above:
     - B - the batch_size (number of graphs in batch)
     - V - the number of nodes
     - D - the node feature dimension (number of features per graph)
     - L - number of edge types

    See also: GNN, dense_gnn.
    """

    def _get_sizes(self, input_shape):
        node_features_shape, adjacency_shape = input_shape
        input_dim = node_features_shape[-1]
        num_edge_types = adjacency_shape[0]
        return input_dim, num_edge_types

    def call(
        self, inputs: DenseInputLike, training: Optional[Union[tf.Tensor, bool]] = None
    ):
        node_features, adjacencies = inputs  # Shpes [B, V, D], [L, B, V, V]
        assert len(node_features.shape) == 3
        assert len(adjacencies.shape) == 4
        assert adjacencies.shape[0] == self._num_edge_types
        # unstack edge type dimension, used in `terms` summation in timesteps loop
        adjacencies = tf.unstack(adjacencies, axis=0)  # shape L * [B, V, V]
        kernels = self.kernels  # shape E * [D, D_out]
        biases = tf.unstack(self.bias, axis=0)  # shape L * [D_out]

        self.gru_cell.reset_dropout_mask()
        self.gru_cell.reset_recurrent_dropout_mask()

        def batch_matmul(a, b):
            return tf.einsum("bij,jk->bik", a, b)  # faster in tf-night 2.3
            # return tf.linalg.matmul(a, b)

        h = node_features
        v = tf.shape(h)[1]
        # h is updated each iteration
        act = tf.nn.relu if self._nonlinear_messages else tf.identity
        for _ in range(self._num_propagations):
            terms = [
                tf.linalg.matmul(adj, act(batch_matmul(h, kern) + b))
                for adj, kern, b in zip(adjacencies, kernels, biases)
            ]  # List(E) shape [B, V, D_out]
            # aggregate messages from each edge_type together
            # shape [B, V, D_out or D_out*edge_types or D_out*agg_types]
            x = self._edge_aggregation(terms)
            # gru_cell expected rank-2 inputs, so flatten -> gru -> unflatten
            x = tf.reshape(x, (-1, x.shape[-1]))  # shape [B * V, D_out]
            h = tf.reshape(h, (-1, h.shape[-1]))  # shape [B * V, D_out]
            h, _ = self.gru_cell(x, (h,), training=training)
            # h-Shape [B * V, D_out]
            h = tf.reshape(h, (-1, v, h.shape[-1]))  # shape [B, V, D_out]
        return h  # shape [B, V, D_out]


class SparseGNN(GNN):
    """
    Sparse GNN implementation.

    Instances of SparseGNN take the following inputs:
    Args: (node_features, adjacency_indices) tuple
        node_features: [V, D] float32 input node feautres.
        adjacency_indices: L * [E?, 2] int32 indices of adjacency matrices for
            each edge types. The number of edges (E?) may vary across edge
            type. Indices are in (src, dst) order.

    Returns:
        [V, D] float32 output node features.

    See also: GNN, sparse_gnn.
    """

    def _get_sizes(self, input_shape):
        node_features_shape, adj = input_shape
        input_dim = node_features_shape[-1]
        num_edge_types = len(adj)
        return input_dim, num_edge_types

    def call(
        self, inputs: SparseInputLike, training: Optional[Union[bool, tf.Tensor]] = None
    ):
        node_features, adjacency_lists = inputs  # [V, D], L * [E?, 2]
        assert len(node_features.shape) == 2
        for a in adjacency_lists:
            assert len(a.shape) == 2
            assert a.shape[1] == 2
            assert a.dtype.is_integer
        num_nodes = tf.shape(node_features)[0]

        # Let M be the number of messages (sum of all E):
        edge_sources, edge_destinations = zip(
            *(tf.unstack(adj, axis=-1) for adj in adjacency_lists)
        )

        cur_node_states = node_features
        kernels = self.kernels
        biases = tf.unstack(self.bias, axis=0)

        self.gru_cell.reset_dropout_mask()
        self.gru_cell.reset_recurrent_dropout_mask()

        # curr_node_states is updated at each step
        for _ in range(self._num_propagations):
            messages = []  # list of tensors of messages of shape [E?, D]

            # Collect incoming messages per edge type
            for src, dst, kernel, bias in zip(
                edge_sources, edge_destinations, kernels, biases
            ):
                edge_source_states = tf.nn.embedding_lookup(
                    params=cur_node_states, ids=src
                )  # shape [E?, D]

                if self._message_from_sender_receiver:
                    edge_destination_states = tf.nn.embedding_lookup(
                        params=cur_node_states, ids=dst
                    )
                    edge_inpt = tf.concat(
                        [edge_source_states, edge_destination_states], axis=-1
                    )
                else:
                    edge_inpt = edge_source_states

                message = tf.matmul(edge_inpt, kernel) + bias  # shape [E?, D]
                if self._nonlinear_messages:
                    message = tf.nn.relu(message)
                message = tf.math.segment_sum(message, dst)
                message = tf.pad(
                    message, [[0, num_nodes - tf.shape(message)[0]], [0, 0]]
                )
                messages.append(message)

            # aggregate messages from each edge_type together
            # shape [V, D or D * edge_types of D* agg_types]
            aggregated_messages = self._edge_aggregation(messages)

            # pass updated vertex features into RNN cell
            new_node_states, _ = self.gru_cell(
                aggregated_messages, (cur_node_states,), training=training
            )  # shape [V, D]
            cur_node_states = new_node_states

        return cur_node_states


class GNNEncoder(tf.keras.Model):
    def __init__(self, hidden_dim: int, one_hot_embedding: bool):
        super().__init__()
        self.initial_node_embedding = initial_node_embedding(
            one_hot_embedding, hidden_dim
        )

    @abc.abstractmethod
    def _compute_node_features(
        self, initial_features, node_row_splits, adjacency
    ) -> tf.Tensor:
        """ Compute per-node features. """
        raise NotImplementedError("Abstract method")

    def call(self, graphs: Dict[str, tf.Tensor]) -> tf.Tensor:
        """ Compute node features with GNN and skip concat connection. """
        initial_features = self.initial_node_embedding(graphs["node_type"])
        processed_features = self._compute_node_features(
            initial_features, graphs["node_row_splits"], graphs["adjacency"]
        )
        return tf.concat([initial_features, processed_features], axis=-1)


class DenseGNNEncoder(GNNEncoder):
    def __init__(
        self,
        num_propagations: int,
        recurrent_dropout: float,
        nonlinear_messages: bool,
        aggregation_over_edge_types: str,
        **kwargs,
    ):
        super().__init__(**kwargs)
        self.pad = tf.keras.layers.Lambda(
            lambda args: tf.RaggedTensor.from_row_splits(*args).to_tensor()
        )
        self.gnn = DenseGNN(
            num_propagations,
            recurrent_dropout,
            nonlinear_messages=nonlinear_messages,
            aggregation_over_edge_types=aggregation_over_edge_types,
        )

    def _compute_node_features(
        self, initial_features, node_row_splits, adjacency_matrices
    ) -> tf.Tensor:
        """Compute node features with DenseGNN. """
        row_lengths = node_row_splits[1:] - node_row_splits[:-1]

        padded_features = self.pad((initial_features, node_row_splits))
        processed_features = self.gnn(
            (padded_features, tf.transpose(adjacency_matrices, (1, 0, 2, 3)))
        )
        return tf.boolean_mask(processed_features, tf.sequence_mask(row_lengths))


class SparseGNNEncoder(GNNEncoder):
    def __init__(
        self,
        num_propagations: int,
        recurrent_dropout: float,
        nonlinear_messages: bool,
        aggregation_over_edge_types: str,
        message_from_sender_receiver: bool,
        **kwargs,
    ):
        super().__init__(**kwargs)
        self.gnn = SparseGNN(
            num_propagations,
            recurrent_dropout,
            nonlinear_messages=nonlinear_messages,
            aggregation_over_edge_types=aggregation_over_edge_types,
            message_from_sender_receiver=message_from_sender_receiver,
        )

    def _compute_node_features(
        self, initial_features, _node_row_splits, edge_lists
    ) -> tf.Tensor:
        """Compute node features with SparseGNN. """
        return self.gnn((initial_features, edge_lists))


class StackedGNNEncoder(GNNEncoder):
    def __init__(
        self,
        num_propagations: int,
        recurrent_dropout: float,
        nonlinear_messages: bool,
        aggregation_over_edge_types: str,
        num_gnn_blocks: int,
        stacked_gnn_double_hidden: bool,
        message_from_sender_receiver: bool,
        **kwargs,
    ):
        super().__init__(**kwargs)

        if num_propagations % num_gnn_blocks != 0:
            raise ValueError(
                "The number of GNN blocks should be a divider of the number of propagations."
            )

        def get_gnn():
            return SparseGNN(
                num_propagations // num_gnn_blocks,
                recurrent_dropout,
                nonlinear_messages=nonlinear_messages,
                aggregation_over_edge_types=aggregation_over_edge_types,
                message_from_sender_receiver=message_from_sender_receiver,
            )

        self.gnns = [
            get_gnn()
            if stacked_gnn_double_hidden
            else tf.keras.Sequential(
                [get_gnn(), tf.keras.layers.Dense(kwargs["hidden_dim"]),]
            )
            for _ in range(num_gnn_blocks)
        ]

    def _compute_node_features(
        self, initial_features, _node_row_splits, edge_lists
    ) -> tf.Tensor:
        """Compute node features with a stack of SparseGNNs. """
        h = initial_features
        for gnn in self.gnns:
            h = gnn((tf.concat([h, initial_features], axis=-1), edge_lists,))
        return h


class GatedRegression(tf.keras.Model):
    def __init__(
        self,
        hidden_dim: int,
        num_outputs: int,
        dropout_rate: float,
        cumsum: Optional[float] = None,
        readout: str = "mean",
    ):
        super().__init__()
        self.out_layer = mlp(
            hidden_dim, [], dropout_rate=dropout_rate, name="out_layer",
        )
        self.regression_gate = mlp(
            1,
            [],
            dropout_rate=dropout_rate,
            final_activation="sigmoid",
            name="regression_gate",
        )
        self.regression_transform = mlp(
            num_outputs, [], dropout_rate=dropout_rate, name="regression_transform",
        )

        if readout == "mean":
            readout = tf.math.segment_mean
        elif readout == "max":
            readout = tf.math.segment_max
        elif readout == "min":
            readout = tf.math.segment_min
        elif readout == "sum":
            readout = tf.math.segment_sum
        else:
            raise ValueError(f"{readout} is an unknown readout.")

        def readout_fun(args):
            flat_values, row_splits = args
            segment_ids = tf.ragged.row_splits_to_segment_ids(row_splits)
            return readout(flat_values, segment_ids)

        self.readout = tf.keras.layers.Lambda(readout_fun)

        if cumsum is None or (isinstance(cumsum, float) and cumsum > 0):
            self.cumsum = cumsum
        else:
            raise ValueError(
                "Cumsum must be a positive float (the value of alpha, inf=relu)"
            )

    def call(self, flat_features: tf.Tensor, row_splits: tf.Tensor) -> tf.Tensor:
        """
        Produce per-graph features from output of `SparseGNNEncoder` / `DenseGNNEncoder`.

        This uses a gated per-node MLP followed reduction over the node dimension.

        Args:
            flat_features: (total_num_nodes, hidden_dim) float Tensor of per-node features
            row_splits: (num_graphs + 1,) int Tensor representing segments

        Returns:
            (num_graphs, num_time_heads) float Tensor of per-graph/time_left inferences.
        """
        gated_outputs = self.regression_gate(flat_features) * self.regression_transform(
            self.out_layer(flat_features)
        )

        value_vector = self.readout([gated_outputs, row_splits])

        if self.cumsum is not None:
            if self.cumsum == float("inf"):
                pos_values = tf.nn.relu(value_vector)
            else:
                pos_values = tf.nn.softplus(value_vector * self.cumsum) / self.cumsum
            value_vector = tf.cumsum(pos_values, axis=1)
        return value_vector


def _segment_reduction_with_broadcast(
    segment_op: Callable[[tf.Tensor, tf.Tensor], tf.Tensor],
    data: tf.Tensor,
    segment_ids: tf.Tensor,
):
    """
    Perform the given segment reduction and broadcast the result to the input size.

    Example usage:
    ```python
    data = tf.range(5)
    segment_ids = tf.constant([0, 0, 1, 1, 1])
    seg_sum = _segment_reduction_with_broadcast(tf.math.segment_sum, data, segment_ids)
    assert tf.reduce_all(seg_sum == [1, 1, 9, 9, 9])
    assert seg_sum.shape == data.shape
    ```
    """
    return tf.gather(segment_op(data, segment_ids), segment_ids)


def segment_log_softmax(
    data: tf.Tensor,
    segment_ids: tf.Tensor,
    additional_axes: Union[int, Sequence[int]] = (),
):
    """
    softmax applied over segments.

    This similar to `tf.math.segment_[sum|max]`, though `additional_axes` can be
    provided to include additional axes in the normalization.

    Args:
        data: rank N float tensor, N > 0. logit values
        segment_ids: rank 1 int indices of segments to apply softmax over
        additional_axes: int or ints of additional axes to reduce over.

    Returns:
        softmax values, same shape and dtype as `data`.
    """
    # subtract max value in each segment for numerical stability
    segment_max = _segment_reduction_with_broadcast(
        tf.math.segment_max,
        tf.math.reduce_max(data, axis=additional_axes, keepdims=True),
        segment_ids,
    )
    data = data - segment_max

    # standard log softmax
    exp = tf.exp(data)
    norm_factor = _segment_reduction_with_broadcast(
        tf.math.segment_sum,
        tf.math.reduce_sum(exp, axis=additional_axes, keepdims=True),
        segment_ids,
    )
    return data - tf.math.log(norm_factor)
