"""
This module implements layers and nn.Module-like building blocks from which a complete
model can be composed.

In this implementation, we account for different edge types in the Graph Neural-Net
(GNN) layers by using a different message function to compute the message passed 
along each edge type. This is in contrast to what's often done in many GNN packages:
encoding the edge type with a vector embedding.

The intuition for this decision is that the edge type can fundamentally change what
information needs to be be exchanged, and is not only an additional feature.
"""
import functools
from typing import List, Literal, Union, Optional
import torch
import torch.nn as nn
from torch import Tensor, IntTensor  # pylint:disable=no-name-in-module
from torch_geometric.nn import (
    GlobalAttention,
    global_add_pool,
    global_max_pool,
    global_mean_pool,
)
from torch_scatter import scatter

from rlo.torch_graph_data import EdgeList

# A callable that takes a tensor of per-node features, and a vector indicating which graph
# each node belongs to, and aggregates the per-node features in a permutation-invariant way.
GlobalGraphPool = Union[
    global_add_pool, global_max_pool, global_mean_pool, GlobalAttention
]

ScatterAggregation = Literal["mean", "sum", "min", "max", "mul"]


def aggregate_messages(
    messages: Tensor,  # [num_messages, feature_dim]
    message_targets: IntTensor,  # [num_messages,]
    num_nodes: int,
    aggr: ScatterAggregation,
) -> Tensor:  # [num_nodes, feature_dim]
    """
    Function to efficiently aggregate messages for each node in graph, where there are possibly
    multiple messages per each target node.
    
    This is a utility function for implementations of GNN message passing.

    Args:
        messages: The messages to be aggregated
        message_targets: The indices of the target nodes 
            for each of the messages to be aggregated at
        num_nodes: The number of nodes in the graph. Determines the size of the returned
            tensor with aggregated messages.
        aggr: Aggregation type ("mean", "sum", "max", ...)
    
    Returns:
        A tensor of shape [num_nodes, feature_dim] holding aggregated messages for each
        node in graph.
    """
    msg_dtype = messages.dtype
    return scatter(
        messages.to(torch.float32),
        index=message_targets,
        dim=0,
        dim_size=num_nodes,
        reduce=aggr,
    ).to(msg_dtype)


class GRUTensorflowLike(torch.nn.Module):  # TODO: Remove after debugging
    # Imitates the GRU cell of the tensorflow implementation, with zero dropout
    def __init__(self, input_size, weight_ih, weight_hh):
        super().__init__()
        self.input_size = input_size
        self.weight_ih = weight_ih.T
        self.weight_hh = weight_hh.T

    def forward(self, inputs, states):
        n = self.input_size
        h_tm1 = states
        x_z = torch.matmul(inputs, self.weight_ih[:, :n])
        x_r = torch.matmul(inputs, self.weight_ih[:, n : n * 2])
        x_h = torch.matmul(inputs, self.weight_ih[:, n * 2 :])
        recurrent_z = torch.matmul(h_tm1, self.weight_hh[:, :n])
        recurrent_r = torch.matmul(h_tm1, self.weight_hh[:, n : n * 2])
        z = torch.sigmoid(x_z + recurrent_z)
        r = torch.sigmoid(x_r + recurrent_r)
        recurrent_h = torch.matmul(r * h_tm1, self.weight_hh[:, n * 2 :])
        hh = torch.tanh(x_h + recurrent_h)
        h = z * h_tm1 + (1 - z) * hh
        return h


class GRUCell(torch.nn.Module):
    """A GRUCell written purely in Torch. Behaves the same way as torch.nn.GRUCell
    
    TODO #19341 test this and use it. Currently this class is not used.

    """

    def __init__(self, input_size: int, hidden_size: int, bias: bool = True):
        super().__init__()
        self.input_size = input_size
        self.hidden_size = hidden_size
        self.bias = bias

        self.weight_ih = torch.nn.Parameter(torch.Tensor(hidden_size * 3, input_size))
        self.weight_hh = torch.nn.Parameter(torch.Tensor(hidden_size * 3, hidden_size))
        self.bias_ih = torch.nn.Parameter(torch.Tensor(hidden_size * 3))
        self.bias_hh = torch.nn.Parameter(torch.Tensor(hidden_size * 3))

        self.reset_parameters()

    def reset_parameters(self) -> None:
        nn.init.orthogonal_(self.weight_hh)
        nn.init.xavier_uniform_(self.weight_ih)
        nn.init.constant_(self.bias_hh, val=0.0)
        nn.init.constant_(self.bias_ih, val=0.0)

    def forward(self, x: Tensor, hidden: Tensor) -> Tensor:
        # Linear transformation of input (shape [batch_size, hidden_size * 3])
        gi = torch.mm(x, self.weight_ih.t()) + self.bias_ih
        # Linear transformation of hidden (shape [batch_size, hidden_size * 3])
        gh = torch.mm(hidden, self.weight_hh.t()) + self.bias_hh

        # Chunk transformed tensors into 3 tensors of shape [batch_size, hidden_size]
        # These hold weights for: resetgate, inputgate (update) and newgate (candidate state)
        i_r, i_i, i_n = gi.chunk(3, dim=1)
        h_r, h_i, h_n = gh.chunk(3, dim=1)

        resetgate = torch.sigmoid(i_r + h_r)
        inputgate = torch.sigmoid(i_i + h_i)
        newgate = torch.tanh(i_n + resetgate * h_n)
        # Previous and candidate state mixed by input-gate
        # This is the same as (newgate * (1 - inputgate) + hidden * inputgate)
        hy = newgate + inputgate * (hidden - newgate)

        # TODO when we get mypy to work with Tensor, we can remove this 'ignore'
        return hy


class DropoutGRUCell(nn.GRUCell):
    """A wrapper around torch.nn.GRUCell that adds dropout to inputs and hidden.
    
    This wrapper makes the implementation more in-line with that of tf.keras.layers.GRUCell

    It doesn't accomplish it entirely, because: 1) the base GRU cell
    is different (the ordering of when the reset gate is applied is different between
    torch and tensorflow). 2) in the TF implementation a different mask is applied
    to the input/hidden vectors when passed into different gates.
    """

    def __init__(
        self,
        input_size,
        hidden_size,
        bias=True,
        dropout: float = 0.0,
        recurrent_dropout: float = 0.0,
    ):
        """
        Args:
            input_size: Dimensionality of the input to the GRUCell
            hidden_size: Dimensionality of the hidden dimension of the GRUCell
            bias (optional): If False, then the layer does not use bias weights b_ih and b_hh. Defaults to True.
            dropout (optional): Fraction of the units to drop for the linear transformation of the inputs. Defaults to 0.0.
            recurrent_dropout (optional): Fraction of the units to drop for the linear transformation of the inputs. Defaults to 0.0.
        """
        super().__init__(input_size=input_size, hidden_size=hidden_size, bias=bias)
        assert 0 <= dropout <= 1, "Dropout rate must be in the range [0, 1]"
        assert 0 <= recurrent_dropout <= 1, "Dropout rate must be in the range [0, 1]"
        self._dropout = dropout
        self._recurrent_dropout = recurrent_dropout

    def forward(
        self,
        input: Tensor,  # [batch, input_size],
        hx: Optional[Tensor] = None,  # [batch, hidden_size]
    ) -> Tensor:  # [batch, hidden_size]
        """
        Apply dropout to input and hidden tensors (`hx`), and pass them to the torch GRUCell.
        """
        if hx is None:
            # Imitate behaviour of parent class: hx is assumed zero if not given.
            hx = torch.zeros(
                input.size(0), self.hidden_size, dtype=input.dtype, device=input.device
            )
        dropped_input = nn.functional.dropout(
            input=input, p=self._dropout, training=self.training
        )
        dropped_hidden = nn.functional.dropout(
            input=hx, p=self._recurrent_dropout, training=self.training
        )
        return super().forward(dropped_input, dropped_hidden)


class RelationalGatedGraphConv(nn.Module):
    """A GatedGraphConv layer that supports different edge types.
    
    Specifically, the "message" will be computed by computing a message for each of the
    edge types (using different weights), and then aggregating those messages.

    It deviates from original Gated GNN in a few ways:
     -  Allows for multiple edge types. When the messages are passed between the nodes,
        the messages for each edge type will be computed using a separate set of weights.

    We chose not to override the nn.RelationGCN module from torch-geometric,
    to allow to pass different messages along each edge. That implementation indexes 
    into the weights vector with the edge-type, which can be computationally expensive.

    This implementation instead does a separate round of message propagation for each
    edge type, and then aggregates the results.
    """

    def __init__(
        self,
        hidden_dim: int,
        num_edge_types: int,
        num_propagations: int = 1,
        aggr: ScatterAggregation = "sum",
        bias: bool = True,
        recurrent_dropout: float = 0.0,
    ):
        super().__init__()
        self._num_edge_types = num_edge_types
        self._num_propagations = num_propagations
        self._hidden_dim = hidden_dim
        self._recurrent_dropout = recurrent_dropout
        self._aggr = aggr
        # Aggregation method

        self.aggregate_messages = functools.partial(aggregate_messages, aggr=aggr)

        # Make a GRU cell that updates the node hidden representation with a new one
        self.rnn = DropoutGRUCell(
            input_size=hidden_dim,
            hidden_size=hidden_dim,
            bias=bias,
            recurrent_dropout=recurrent_dropout,
        )
        # One message function for each edge type. Will be applied to node features before those are propagated
        # TODO: #19614 try message dropout
        self.edge_message_functions = nn.ModuleList(
            [
                nn.Linear(in_features=hidden_dim, out_features=hidden_dim, bias=bias)
                for _ in range(num_edge_types)
            ]
        )
        self.reset_parameters()

    @property
    def num_edge_types(self) -> int:
        return self._num_edge_types

    def reset_parameters(self) -> None:
        """Reset layer weights. Calls reset_parameters() of internal nn.modules"""
        for i in range(self._num_edge_types):
            nn.init.xavier_uniform_(self.edge_message_functions[i].weight,)
            nn.init.constant_(self.edge_message_functions[i].bias, val=0.0)
        nn.init.orthogonal_(self.rnn.weight_hh)
        nn.init.xavier_uniform_(self.rnn.weight_ih)
        nn.init.constant_(self.rnn.bias_hh, val=0.0)
        nn.init.constant_(self.rnn.bias_ih, val=0.0)

    def forward(
        self,
        node_states: Tensor,  # [num_nodes, hidden_dim]
        edge_lists: List[EdgeList],
    ) -> Tensor:  # [num_nodes, hidden_dim]
        """
        Foward pass through the network (a round of message propagation, followed by 
        aggregation, and passing through a GRU Cell).

        Note: Doesn't support receiving a full adjacency matrix of type
            torch_sparse.TorchSparse, unlike many torch_geometric GNNs.

        Args:
            node_states: A [num_nodes, input_hidden_dim] tensor with input_hidden_dim features for each
                node in the graph. If input_hidden_dim < hidden_dim, the input will be padded
                to match hidden_dim of this layer.
            edge_lists: One EdgeList per edge type.

        Returns:
            A tensor of shape [num_nodes, hidden_dim]
        """
        assert len(edge_lists) == self._num_edge_types
        num_nodes, hidden_dim = node_states.shape
        assert hidden_dim == self._hidden_dim

        # Get the message targets for all messages (along edges of all edge types)
        message_target_idxs = torch.cat([edge_index[1] for edge_index in edge_lists])

        # Perform message passing num_propagations times (with same weights each time)
        for _ in range(self._num_propagations):
            # 1. Gather messages for each edge type
            all_messages: List[
                Tensor  # shape [num_messages_of_this_type, hidden_dim]
            ] = []

            # TODO: consider asynchronous computation below: https://pytorch.org/tutorials/advanced/torch-script-parallelism.html
            for (edge_type_idx, edge_list) in enumerate(edge_lists):
                # edge_list shape: [2, "num_edges_of_this_type"]
                message_source_idxs = edge_list[0, :]

                # 1.0 Look up node_states for message sources
                source_node_states = nn.functional.embedding(
                    message_source_idxs, node_states
                )

                # 1.1 Apply edge-type-specific message function
                edge_message_function = self.edge_message_functions[edge_type_idx]
                message = edge_message_function(source_node_states)

                # 1.2 Store to all_messages
                all_messages.append(message)

            # 2. Aggregate the messages from each edge for all edge types
            aggregated_messages = self.aggregate_messages(
                messages=torch.cat(all_messages, dim=0),
                message_targets=message_target_idxs,
                num_nodes=num_nodes,
            )  # [num_nodes, node_state_dim]

            # 3. Update node state with the aggregated messages
            node_states = self.rnn(aggregated_messages, node_states)

        return node_states


class StackedRelationalGNN(nn.Module):
    """
    Applies multiple passes of graph-conv layers.
    """

    def __init__(
        self,
        hidden_dim: int,
        num_edge_types: int,  # Same as former num_edge_types
        recurrent_dropout: float,
        num_propagations: int = 1,
        num_gnn_blocks: int = 1,
        aggr: ScatterAggregation = "sum",
    ):
        super().__init__()
        self._num_edge_types = num_edge_types
        self.gnn_blocks = nn.ModuleList(
            [
                RelationalGatedGraphConv(
                    hidden_dim=hidden_dim,
                    num_edge_types=num_edge_types,
                    num_propagations=num_propagations,
                    aggr=aggr,
                    recurrent_dropout=recurrent_dropout,
                )
                for _ in range(num_gnn_blocks)
            ]
        )

    @property
    def num_edge_types(self) -> int:
        return self._num_edge_types

    def reset_parameters(self) -> None:
        """Reset weights. Calls reset_parameters() of internal gnn blocks."""
        for gnn_block in self.gnn_blocks:
            gnn_block.reset_parameters()

    def forward(self, node_states: Tensor, edge_lists: List[EdgeList]) -> Tensor:
        for gnn_block in self.gnn_blocks:
            node_states = gnn_block(node_states=node_states, edge_lists=edge_lists)
        return node_states


class GNNEncoder(nn.Module):
    def __init__(
        self,
        num_node_types: int,
        hidden_dim: int,
        num_edge_types: int,  # Same as former num_edge_types
        recurrent_dropout: float,
        num_propagations: int = 1,
        num_gnn_blocks: int = 1,
        aggr: ScatterAggregation = "sum",
    ):
        super().__init__()
        self._hidden_dim = hidden_dim
        self._num_edge_types = num_edge_types
        self.embedding = nn.Embedding(
            num_embeddings=num_node_types, embedding_dim=hidden_dim
        )  # TODO Make an option for a OneHot encoding (torch.functional...)
        self.gnn = StackedRelationalGNN(
            hidden_dim=hidden_dim,
            recurrent_dropout=recurrent_dropout,
            num_edge_types=num_edge_types,
            num_propagations=num_propagations,
            num_gnn_blocks=num_gnn_blocks,
            aggr=aggr,
        )
        self.reset_parameters()

    @property
    def num_edge_types(self) -> int:
        return self.gnn.num_edge_types

    @property
    def hidden_dim(self) -> int:
        return self._hidden_dim

    @property
    def output_dim(self) -> int:
        """
        GNNEncoder outputs initial node embedding with dim. hidden_dim stacked with the
        processed output from the GNN (also of shape hidden_dim).
        """
        return 2 * self._hidden_dim

    def reset_parameters(self) -> None:
        self.gnn.reset_parameters()
        # Initialise the embedding with xavier (Glorot) uniform
        # torch.nn.init.xavier_uniform_(self.embedding.weight)
        # Initialise the embedding with uniform [-0.05, 0.05] (same as TensorFlow default)
        torch.nn.init.uniform_(self.embedding.weight, a=-0.05, b=0.05)

    def forward(
        self, node_type: IntTensor, edge_lists: List[EdgeList]  # [num_nodes],
    ) -> Tensor:  # [num_nodes, output_dim]
        """
        node_type: Integers representing node types
        edge_lists: One EdgeList per edge type.

        Returns:
            A tensor of shape [total_num_nodes, 2 * hidden_dim]. The outputs is the
            initial node embedding and the node state after passing through a GNN
            stacked together.
        """
        node_embedding = self.embedding(node_type)
        processed_node_states = self.gnn(
            node_states=node_embedding, edge_lists=edge_lists
        )
        return torch.cat((node_embedding, processed_node_states), dim=-1)


class GatedRegression(nn.Module):
    """
    A gated regression layer that passes the outputs from a GNN through a gated MLP (processing 
    representation of each node independently) and then aggregates the outputs in a permutation-invariant way.
    
    TODO #19681 revisit the choice of pooling function.
    """

    def __init__(
        self,
        input_dim: int,
        hidden_dim: int,
        output_dim: int,
        dropout: float,
        pooling: GlobalGraphPool = global_mean_pool,
    ):
        super().__init__()
        self.pooling = pooling
        self.mlp = nn.Sequential(
            nn.Dropout(p=dropout),
            nn.Linear(in_features=input_dim, out_features=hidden_dim),
            # nn.ReLU(),  # TODO should we keep it here? this was missing in TF version
            nn.Dropout(p=dropout),
            nn.Linear(in_features=hidden_dim, out_features=output_dim),
        )
        self.gate = nn.Sequential(
            nn.Dropout(p=dropout),
            nn.Linear(in_features=input_dim, out_features=1),
            nn.Sigmoid(),
        )

        self.reset_parameters()

    def reset_parameters(self) -> None:
        """Initialise module parameters"""

        def _init_linear_weights(m: nn.Module):
            # Helper function to apply initialisation to all linear layers
            if type(m) == nn.Linear:
                # TODO: mypy ignore flag can be removed once we upgrade to torch==1.9.0
                torch.nn.init.xavier_uniform_(m.weight)  # type: ignore
                nn.init.normal_(m.bias, std=1e-5)  # type: ignore

        # Apply _init_weights recursively to each module in self.mlp
        self.mlp.apply(_init_linear_weights)
        # Initialise gate weights
        self.gate.apply(_init_linear_weights)

    def forward(self, x: Tensor, graph_id: IntTensor,) -> Tensor:
        """Pass tensors through the gated MLP and aggregate results per graph in batch.

        Args:
            x: shape [num_nodes, input_dim].
            graph_id: shape [num_nodes,]. graph_id[i] tells you which graph node i belongs to.

        Returns:
            A tensor of shape [num_graphs, output_dim], where num_graphs is the number
            of graphs in the input batch.
        """
        values = self.gate(x) * self.mlp(x)
        aggregated_values = self.pooling(values, batch=graph_id)

        return aggregated_values


def global_pooling_from_str(pooling: Literal["mean", "sum", "max"]) -> GlobalGraphPool:
    if pooling == "mean":
        return global_mean_pool
    elif pooling == "sum":
        return global_add_pool
    elif pooling == "max":
        return global_max_pool
    else:
        raise ValueError(f"No such global pooling strategy implemented: {pooling}.")
