import torch
import torch_geometric as tg
from torch import Tensor

from rlo.torch_graph_data import BatchedGraphData

from .layers import GatedRegression, GlobalGraphPool, GNNEncoder, ScatterAggregation
from .model import Model
from .losses import loss_func_from_str


class StateValueModel(Model):
    """A model that predicts the value for multiple time-steps left from an expression"""

    def __init__(
        self,
        *,
        num_node_types: int,
        gnn_hidden_dim: int,
        num_edge_types: int,
        num_gnn_blocks: int,
        regressor_hidden_dim: int,
        num_time_heads: int,
        loss: str,
        lr: float,
        gnn_dropout: float = 0.0,
        regressor_dropout: float = 0.0,
        aggregation_over_edge_types: ScatterAggregation = "sum",
        global_pooling: GlobalGraphPool = tg.nn.global_mean_pool,
        num_propagations: int = 1,
    ):
        super().__init__()

        self.encoder = GNNEncoder(
            num_node_types=num_node_types,
            hidden_dim=gnn_hidden_dim,
            num_edge_types=num_edge_types,
            recurrent_dropout=gnn_dropout,
            num_propagations=num_propagations,
            num_gnn_blocks=num_gnn_blocks,
            aggr=aggregation_over_edge_types,
        )
        self.regressor = GatedRegression(
            input_dim=self.encoder.output_dim,  # GNNEncoder returns init node embedding and GNN outputs stacked together
            hidden_dim=regressor_hidden_dim,
            output_dim=num_time_heads,
            dropout=regressor_dropout,
            pooling=global_pooling,
        )
        self._loss_str = loss

        self._lr = lr

    def init_optimizer_and_loss(self):
        self._optimizer = torch.optim.Adam(self.parameters(), lr=self._lr)
        self._loss_func = loss_func_from_str(loss_str=self._loss_str)

    @property
    def num_edge_types(self) -> int:
        return self.encoder.num_edge_types

    def forward(
        self, batch: BatchedGraphData
    ) -> Tensor:  # Shape [num_graphs, num_time_heads]
        """
        Predict values for a batch of graphs.

        Args:
            batch: graph representations of expressions.

        Returns:
            torch.Tensor of shape [num_graphs, num_time_heads] with predictions for all 
            times left for each graph
        """
        x = self.encoder(node_type=batch.node_type, edge_lists=batch.edge_lists,)
        return self.regressor(x=x, graph_id=batch.graph_id)
