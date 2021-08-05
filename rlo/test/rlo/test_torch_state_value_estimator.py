# pylint: disable=redefined-outer-name
import pytest

from rlo.model.state_value_estimator import StateValueModel
from test_torch_layers import get_graph_batch
from rlo.expression import Expression
from rlo.graph_data import EdgeType
from rlo import utils


def get_state_value_estimator(num_time_heads, seed: int = 0):
    with utils.random_state_context(seed):
        return StateValueModel(
            num_node_types=Expression.num_node_types,
            gnn_hidden_dim=3,
            num_edge_types=EdgeType.num_edge_types(use_subtree_match_edges=False),
            num_gnn_blocks=2,
            regressor_hidden_dim=13,
            num_time_heads=num_time_heads,
            loss="pinball=0.9",
            lr=0.001,
        )


@pytest.mark.parametrize(
    "batch_size,num_time_heads", [(1, 13), (10, 0), (10, 32), (9, 1)]
)
def test_output_shape(batch_size, num_time_heads):
    model = get_state_value_estimator(num_time_heads)
    batch = get_graph_batch(batch_size=batch_size, use_subtree_match_edges=False)
    prediction = model(batch)
    assert prediction.shape == (batch_size, num_time_heads)
