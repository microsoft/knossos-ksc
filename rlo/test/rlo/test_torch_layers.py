# pylint: disable=redefined-outer-name, no-self-use
import pytest
import torch
from torch.nn.functional import one_hot
from typing import Any, Dict, List, Tuple

from rlo import sparser
from rlo.torch_graph_data import DataConverter, BatchedGraphData
from rlo.model.layers import (
    GNNEncoder,
    RelationalGatedGraphConv,
    StackedRelationalGNN,
    GatedRegression,
)
from rlo.pipelines.graph_pipeline import EdgeType
from rlo.expression import Expression
from rlo.expression_util import SymtabAndDefs
from ksc.type import Type
from rlo.utils import random_state_context

from testutils import load_dataset


def parse_exprs(dataset, batch_size):
    return [
        SymtabAndDefs({"x": Type.Float, "y": Type.Float}).make_toplevel(
            sparser.parse_expr(expr_str)
        )
        for _, expr_str, _ in dataset[:batch_size]
    ]


def as_one_hot(node_type: torch.LongTensor) -> torch.FloatTensor:
    return one_hot(node_type, num_classes=get_node_representation_dim()).float()


@pytest.fixture(params=[True, False])
def use_subtree_match_edges(request) -> bool:
    return request.param


def get_graph_batch(batch_size: int, use_subtree_match_edges: bool) -> BatchedGraphData:
    """Get a batch of converted Torch graph data for testing"""
    dataset = load_dataset("datasets/value_dataset.json")
    exprenvs = parse_exprs(dataset, batch_size)
    assert (
        len(exprenvs) == batch_size
    )  # TODO this might fail if batch_size > len(dataset)
    data_converter = DataConverter(
        use_subtree_match_edges=use_subtree_match_edges,
        num_time_heads=300,
        cost_norm="none",
        device=torch.device("cpu"),
    )
    graph_data_list = [data_converter.expr_to_graph(e) for e in exprenvs]
    return BatchedGraphData.collate(graph_data_list)


@pytest.fixture
def node_representation_dim() -> int:
    """
    A valid hidden dimension for the GNN. It must be greater than number of node types for 
    one-hot encoding to work.
    """
    return get_node_representation_dim()


def get_node_representation_dim():
    return Expression.num_node_types + 3


def assert_forward_results_depend_on_random_state(
    model: torch.nn.Module, forward_kwargs: Dict[str, Any],
) -> None:
    """
    Checks that
    - forward passes with different seeds give different results (because the model uses dropout).
    - forward passes with the same seed give the same result.
    """
    with random_state_context(5):
        outputs1 = model(**forward_kwargs)
    with random_state_context(1000000):
        outputs2 = model(**forward_kwargs)
    with random_state_context(5):
        outputs3 = model(**forward_kwargs)
    # Expecting: outputs1 == outputs3, but outputs1 != outputs2
    assert torch.all(
        outputs1 == outputs3
    ), "All outputs should be the same for same random seed"

    assert torch.any(
        outputs1 != outputs2
    ), "Some outputs should be different when using dropout"


class TestRelationalGatedGraphConv:
    @pytest.fixture
    def relational_gcn(
        self, node_representation_dim: int, use_subtree_match_edges: bool
    ):
        return RelationalGatedGraphConv(
            hidden_dim=node_representation_dim,
            num_edge_types=EdgeType.num_edge_types(
                use_subtree_match_edges=use_subtree_match_edges
            ),
            num_propagations=2,
            recurrent_dropout=0.2,
        )

    def test_outputs_expected_shape(
        self,
        relational_gcn: RelationalGatedGraphConv,
        node_representation_dim: int,
        use_subtree_match_edges: bool,
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)

        outputs = relational_gcn(
            node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists,
        )
        assert isinstance(outputs, torch.Tensor)
        assert outputs.shape == (batch.num_nodes, node_representation_dim)

    def test_eval_mode_not_stochastic(
        self, relational_gcn: RelationalGatedGraphConv, use_subtree_match_edges: bool
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)

        relational_gcn.eval()  # Set to eval mode (disable dropout if exists)
        outputs1 = relational_gcn(
            node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists,
        )
        outputs2 = relational_gcn(
            node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists,
        )
        assert torch.all(outputs1 == outputs2).detach().item()

    def test_reset_parameters__resets_parameters(
        self, relational_gcn: RelationalGatedGraphConv, use_subtree_match_edges: bool
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)

        relational_gcn.eval()  # Set to eval mode (disable dropout if exists)
        # Get predictions before resetting weights
        pre_reset_outputs = relational_gcn(
            node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists,
        )
        # Get parameters before resetting them and save them in a list of tuples with
        # parameter name and parameter tensor
        pre_reset_weights: List[Tuple[str, torch.Tensor]] = [
            (param_name, param.detach().clone())
            for (param_name, param) in relational_gcn.named_parameters()
        ]
        relational_gcn.reset_parameters()
        relational_gcn.eval()
        post_reset_outputs = relational_gcn(
            node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists,
        )
        post_reset_weights: List[Tuple[str, torch.Tensor]] = [
            (param_name, param.detach().clone())
            for (param_name, param) in relational_gcn.named_parameters()
        ]

        for (
            (pre_reset_param_name, pre_reset_param),
            (post_reset_param_name, post_reset_param),
        ) in zip(pre_reset_weights, post_reset_weights):
            assert pre_reset_param_name == post_reset_param_name
            if "bias" in pre_reset_param_name or "weight_hh" in pre_reset_param_name:
                # Biases and recurrent GRU weights are initialised deterministically
                continue
            # Assert at least some weights different (which would be expected after resetting)
            assert torch.any(pre_reset_param != post_reset_param)

        # Assert at least some outputs different (which would be expected after resetting)
        assert torch.any(pre_reset_outputs != post_reset_outputs)

    def test_dropout_consistent(
        self, relational_gcn: RelationalGatedGraphConv, use_subtree_match_edges: bool
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)
        relational_gcn.train()
        assert_forward_results_depend_on_random_state(
            model=relational_gcn,
            forward_kwargs=dict(
                node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists
            ),
        )


class TestStackedRelationalGNN:
    @pytest.fixture
    def stacked_relational_gcn(
        self, node_representation_dim: int, use_subtree_match_edges: bool
    ):
        return StackedRelationalGNN(
            hidden_dim=node_representation_dim,
            num_edge_types=EdgeType.num_edge_types(
                use_subtree_match_edges=use_subtree_match_edges
            ),
            num_propagations=2,
            recurrent_dropout=0.7,
            num_gnn_blocks=2,
        )

    def test_outputs_expected_shape(
        self,
        stacked_relational_gcn: StackedRelationalGNN,
        node_representation_dim: int,
        use_subtree_match_edges: bool,
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)
        outputs = stacked_relational_gcn(
            node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists,
        )
        assert isinstance(outputs, torch.Tensor)
        assert outputs.shape == (batch.num_nodes, node_representation_dim)

    def test_eval_mode_not_stochastic(
        self,
        stacked_relational_gcn: StackedRelationalGNN,
        use_subtree_match_edges: bool,
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)

        stacked_relational_gcn.eval()  # Set to eval mode (disable dropout if exists)
        outputs1 = stacked_relational_gcn(
            node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists,
        )
        outputs2 = stacked_relational_gcn(
            node_states=as_one_hot(batch.node_type), edge_lists=batch.edge_lists,
        )
        assert torch.all(outputs1 == outputs2).detach().item()


class TestGNNEncoder:
    """GNNEncoder converts a node index to one-hot or an embedding before applying GNN"""

    @pytest.fixture
    def gnn(self, node_representation_dim: int, use_subtree_match_edges: bool):
        return GNNEncoder(
            num_node_types=Expression.num_node_types,
            hidden_dim=node_representation_dim,
            num_edge_types=EdgeType.num_edge_types(
                use_subtree_match_edges=use_subtree_match_edges
            ),
            num_propagations=2,
            recurrent_dropout=0.2,
            num_gnn_blocks=2,
        )

    def test_outputs_expected_shape(
        self,
        gnn: GNNEncoder,
        node_representation_dim: int,
        use_subtree_match_edges: bool,
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)
        outputs = gnn(node_type=batch.node_type, edge_lists=batch.edge_lists,)
        assert gnn.output_dim == 2 * node_representation_dim
        assert isinstance(outputs, torch.Tensor)
        assert outputs.shape == (batch.num_nodes, gnn.output_dim)

    def test_eval_mode_not_stochastic(
        self, gnn: GNNEncoder, use_subtree_match_edges: bool
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)
        gnn.eval()  # Set to eval mode (disable dropout if exists)
        outputs1 = gnn(node_type=batch.node_type, edge_lists=batch.edge_lists,)
        outputs2 = gnn(node_type=batch.node_type, edge_lists=batch.edge_lists,)
        assert torch.all(outputs1 == outputs2).detach().item()


class TestGatedRegressor:
    @pytest.fixture(autouse=True)
    def configure(self):
        self.node_representation_dim = 15
        self.hidden_dim = 12
        self.num_time_heads = 5

        # An arbitrary embedding vector to imitate outputs from a GNN
        self.embedder = torch.nn.Embedding(
            num_embeddings=Expression.num_node_types,
            embedding_dim=self.node_representation_dim,
        )

    @pytest.fixture
    def gated_regressor(self):
        return GatedRegression(
            input_dim=self.node_representation_dim,
            hidden_dim=self.hidden_dim,
            output_dim=self.num_time_heads,
            dropout=0.5,
        )

    def test_outputs_expected_shape(
        self, gated_regressor: GatedRegression, use_subtree_match_edges: bool
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)

        node_features = self.embedder(batch.node_type)

        outputs = gated_regressor(x=node_features, graph_id=batch.graph_id)
        assert isinstance(outputs, torch.Tensor)
        assert outputs.shape == (batch.num_graphs, self.num_time_heads)

    def test_eval_mode_not_stochastic(
        self, gated_regressor: GatedRegression, use_subtree_match_edges: bool
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)

        node_features = self.embedder(batch.node_type)

        gated_regressor.eval()  # Set to eval mode (disable dropout if exists)
        outputs1 = gated_regressor(x=node_features, graph_id=batch.graph_id)
        outputs2 = gated_regressor(x=node_features, graph_id=batch.graph_id)
        assert torch.all(outputs1 == outputs2).detach().item()

    def test_dropout_consistent(
        self, gated_regressor: GatedRegression, use_subtree_match_edges: bool
    ):
        batch_size = 10
        batch = get_graph_batch(batch_size, use_subtree_match_edges)
        gated_regressor.train()

        assert_forward_results_depend_on_random_state(
            model=gated_regressor,
            forward_kwargs=dict(
                x=self.embedder(batch.node_type), graph_id=batch.graph_id
            ),
        )


@pytest.mark.parametrize("batch_size", [1, 10])
def test_gnn_encoder_and_regressor__end_to_end__outputs_expected_shape(
    batch_size: int, use_subtree_match_edges: bool, node_representation_dim: int,
):
    num_time_heads = 7
    batch = get_graph_batch(batch_size, use_subtree_match_edges)
    encoder = GNNEncoder(
        num_node_types=node_representation_dim,
        hidden_dim=node_representation_dim,
        num_edge_types=EdgeType.num_edge_types(
            use_subtree_match_edges=use_subtree_match_edges
        ),
        recurrent_dropout=0.3,
        num_propagations=1,
        num_gnn_blocks=3,
        aggr="mean",
    )
    regressor = GatedRegression(
        input_dim=encoder.output_dim,
        hidden_dim=5,
        output_dim=num_time_heads,
        dropout=0.2,
    )
    encoded = encoder(node_type=batch.node_type, edge_lists=batch.edge_lists,)
    regressed_values = regressor(encoded, graph_id=batch.graph_id)
    assert regressed_values.shape == (batch_size, num_time_heads)
