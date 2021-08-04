import pytest

from rlo import factory


@pytest.mark.parametrize("use_subtree_match_edges", [True, False])
@pytest.mark.parametrize("loss", ["pinball=0.6", "huber"])
def test_torch_model_from_config(use_subtree_match_edges, loss):
    # Check we can construct a Model
    config = {
        "num_embeddings": 3,
        "hidden_dim": 2,
        "num_gnn_blocks": 5,
        "output_hidden_dim": 2,
        "simulation_depth_train": 10,
        "lr": 0.01,
        "loss": loss,
        "repetition": 1,
        "decoder_readout": "sum",
        "graph_state_keep_prob": 0.9,
        "output_keep_prob": 0.2,
        "aggregation_over_edge_types": "sum",
        "use_subtree_match_edges": use_subtree_match_edges,
    }

    factory.torch_model_from_config(config)


@pytest.mark.parametrize("use_subtree_match_edges", [True, False])
def test_torch_data_converter_from_config(use_subtree_match_edges):
    # Check we can construct a DataConverter
    config = {
        "simulation_depth_train": 11,
        "use_subtree_match_edges": use_subtree_match_edges,
        "cost_normalization": "none",
    }
    factory.data_converter_from_config(config)


@pytest.mark.parametrize("use_subtree_match_edges", [True, False])
@pytest.mark.parametrize("loss", ["pinball=0.3", "huber"])
def test_torch_regressor_from_config(use_subtree_match_edges, loss):
    # Check we can construct a TorchModelWrapper
    config = {
        "num_embeddings": 3,
        "hidden_dim": 2,
        "num_gnn_blocks": 5,
        "output_hidden_dim": 2,
        "lr": 0.01,
        "loss": loss,
        "repetition": 1,
        "use_subtree_match_edges": use_subtree_match_edges,
        "cost_normalization": "none",
        "tensorflow": False,
        "simulation_depth_eval": 10,
        "decoder_readout": "sum",
        "graph_state_keep_prob": 0.99,
        "output_keep_prob": 0.2,
        "aggregation_over_edge_types": "sum",
        "simulation_depth_train": 10,
    }
    factory.single_regressor_from_config(config)
