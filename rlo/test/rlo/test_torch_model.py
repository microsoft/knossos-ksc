# pylint: disable=redefined-outer-name
import tempfile
import os

import numpy as np
import pytest
import torch

from test_torch_state_value_estimator import get_state_value_estimator
from rlo.model.model import TorchModelWrapper, OptimizerAndLossNotInitialized
from rlo.torch_graph_data import DataConverter
from testutils import parse_expr_typed


@pytest.fixture
def expr_batch():
    return [parse_expr_typed("(let ((a (add x 3.0))) (add (mul a a) y))")]


def get_model_wrapper(seed, num_time_heads):
    model = get_state_value_estimator(num_time_heads=num_time_heads, seed=seed)
    data_converter = get_data_converter(num_time_heads)
    model.eval()
    return TorchModelWrapper(
        data_converter=data_converter, model=model, device=torch.device("cpu")
    )


def get_data_converter(num_time_heads):
    return DataConverter(
        num_time_heads=num_time_heads,
        cost_norm="none",
        use_subtree_match_edges=False,
        device=torch.device("cpu"),
    )


def test_different_seed_gives_different_model(expr_batch):
    with get_model_wrapper(seed=10, num_time_heads=12) as model_wrapper:
        res = model_wrapper.evaluate_all_time_left(expr_batch)
    with get_model_wrapper(seed=2, num_time_heads=12) as model_wrapper:
        # Model with another random seed should behave differently
        assert (model_wrapper.evaluate_all_time_left(expr_batch) != res).any()


def test_same_seed_gives_same_model(expr_batch):
    with get_model_wrapper(seed=10, num_time_heads=12) as model_wrapper:
        res = model_wrapper.evaluate_all_time_left(expr_batch)
    with get_model_wrapper(seed=10, num_time_heads=12) as model_wrapper:
        np.testing.assert_allclose(
            model_wrapper.evaluate_all_time_left(expr_batch), res, rtol=1e-6
        )


def dummy_step(model_wrapper):
    param = model_wrapper.model.optimizer.param_groups[0]["params"][0]
    loss = torch.sum(param)
    loss.backward()
    model_wrapper.model.optimizer.step()


def assert_optimizer_has_stepped(model_wrapper, i):
    # Asserts the optimizer looks like it has been stepped i times
    if i == 0:
        assert model_wrapper.model.optimizer.state_dict()["state"] == {}
    else:
        print(model_wrapper.model.optimizer.state_dict())
        assert model_wrapper.model.optimizer.state_dict()["state"][0]["step"] == i


def test_evalution_is_deterministic(expr_batch):
    # Evaluate the same batch twice
    with get_model_wrapper(seed=10, num_time_heads=12) as model_wrapper:
        res = model_wrapper.evaluate_all_time_left(expr_batch)
        np.testing.assert_allclose(
            model_wrapper.evaluate_all_time_left(expr_batch), res, rtol=1e-6
        )


def test_optim_step_changes_model(expr_batch):
    # Step optimizer and check evaluation changes.
    with get_model_wrapper(seed=10, num_time_heads=12) as model_wrapper:
        model_wrapper.model.init_optimizer_and_loss()
        res_pre_training = model_wrapper.evaluate_all_time_left(expr_batch)
        assert_optimizer_has_stepped(model_wrapper, 0)
        dummy_step(model_wrapper)
        assert_optimizer_has_stepped(model_wrapper, 1)
        assert (
            model_wrapper.evaluate_all_time_left(expr_batch) != res_pre_training
        ).any()


def test_training_before_init_raises():
    # Attempts to evaluate loss before initializing optimizer and loss should fail with a meaningful error.
    with get_model_wrapper(seed=10, num_time_heads=3) as model_wrapper:
        with pytest.raises(OptimizerAndLossNotInitialized):
            model_wrapper.model.evaluate_training_loss(None)
        with pytest.raises(OptimizerAndLossNotInitialized):
            model_wrapper.model.evaluate_validation_loss(None)


def test_save_restore_model_state(expr_batch):
    num_time_heads = 13
    with tempfile.TemporaryDirectory() as tmpdir:
        npzfile = os.path.join(tmpdir, "test.npz")
        with get_model_wrapper(seed=10, num_time_heads=num_time_heads) as model_wrapper:
            model_wrapper.model.init_optimizer_and_loss()
            # Save weights for model with one random seed, after one step of training
            res_pre_training = model_wrapper.evaluate_all_time_left(expr_batch)
            dummy_step(model_wrapper)
            res = model_wrapper.evaluate_all_time_left(expr_batch)
            assert_optimizer_has_stepped(model_wrapper, 1)
            weights = model_wrapper.get_weights()
            weights.save(npzfile)
        with get_model_wrapper(seed=2, num_time_heads=num_time_heads) as model_wrapper:
            # Load model state from file, check optimizer state and weights are restored
            model_wrapper.model.init_optimizer_and_loss()
            assert (
                model_wrapper.evaluate_all_time_left(expr_batch) != res_pre_training
            ).any()
            assert_optimizer_has_stepped(model_wrapper, 0)
            model_wrapper.load_weights(npzfile)
            np.testing.assert_allclose(
                model_wrapper.evaluate_all_time_left(expr_batch), res, rtol=1e-6
            )
            assert_optimizer_has_stepped(model_wrapper, 1)
        with get_model_wrapper(seed=2, num_time_heads=num_time_heads) as model_wrapper:
            # Load model state from memory, check optimizer state and weights are restored
            assert (model_wrapper.evaluate_all_time_left(expr_batch) != res).any()
            model_wrapper.model.init_optimizer_and_loss()
            assert_optimizer_has_stepped(model_wrapper, 0)
            model_wrapper.set_weights(weights)
            np.testing.assert_allclose(
                model_wrapper.evaluate_all_time_left(expr_batch), res
            )
            assert_optimizer_has_stepped(model_wrapper, 1)
        with get_model_wrapper(
            seed=0, num_time_heads=num_time_heads + 29
        ) as model_wrapper:
            # Model with different num_time_heads is incompatible
            with pytest.raises(Exception):
                model_wrapper.load_weights(npzfile)
        with get_model_wrapper(
            seed=0, num_time_heads=num_time_heads + 29
        ) as model_wrapper:
            # Model with different num_time_heads is incompatible
            with pytest.raises(Exception):
                model_wrapper.set_weights(weights)
