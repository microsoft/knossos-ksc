from typing import Tuple

import numpy as np
import torch
import pytest

from rlo.model.losses import (
    loss_func_from_str,
    unreduced_pinball_loss,
)
from rlo import utils

# pylint: disable=not-callable


def random_pred_test_data(size: Tuple[int, ...] = (10, 10), seed: int = 4321):
    """Return a tuple of random tensors representing test and pred. data"""
    with utils.random_state_context(seed):
        pred = torch.rand(size)
        target = torch.rand(size)
    return pred, target


@pytest.mark.parametrize("size", ((5,), (2, 1), (10, 10), (5, 4, 6)))
def test_unreduced_pinball_expected_shape(size: Tuple[int, ...]):

    pred, target = random_pred_test_data(size=size)
    loss = unreduced_pinball_loss(pred, target, tau=0.4)
    assert loss.shape == torch.Size(size)


@pytest.mark.parametrize(
    "loss_str", ("pinball=0.3", "huber"),
)
@pytest.mark.parametrize("size", ((5,), (2, 1), (10, 10), (5, 4, 6)))
def test_reduced_losses_expected_shape(size: Tuple[int, ...], loss_str: str):
    pred, target = random_pred_test_data(size=size)
    loss = loss_func_from_str(loss_str)(pred, target, None)
    assert loss.shape == torch.Size([])


@pytest.mark.parametrize(
    "loss_str", ("pinball=0.5", "huber"),
)
def test_losses_always_nonnegative(loss_str: str):
    pred, target = random_pred_test_data()
    loss = loss_func_from_str(loss_str)(pred, target, None)
    assert np.all(
        loss.detach().numpy() >= 0.0
    ), "loss function should return non-negative values"


@pytest.mark.parametrize("tau", (0.0, 0.01, 0.3, 0.5, 0.9, 1.0))
def test_unreduced_pinball_loss__expected_on_overestimates(tau: float):
    target = torch.tensor(np.zeros(5), dtype=torch.float)
    overestimate = torch.tensor([0.0, 1.0, 2.0, 3.0, 4.0], dtype=torch.float)
    # Loss is linear with gradient (1 - tau) for overestimates
    expected_loss = overestimate * (1 - tau)

    # Calculated loss should exactly equal expected
    assert torch.equal(
        unreduced_pinball_loss(pred=overestimate, target=target, tau=tau), expected_loss
    )


@pytest.mark.parametrize("tau", (0.0, 0.01, 0.3, 0.5, 0.9, 1.0))
def test_unreduced_pinball_loss__expected_on_underestimates(tau: float):
    target = torch.tensor(np.zeros(5), dtype=torch.float)
    underestimate = torch.tensor([0.0, -1.0, -2.0, -3.0, -4.0], dtype=torch.float)

    # Loss is linear with gradient tau for underestimates of the target
    expected_loss = torch.abs(underestimate * tau)
    calculated_loss = unreduced_pinball_loss(pred=underestimate, target=target, tau=tau)

    # Calculated loss should exactly equal expected
    assert pytest.approx(expected_loss.numpy(), rel=1e-6, abs=0) == calculated_loss


@pytest.mark.parametrize("loss_str", ("huber", "pinball=0.5", "pinball=0.3"))
def test_weighted_loss_with_uniform_weights_same_as_unweighted(loss_str):
    size = (1024,)
    pred, target = random_pred_test_data(size=size)
    loss_func = loss_func_from_str(loss_str)
    loss_no_weights = loss_func(pred, target)
    loss_unit_weights = loss_func(pred, target, torch.ones_like(pred))
    assert pytest.approx(loss_no_weights, rel=1e-6, abs=0) == loss_unit_weights


@pytest.mark.parametrize("loss_str", ("pinball=0.6", "huber", "pinball=0.2"))
def test_weighted_loss__zero_masks(loss_str: str):
    """
    Generating two complementary zero-masks (e.g. weights1=[a, 0, 0] and weights2=[0, b, c])
    should give two losses that add up to that of `weights=[a, b, c]`
    """
    weighted_loss_func = loss_func_from_str(loss_str)
    size = (1024,)
    pred, target = random_pred_test_data(size=size)

    torch.manual_seed(4)
    weights = torch.abs(torch.rand(size))
    # Generate a mask and its complement
    mask1 = torch.randint_like(weights, low=0, high=2)
    mask2 = 1 - mask1

    weighted_loss1 = weighted_loss_func(pred, target, weights * mask1)
    weighted_loss2 = weighted_loss_func(pred, target, weights * mask2)
    total_weighted_loss = weighted_loss_func(pred, target, weights)
    assert (
        pytest.approx(weighted_loss1.numpy() + weighted_loss2.numpy(), rel=1e-4, abs=0)
        == total_weighted_loss.numpy()
    )


@pytest.mark.parametrize("loss_str", ("pinball=0.5", "huber", "pinball=0.4"))
def test_weighted_loss__gradients_for_0_weight_0(loss_str: str):
    """If the sample weights are zero, gradients of loss w.r.t. predictions should be non-zero"""
    weighted_loss_func = loss_func_from_str(loss_str)
    size = (1024,)
    pred, target = random_pred_test_data(size=size)
    pred = pred.clone().detach().requires_grad_(True)

    torch.manual_seed(4)
    weights = torch.randint_like(target, low=0, high=2)

    weighted_loss = weighted_loss_func(pred, target, weights)
    weighted_loss.backward()

    # All gradients where weights == 0 are 0
    assert np.all(pred.grad[torch.where(weights == 0)].numpy() == 0)


@pytest.mark.parametrize("loss_str, tau", [("pinball=0.5", 0.5), ("pinball=0.3", 0.3)])
@pytest.mark.parametrize("use_weights", [True, False])
def test_pinball_gradients(loss_str, tau, use_weights):
    size = (1024,)
    _, target = random_pred_test_data(size=size)

    n = 10  # underestimate the first n, overestimate the rest
    pred = torch.zeros_like(target)
    pred[:n] = target[:n] - 1  # Underestimates
    pred[n:] = target[n:] + 1
    pred.requires_grad_(True)

    if use_weights:
        weights = target ** 2
        loss = loss_func_from_str(loss_str)(pred, target, weights)
        loss.backward()
        assert torch.all(pred.grad[:n] == -tau * weights[:n])
        assert torch.all(pred.grad[n:] == (1 - tau) * weights[n:])
    else:
        loss = loss_func_from_str(loss_str)(pred, target)
        loss.backward()
        assert torch.all(pred.grad[:n] == -tau)
        assert torch.all(pred.grad[n:] == 1 - tau)
