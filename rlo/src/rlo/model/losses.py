from typing import Callable, Optional
import functools

import torch
from torch.nn.functional import smooth_l1_loss


# LossFunc accepts prediction, target, and optionally weights, and returns tensor loss
LossFunc = Callable[[torch.Tensor, torch.Tensor, Optional[torch.Tensor]], torch.Tensor]


def unreduced_pinball_loss(
    pred: torch.Tensor, target: torch.Tensor, tau: float
) -> torch.Tensor:
    """Pinball loss parameterised by parameter tau in [0, 1]. 

    Equivalent to:
        L(pred, target) = (target - pred) * tau             if pred <= target
                          (pred - target) * (1 - tau)       if pred > target

    When the expectation of this loss is minimised, the prediction will be an overestimate of the target
    a fraction tau of the time.  For example, if the target is a value function and tau is nearly 1, 
    then the predictions are usually optimistic.

    Args:
        pred: Tensor of predictions
        target: Tensor of targets of same shape as pred
        tau: The tau parameter between 0 and 1.

    Returns:
        The unreduced loss of same shape as prediction and target.
    """
    deviation = pred - target
    slope = torch.where(pred <= target, -tau, (1 - tau))
    return deviation * slope


def loss_func_from_str(loss_str: str) -> LossFunc:
    """Factory function to create a loss function from string specifier.
    Specifier can be 'huber' or something of the form 'pinball=X' where X is 
    the pinball tau parameter.
    
    The returned callable takes (prediction, target) or (prediction, target, weights) and returns a scalar.
    """
    if loss_str == "huber":
        unreduced_loss_func = functools.partial(
            smooth_l1_loss, reduction="none", beta=1.0
        )
    else:
        # Loss should be of the form `pinball=0.x`
        _pinball, tau_str = loss_str.split("=")
        assert _pinball == "pinball"
        tau = float(tau_str)
        assert 0.0 <= tau <= 1.0
        unreduced_loss_func = functools.partial(unreduced_pinball_loss, tau=tau)

    return functools.partial(weighted_loss, unreduced_loss_func=unreduced_loss_func)


def weighted_loss(
    prediction: torch.Tensor,
    target: torch.Tensor,
    weight: Optional[torch.Tensor] = None,
    *,
    unreduced_loss_func: Callable
) -> torch.Tensor:
    if weight is None:
        return torch.sum(unreduced_loss_func(prediction, target))
    else:
        return torch.sum(unreduced_loss_func(prediction, target) * weight)
