from functools import partial

import numpy as np
import tensorflow as tf
import tensorflow_addons as tfa
import pytest

from rlo import losses


def random_pred_test_data(size: int, seed: int = 4321):
    """Return a tuple of random tensors representing test and pred. data"""
    y_pred = tf.random.stateless_normal((size,), seed=(seed, 0))
    y_test = tf.random.stateless_normal((size,), seed=(seed, 1))
    return y_pred, y_test


def test_unreduced_huber__same_as_tensorflow():
    check_same_as_tensorflow(losses.unreduced_huber, tf.keras.losses.Huber())


@pytest.mark.parametrize("tau", (0.01, 0.3, 0.5, 0.9))
def test_unreduced_pinball__same_as_tensorflow(tau):
    check_same_as_tensorflow(
        partial(losses.unreduced_pinball, tau=tau), tfa.losses.PinballLoss(tau=tau)
    )


def check_same_as_tensorflow(ours, theirs):
    """Test our "unreduced" implementation (ours) gives the same value as the tensorflow implementation (theirs) after averaging"""
    size = 1024
    y_pred, y_test = random_pred_test_data(size)
    actual = tf.reduce_mean(ours(y_pred, y_test))
    expected = theirs(y_pred, y_test)
    np.testing.assert_allclose(actual.numpy(), expected.numpy(), rtol=1e-5)
