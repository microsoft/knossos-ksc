# Legacy tensorflow losses
from typing import Callable
import tensorflow as tf


def unreduced_huber(y_true, y_pred, delta=1.0):
    """Based on `tf.keras.losses.huber` but without the reduction."""
    error = y_pred - y_true
    abs_error = tf.math.abs(error)
    clipped_error = tf.math.minimum(abs_error, delta)
    linear = abs_error - clipped_error
    return tf.math.square(clipped_error) / 2 + delta * linear


def unreduced_pinball(y_true: tf.Tensor, y_pred: tf.Tensor, tau: float) -> tf.Tensor:
    """
    Based on `tfa.losses.pinball_loss`, but doesn't reduce across the batch dimensions, allowing for
    taking a weighted average down-stream.
    
    

    """
    return tf.math.maximum(tau * (y_true - y_pred), (tau - 1) * (y_true - y_pred))


class WeightedHuber(tf.keras.losses.Loss):
    """
    `unreduced_huber` loss wrapper.

    Unlike `tf.keras.losses.Huber`, this does not reduce over the final dimension until
    after weighting, allowing for weights with rank > 1.

    This weighting is handled by the parent class's __call__() method which optionally takes
    sample weights as arguments.
    """

    def __init__(
        self,
        name: str = "weighted_huber",
        delta: float = 1.0,
        reduction: str = tf.keras.losses.Reduction.SUM,
    ):
        self.delta = delta
        super().__init__(name=name, reduction=reduction)

    def get_config(self):
        config = super().get_config()
        config["delta"] = self.delta
        return config

    def call(self, y_true, y_pred):
        return unreduced_huber(y_true, y_pred, delta=self.delta)


class SquaredDifference(tf.keras.losses.Loss):
    """
    `tf.math.squared_difference` wrapper.

    Unlike `tf.keras.losses.MeanSquaredError`, this does not reduce over the final
    dimension until after weighting, allowing for weights with rank > 1.

    This weighting is handled by the parent class's __call__() method which optionally takes
    sample weights as arguments.
    """

    def __init__(
        self,
        name: str = "squared_difference",
        reduction: str = tf.keras.losses.Reduction.SUM_OVER_BATCH_SIZE,
    ):
        super().__init__(name=name, reduction=reduction)

    def call(self, y_true, y_pred):
        return tf.math.squared_difference(y_true, y_pred)


class Pinball(tf.keras.losses.Loss):
    """
    Wrapper around the pinball loss function. Doesn't reduce across the batch dimension in call()
    method, allowing for taking a weighted average down-stream. This weighting is handled by the
    parent class's __call__() method, which takes sample weights as an optional argument.
    
    The loss function can be interpreted as matching the tau-th quantile to the random variable:
        When tau is 0, you are only penalised for over-estimating y_true.
        When tau is 1, you are only penalised for under-estimating y_true.
    """

    def __init__(
        self,
        name: str = "pinball",
        tau: float = 0.5,
        reduction: str = tf.keras.losses.Reduction.SUM,
    ):
        if not 0 <= tau <= 1.0:
            raise ValueError("Pinbal tau parameter must be between 0 and 1.")
        self._tau = tau
        super().__init__(name=name, reduction=reduction)

    def get_config(self):
        config = super().get_config()
        config["tau"] = self._tau
        return config

    def call(self, y_true: tf.Tensor, y_pred: tf.Tensor) -> tf.Tensor:
        return unreduced_pinball(y_true, y_pred, tau=self._tau)


class MeanLossWrapper(tf.keras.losses.Loss):
    """
    TODO(t-mbruno): This class and all the derived classes seem redundant -> they are not used
    anywhere in the code.

    Wrapper class for implementing weighted mean across each batch.

    The weighted mean is defined as
    `sum(fn(y_true, y_pred) * sample_weights) / sum(sample_weight)`.

    Note this allows for `sample_weights` with any shape broadcastable to fn output.

    This is the loss equivalent of `tfa.metrics.MeanMetricWrapper`.
    """

    def __init__(
        self, fn: Callable[[tf.Tensor, tf.Tensor], tf.Tensor], name: str, **kwargs
    ):
        self._fn = tf.keras.utils.deserialize_keras_object(fn)
        self._name = name
        self._kwargs = kwargs
        super().__init__(name=name, reduction=tf.keras.losses.Reduction.NONE)

    def get_config(self):
        config = super().get_config()
        del config["reduction"]
        config.update(
            dict(fn=tf.keras.utils.serialize_keras_object(self._fn), **self._kwargs)
        )
        return config

    def call(self, y_true, y_pred):
        return self._fn(y_true, y_pred, **self._kwargs)

    def __call__(self, y_true, y_pred, sample_weight=None):
        loss = self.call(y_true, y_pred)  # unreduced
        if sample_weight is None:
            return tf.reduce_mean(loss)
        return tf.reduce_sum(loss * sample_weight) / tf.reduce_sum(sample_weight)


class MeanHuber(MeanLossWrapper):
    """
    TODO(t-mbruno): Not used anywhere in code.

    Similar to `tf.keras.losses.Huber` with weighted averaging.

    Compared to `tf.keras.losses.Huber` this has two differences:
        * if given, weights are applied before reducing over the final dimension; and
        * reduction is a possibly weighted average.
    """

    def __init__(self, name: str = "weighted_huber", delta: float = 1.0):
        super().__init__(name=name, delta=delta, fn=unreduced_huber)


class MeanSquaredDifference(MeanLossWrapper):
    """
    TODO(t-mbruno): Not used anywhere in code.

    Similar to `tf.keras.losses.MeanSquaredError` with weighted averaging.

    Compared to `tf.keras.losses.MeanSquaredError` this has two differences:
        * if given, weights are applied before reducing over the final dimension; and
        * reduction is a possibly weighted average.
    """

    def __init__(self, name="weighted_squared_difference"):
        super().__init__(name=name, fn=tf.math.squared_difference)
