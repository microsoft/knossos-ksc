import functools

import tensorflow as tf
import tensorflow_addons as tfa

from rlo import losses


class MeanHuber(tfa.metrics.MeanMetricWrapper):
    def __init__(self, name="mean_huber", dtype=tf.float32):
        # TODO(t-salewi) Huber loss in losses.py has a configurable 'delta' parameter; here it is fixed at 1.
        # Do we want it to be configurable here too?
        super().__init__(fn=losses.unreduced_huber, name=name, dtype=dtype)


class MeanPinball(tfa.metrics.MeanMetricWrapper):
    def __init__(self, name="mean_pinball", dtype=tf.float32, tau: float = 0.5):

        super().__init__(
            fn=functools.partial(losses.unreduced_pinball, tau=tau),
            name=name,
            dtype=dtype,
        )


class TotalWeight(tf.keras.metrics.Metric):
    def __init__(self, name="total_weight", dtype=tf.float32):
        super().__init__(name=name, dtype=dtype)
        self.total = self.add_weight(name="total_weight", shape=(), dtype=dtype)

    def update_state(self, y_labels, y_pred, sample_weight=None):
        if sample_weight is None:
            raise ValueError("TotalWeight undefined in sample_weight is None")
        self.total.assign_add(tf.reduce_sum(sample_weight))

    def result(self):
        return self.total
