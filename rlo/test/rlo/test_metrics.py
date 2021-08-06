import tensorflow as tf
import numpy as np

from rlo.metrics import MeanHuber
from rlo import losses


def test_mean_huber():
    metric = MeanHuber()
    batch_size = 5
    num_features = 7
    x = tf.random.normal(shape=(batch_size, num_features))
    y = tf.random.normal(shape=(batch_size, num_features))
    w = tf.random.uniform(shape=(batch_size, num_features))

    metric.update_state(x, y, w)
    metric.update_state(x, y, w)
    actual = metric.result()
    expected = tf.reduce_sum(losses.unreduced_huber(x, y) * w) / tf.reduce_sum(w)
    np.testing.assert_equal(actual.numpy(), expected.numpy())
