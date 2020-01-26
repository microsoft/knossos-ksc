import numpy as np

import ksc
from ksc.tracing.functions import math, nn

@ksc.trace
def dense(weights, x):
    W, b = weights
    return math.broadcast_add(math.dot(x, W), b)

@ksc.trace
def mlp(weights, x):
    weights1, weights2 = weights
    return dense(weights2, nn.relu(dense(weights1, x)))

def test_dense():
    W = np.random.normal(0, 1, (3, 4))
    b = np.random.normal(0, 1, (4,))
    x = np.random.normal(0, 1, (5, 3))
    out = dense((W, b), x)
    expected_output = np.dot(x, W) + b
    assert np.allclose(out.data, expected_output, rtol=1e-3, atol=1e-4)

def test_mlp():
    W1 = np.random.normal(0, 1, (3, 7))
    b1 = np.random.normal(0, 1, (7,))
    W2 = np.random.normal(0, 1, (7, 4))
    b2 = np.random.normal(0, 1, (4,))
    x = np.random.normal(0, 1, (5, 3))
    out = mlp(((W1, b1), (W2, b2)), x)
    h1 = np.maximum(np.dot(x, W1) + b1, 0.0)
    expected_output = np.dot(h1, W2) + b2
    assert np.allclose(out.data, expected_output, rtol=1e-3, atol=1e-4)
