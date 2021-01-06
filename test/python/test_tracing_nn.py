import pytest
import numpy as np

import ksc
from ksc.tracing.functions import math, nn
from ksc.shape import Shape, TensorShape, ScalarShape


@pytest.mark.parametrize(
    "kernel_size,strides,padding,expected_shape", [
        ((3, 3), (1, 1), "SAME", (1, 64, 56, 56)),
        ((3, 3), (2, 2), "SAME", (1, 64, 28, 28)),
        ((3, 3), (1, 1), "VALID", (1, 64, 54, 54)),
        ((3, 3), (2, 2), "VALID", (1, 64, 27, 27))
    ]
)
def test_conv_2d_no_bias(kernel_size, strides, padding, expected_shape):
    x = np.random.uniform(0, 1, (1, 64, 56, 56))
    weights = np.random.normal(0, 1, (64, 64) + kernel_size)
    y = nn.conv_2d_no_bias(x, weights, kernel_size, strides, padding=padding)
    assert y.data.shape == expected_shape

@pytest.mark.parametrize(
    "pooling", [nn.max_pool, nn.avg_pool])
@pytest.mark.parametrize(
    "padding,expected_shape", [("VALID", (1, 64, 55, 55)), ("SAME", (1, 64, 56, 56))]
)
def test_pooling(pooling, padding, expected_shape):
    x = np.random.uniform(0, 1, (1, 64, 112, 112))
    y = pooling(x, (3, 3), (2, 2), padding=padding) # padding needs to be a keyword argument
    print(y.shape_type)
    assert y.shape_type.shape == TensorShape(expected_shape, ScalarShape)
