"""
 A version of jax backend that expects arguments as (..., weights, input)
 used by resnet_v2.ks
"""
from ksc.backends.common import *
from ksc.backends.jax import (
    broadcast_add,
    dot,
    transpose,
    relu,
    log_softmax,
    to_float,
    flatten
)
import ksc.backends.jax as jax_backend

def conv_2d_no_bias(ksizes, strides, paddings, weights, x):
    return jax_backend.conv_2d_no_bias(x, weights, ksizes, strides, paddings)

def normalize_2d(weights, x):
    return jax_backend.normalize_2d(x, weights)

def batch_norm_2d(weights, x):
    return jax_backend.batch_norm_2d(x, weights)

def max_pool_same(pool_size, strides, x):
    return jax_backend.max_pool_same(x, pool_size, strides)

def avg_pool_valid(pool_size, strides, x):
    return jax_backend.avg_pool_valid(x, pool_size, strides)
