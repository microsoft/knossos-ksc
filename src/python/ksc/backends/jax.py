try:
  import jax
  import jax.numpy as np
  import jax.experimental.stax as stax
except ModuleNotFoundError:
  import numpy as np

from ksc.backends.common import *

def broadcast_add(x, b):
  return x + b[None, :]

def dot(x, y):
  return np.dot(x, y)

def transpose(x):
  return x.T

def relu(x):
  return np.maximum(x, 0.0)

def log_softmax(x):
  x_max = np.amax(x, axis=-1, keepdims=True)
  return (x - x_max) - np.log(np.exp(x - x_max).sum(axis=-1, keepdims=True))

def _conv_2d_no_bias_factory(padding):
  def conv_2d(x, weights, strides):
    y = jax.lax.conv_general_dilated(
      x,
      weights,
      strides,
      padding,
      dimension_numbers=('NCHW', 'OIHW', 'NCHW') # the same as pytorch / onnx
    )
    print(f"conv_2d shape: {y.shape}")
    return y
  return conv_2d

conv_2d_no_bias_same = _conv_2d_no_bias_factory("SAME")
conv_2d_no_bias_valid = _conv_2d_no_bias_factory("VALID")

def normalize_2d(x, weights):
  mean, sigma = weights
  return (x - mean[:, None, None]) / sigma[:, None, None]

def batch_norm_2d(x, weights):
  mean, var, gamma, beta = weights
  sigma = np.sqrt(var + 1e-5)
  z = normalize_2d(x, (mean, sigma))
  return gamma[:, None, None] * z + beta[:, None, None]

def to_float(x):
  if hasattr(x, "astype"):
    return x.astype(np.float32)
  else:
    return float(x)

def _pooling_factory(pool_type, padding):
  def pooling(x, pool_size, strides):
    f = getattr(stax, pool_type)
    _, apply_fn = f(pool_size, strides, padding=padding, spec='NCHW')
    y = apply_fn((), x)
    return y
  return pooling

max_pool_same = _pooling_factory("MaxPool", "SAME")
avg_pool_valid = _pooling_factory("AvgPool", "VALID")

def flatten(x):
  b = x.shape[0]
  return x.reshape((b, -1))

def Add(x, y):
  return x + y

def ones(len):
  return np.ones(len)

def ones_2d(len1, len2):
  return np.ones((len1, len2))
