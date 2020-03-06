try:
  import jax
  import jax.numpy as np
  import jax.experimental.stax as stax
except ModuleNotFoundError:
  import numpy as np

# Use relative import to work around a python 3.6 issue
# https://stackoverflow.com/questions/57615877/importing-difference-in-python3-6-and-python3-7
from . import common
from .common import *

_built_ins = common._built_ins

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

def conv_2d_no_bias(x, weights, ksizes, strides, paddings):
  y = jax.lax.conv_general_dilated(
    x,
    weights,
    strides,
    paddings,
    dimension_numbers=('NCHW', 'OIHW', 'NCHW') # the same as pytorch / onnx
  )
  print(f"conv_2d shape: {y.shape}")
  return y

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

# This is a bit silly but jax does not have an API
# to provide the precise padding sizes for pooling layers
def max_pool(x, pool_size, strides, paddings):
  if paddings == ((0, 0), (0, 0)):
    return max_pool_valid(x, pool_size, strides)
  else:
    return max_pool_same(x, pool_size, strides)

def avg_pool(x, pool_size, strides, paddings):
  if paddings == ((0, 0), (0, 0)):
    return avg_pool_valid(x, pool_size, strides)
  else:
    return avg_pool_same(x, pool_size, strides)

def flatten(x):
  b = x.shape[0]
  return x.reshape((b, -1))
