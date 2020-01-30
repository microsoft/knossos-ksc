try:
  import jax
  import jax.numpy as np
  import jax.experimental.stax as stax
except ModuleNotFoundError:
  import numpy as np

from ksc.backends.common import *

def BroadcastAdd(x, b):
  return x + b[None, :]

def MatMul(x, y):
  return np.dot(x, y)

def Transpose(x):
  return x.T

def Relu(x):
  return np.maximum(x, 0.0)

def LogSoftmax(x):
  x_max = np.amax(x, axis=-1, keepdims=True)
  return (x - x_max) - np.log(np.exp(x - x_max).sum(axis=-1, keepdims=True))

def Conv2DNoBias(filters, kernel_size, strides, weight, x):
  return jax.lax.conv_general_dilated(
    x,
    weight,
    strides,
    'SAME',
    dimension_numbers=('NCHW', 'OIHW', 'NCHW') # the same as pytorch / onnx
  )

def Normalize2D(weights, x):
  mean, sigma = weights
  return (x - mean[:, None, None]) / sigma[:, None, None]

def BatchNorm2D(weights, x):
  mean, var, gamma, beta = weights
  sigma = np.sqrt(var + 1e-5)
  z = Normalize2D((mean, sigma), x)
  return gamma[:, None, None] * z + beta[:, None, None]

def ToFloat(x):
  return x.astype(np.float32)

def MaxPool(pool_size, strides, x):
  _, apply_fn = stax.MaxPool(pool_size, strides, spec='NCHW')
  return apply_fn((), x)

def AvgPool(pool_size, strides, x):
  _, apply_fn = stax.AvgPool(pool_size, strides, spec='NCHW')
  return apply_fn((), x)

def Flatten(x):
  b = x.shape[0]
  return x.reshape((b, -1))

def Add(x, y):
  return x + y

def ones(len):
  return np.ones(len)

def ones_2d(len1, len2):
  return np.ones((len1, len2))
