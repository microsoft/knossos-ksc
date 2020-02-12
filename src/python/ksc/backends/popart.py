# pylint: disable=no-member
import numpy as np
import popart

# from ksc.backends.common import *


# NOTE: This module has mutable state:

# Common builder object
builder = popart.Builder()
# Input operators and input data
inputs = {}


def _np_to_popart_tensor(x):
  '''Convert a numpy array to a popart tensor.'''
  x_typ = type(x)
  if x_typ == np.ndarray:
    typs = {
      np.dtype('float32'): 'FLOAT',
      np.dtype('float64'): 'FLOAT',
      np.dtype('int32'): 'INT32',
    }
    t = builder.addInputTensor(popart.TensorInfo(typs[x.dtype], x.shape))
    if x.dtype == np.dtype('float64'):
      x = x.astype(np.float32)  # TODO maybe not do this
    inputs[t] = x
  elif x_typ == str:
    # TODO need a better way to identify the output of popart operations!
    t = x
  else:
    raise Exception('Got unrecognized type: ' + str(x_typ))
  return t

def _run_test(model, inputs):
  '''Run `model` on the IPU model for debugging the output.'''
  # Popart set-up
  builder = popart.Builder()
  opts = popart.SessionOptions()

  # Construct model
  in_ops, out_ops = model(builder)
  inputs = dict(zip(in_ops, inputs))

  # Run the model
  anchor_map = {o: popart.AnchorReturnType("ALL") for o in out_ops}
  device = popart.DeviceManager().createIpuModelDevice({'numIPUs': 1})
  session = popart.InferenceSession(
      fnModel=builder.getModelProto(),
      dataFeed=popart.DataFlow(1, anchor_map),
      userOptions=opts,
      deviceInfo=device,
  )
  session.prepareDevice()
  anchors = session.initAnchorArrays()
  stepio = popart.PyStepIO(inputs, anchors)
  session.run(stepio)

  return {o: anchors[o] for o in out_ops}

def _run_model(model):
  '''Run `model` on the IPU model.'''
  # Popart set-up
  opts = popart.SessionOptions()

  # Construct model
  out_ops = model()
  if not isinstance(out_ops, list) and not isinstance(out_ops, tuple):
    out_ops = [out_ops]

  # Run the model
  anchor_map = {o: popart.AnchorReturnType("ALL") for o in out_ops}
  device = popart.DeviceManager().createIpuModelDevice({'numIPUs': 1})
  session = popart.InferenceSession(
      fnModel=builder.getModelProto(),
      dataFeed=popart.DataFlow(1, anchor_map),
      userOptions=opts,
      deviceInfo=device,
  )
  session.prepareDevice()
  anchors = session.initAnchorArrays()
  stepio = popart.PyStepIO(inputs, anchors)
  session.run(stepio)

  results = [anchors[o] for o in out_ops]
  if len(results) == 1:
    results = results[0]
  return results

def add(x, b):
  x = _np_to_popart_tensor(x)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.add([x, b])

def sub(a, b):
  a = _np_to_popart_tensor(a)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.sub([a, b])

def mul(a, b):
  a = _np_to_popart_tensor(a)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.mul([a, b])

def div_ii(a, b):
  raise NotImplementedError

def div_ff(a, b):
  a = _np_to_popart_tensor(a)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.div([a, b])

def eq(a, b):
  a = _np_to_popart_tensor(a)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.equal([a, b])

def lt(a, b):
  a = _np_to_popart_tensor(a)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.less([a, b])

def gt(a, b):
  a = _np_to_popart_tensor(a)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.greater([a, b])

def lte(a, b):
  raise NotImplementedError

def gte(a, b):
  raise NotImplementedError

def or_(a, b):
  raise NotImplementedError

def and_(a, b):
  raise NotImplementedError

def abs_(a):
  a = _np_to_popart_tensor(a)
  return builder.aiOnnx.abs([a])

def max_(a, b):
  a = _np_to_popart_tensor(a)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.max([a, b])

def neg(a):
  a = _np_to_popart_tensor(a)
  return builder.aiOnnx.neg([a])

def to_float_i(a):
  raise NotImplementedError

def build(n, f):
  return [f(i) for i in range(n)]

def sumbuild(n, f):
  return sum(f(i) for i in range(n))

def index(i, v):
  return v[i]

def size(v):
  return len(v)

def fold(f, s0, xs):
  s = s0
  for x in xs:
      s = f((s, x))
  return s

def broadcast_add(x, b):
  x = _np_to_popart_tensor(x)
  b = _np_to_popart_tensor(b)
  return builder.aiOnnx.add([x, b])

def dot(x, y):
  x = _np_to_popart_tensor(x)
  y = _np_to_popart_tensor(y)
  return builder.aiOnnx.matmul([x, y])

def transpose(x):
  x = _np_to_popart_tensor(x)
  return builder.aiOnnx.transpose([x])

def relu(x):
  x = _np_to_popart_tensor(x)
  return builder.aiOnnx.relu([x])

def log_softmax(x):
  print('\n\nWARNING: log_softmax is not the same as in jax. FIXME.\n\n')
  x = _np_to_popart_tensor(x)
  return builder.aiOnnx.logsoftmax([x])

def _conv_2d_no_bias_factory(padding):
  def conv_2d(x, weights, strides):
    x = _np_to_popart_tensor(x)
    weights = _np_to_popart_tensor(weights)
    ksize = 7
    pad = int(ksize / 2)
    y = builder.aiOnnx.conv(  # TODO need to fix these options
        [x, weights],
        dilations=[1, 1],
        # kernel_shape=[ksize, ksize],
        strides=strides,
        pads=[pad, pad, pad, pad],
      )
    return y
  return conv_2d

conv_2d_no_bias_same = _conv_2d_no_bias_factory("SAME_UPPER")  # TODO confirm
conv_2d_no_bias_valid = _conv_2d_no_bias_factory("VALID")

def normalize_2d(x, weights):
  mean, sigma = weights
  x = _np_to_popart_tensor(x)
  mean = _np_to_popart_tensor(mean[:, None, None])
  sigma = _np_to_popart_tensor(sigma[:, None, None])
  return builder.aiOnnx.div([builder.aiOnnx.sub([x, mean]), sigma])

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
