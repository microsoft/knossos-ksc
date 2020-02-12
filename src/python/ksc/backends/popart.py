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
      np.dtype('int64'): 'INT32',
    }
    t = builder.addInputTensor(popart.TensorInfo(typs[x.dtype], x.shape))
    if x.dtype == np.dtype('float64'):
      x = x.astype(np.float32)  # TODO maybe not do this
    if x.dtype == np.dtype('int64'):
      x = x.astype(np.int32)  # TODO maybe not do this
    inputs[t] = x
  elif x_typ == str:
    if x in builder.getInputTensorIds() or x in builder.getValueTensorIds():
    t = x
  else:
      raise Exception(f'Tensor {x} not recognized by popART!\n'
        f'Available tensors are:\n'
        f'{builder.getInputTensorIds()}\n{builder.getValueTensorIds()}')
  else:
    raise Exception('Got unrecognized type: ' + str(x_typ))
  return t

def _np_to_popart_tensors(*args):
  '''Convert a list or tuple of numpy arrays to a popart tensors.'''
  if len(args) != 1:
    return tuple(_np_to_popart_tensors(t) for t in args)
  else:
    arg = args[0]
    if isinstance(arg, list):
      return list(_np_to_popart_tensors(t) for t in arg)
    if isinstance(arg, tuple):
      return tuple(_np_to_popart_tensors(t) for t in arg)
    return _np_to_popart_tensor(arg)

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

def add(a, b):
  a, b = _np_to_popart_tensors(a, b)
  return builder.aiOnnx.add([a, b])

def sub(a, b):
  a, b = _np_to_popart_tensors(a, b)
  return builder.aiOnnx.sub([a, b])

def mul(a, b):
  a, b = _np_to_popart_tensors(a, b)
  return builder.aiOnnx.mul([a, b])

def div_ii(a, b):
  raise NotImplementedError

def div_ff(a, b):
  a, b = _np_to_popart_tensors(a, b)
  return builder.aiOnnx.div([a, b])

def eq(a, b):
  a, b = _np_to_popart_tensors(a, b)
  return builder.aiOnnx.equal([a, b])

def lt(a, b):
  a, b = _np_to_popart_tensors(a, b)
  return builder.aiOnnx.less([a, b])

def gt(a, b):
  a, b = _np_to_popart_tensors(a, b)
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
  a, b = _np_to_popart_tensors(a, b)
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

def broadcast_add(a, b):
  a, b = _np_to_popart_tensors(a, b)
  return builder.aiOnnx.add([a, b])

def dot(x, y):
  x, y = _np_to_popart_tensors(x, y)
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

def _padtype_to_pads(in_shape, window_shape, window_strides, padding):
  """Convert padding string to list of pad values."""
  if padding == 'SAME':
    out_shape = np.ceil(np.true_divide(in_shape, window_strides)).astype(int)
      # _ceil_divide(in_shape, window_strides)
    pad_sizes = np.maximum(0, (out_shape - 1) * window_strides +
                                window_shape - in_shape)
    return [f(pad_size)
        for f in (lambda x: x // 2, lambda x: x - x // 2)
        for pad_size in pad_sizes]
  elif padding == 'VALID':
    return [0, 0] * len(in_shape)
  else:
    msg = "Unknown padding type: {}."
    raise TypeError(msg.format(padding))

def _conv_2d_no_bias_factory(padding):
  def conv_2d(x, weights, strides):
    x, weights = _np_to_popart_tensors(x, weights)

    in_shape = builder.getTensorShape(x)[2:]
    window_shape = builder.getTensorShape(weights)[2:]
    ksize = builder.getTensorShape(weights)[1]
    pads = _padtype_to_pads(in_shape, window_shape, strides, padding)

    return builder.aiOnnx.conv(
        [x, weights],
        dilations=[1, 1],
        kernel_shape=[ksize, ksize],
        strides=strides,
        pads=pads,
      )
  return conv_2d

conv_2d_no_bias_same = _conv_2d_no_bias_factory("SAME")
conv_2d_no_bias_valid = _conv_2d_no_bias_factory("VALID")

def normalize_2d(x, weights):
  mean, sigma = weights
  x, mean, sigma = _np_to_popart_tensors(
      x, mean[:, None, None], sigma[:, None, None])
  return builder.aiOnnx.div([builder.aiOnnx.sub([x, mean]), sigma])

def batch_norm_2d(x, weights):
  # TODO implement using builder.aiOnnx.batchnormalization
  mean, var, gamma, beta = weights
  sigma = np.sqrt(var + 1e-5)
  z = normalize_2d(x, (mean, sigma))
  return add(mul(gamma[:, None, None], z), beta[:, None, None])

def to_float(x):
  if hasattr(x, "astype"):
    return x.astype(np.float32)
  else:
    return float(x)

def max_pool_same(x, pool_size, strides):
  x = _np_to_popart_tensor(x)
  in_shape = builder.getTensorShape(x)[2:]
  pads = _padtype_to_pads(in_shape, pool_size, strides, 'SAME')
  return builder.aiOnnx.maxpool(
    [x],
    num_outputs=1,
    kernel_shape=pool_size,
    pads=pads,
    strides=strides,
  )[0]  # Because for some reason maxpool returns a list of one output..

def avg_pool_valid(x, pool_size, strides):
  raise NotImplementedError

def flatten(x):
  b = x.shape[0]
  return x.reshape((b, -1))
