# pylint: disable=no-member
import numpy as np
import popart

from ksc.backends.common import *


# NOTE: This module has mutable state
# Common builder object:
builder = popart.Builder()
# Input operators and input data
inputs = {}


def _run_model(model):
    '''Run `model` on the IPU model for debugging the output.'''
    # Popart set-up
    opts = popart.SessionOptions()

    # Construct model
    out_ops = model()
    if not isinstance(out_ops, list) or not isinstance(out_ops, tuple):
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

    return {o: anchors[o] for o in out_ops}

def ones(size):
  x = builder.addInputTensor(popart.TensorInfo('FLOAT', [size]))
  inputs[x] = np.ones(size, dtype=np.float32)
  return x

def ones_2d(len1, len2):
  x = builder.addInputTensor(popart.TensorInfo('FLOAT', [len1, len2]))
  inputs[x] = np.ones((len1, len2), dtype=np.float32)
  return x

def BroadcastAdd(x, b):
  return builder.aiOnnx.add([x, b])

def MatMul(x, y):
  return builder.aiOnnx.matmul([x, y])

def Transpose(x):
  return builder.aiOnnx.transpose([x])

def Relu(x):
  return builder.aiOnnx.relu([x])

def LogSoftmax(x):
  return builder.aiOnnx.logsoftmax([x])

def Conv2DNoBias(filters, kernel_size, strides, weight, x):
  raise NotImplementedError

def Normalize2D(weights, x):
  raise NotImplementedError

def BatchNorm2D(weights, x):
  raise NotImplementedError

def ToFloat(x):
  raise NotImplementedError

def MaxPool(pool_size, strides, x):
  raise NotImplementedError

def AvgPool(pool_size, strides, x):
  raise NotImplementedError

def Flatten(x):
  raise NotImplementedError

def Add(x, y):
  raise NotImplementedError
