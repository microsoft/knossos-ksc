# Copyright (c) Microsoft Corporation.
# Licensed under the MIT license.

from jax import random
from ksc.jax import mnist_classifier

import mnistcnncpp as e

import numpy as np
import numpy.random

def vd(x): return e.vec_double([y for y in x])
def vvd(x): return e.vec_vec_double([vd(y) for y in x])
def vvvd(x): return e.vec_vec_vec_double([vvd(y) for y in x])
def vvvvd(x): return e.vec_vec_vec_vec_double([vvvd(y) for y in x])

def vec_iter(v): return (v[i] for i in range(len(v)))

# There are only 9 layers in the JAX convnet that we use.  The main
# unpacking function unpacks the weights corresponding to 10 layers
# instead of 9 to give us the opportunity to add a LogSoftmax layer
# later.  We adapt for the case without softmax case here.
def knossos_weights_of_jax_weights_no_softmax(weights):
    ignored_logsoftmax_weight = ()
    return knossos_weights_of_jax_weights(weights + [ignored_logsoftmax_weight])

def knossos_weights_of_jax_weights(weights):
    [ (k1, bk1), _, _,
      (k2, bk2), _, _,
      (d1, bd1), _,
      (d2, bd2), _
    ] = weights

    # The Knossos convnet arranges the data with axes opposite to the
    # JAX implementation.  We should probably switch the Knossos
    # implementation to match the JAX, but until we do that 'array'
    # switches them.
    def array(v):
        axes = reversed(range(len(v.shape)))
        return np.array(v).transpose(*axes)

    k1  = vvvvd(array(k1))
    bk1 = vd(array(bk1))
    k2  = vvvvd(array(k2))
    bk2 = vd(array(bk2))
    d1  = vvvvd(array(d1))
    bd1 = vd(array(bd1))
    d2  = vvd(array(d2))
    bd2 = vd(array(bd2))

    return (k1, bk1, k2, bk2, d1, bd1, d2, bd2)

def knossos_image(v): return vvvd(v.reshape(1,28,28).transpose(0,2,1))

def jax_image(v): return v.reshape(1,28,28,1)

def test():
    _, random_weights_j = \
      mnist_classifier.init_mnistjax(random.PRNGKey(0), (-1, 28, 28, 1))

    random_weights_k = \
      knossos_weights_of_jax_weights_no_softmax(random_weights_j)

    random_image   = numpy.random.random_sample((1,28,28))
    random_image_k = knossos_image(random_image)
    random_image_j = jax_image(random_image)

    mk = e.mnist(random_image_k, *random_weights_k)
    mk = np.array(list(vec_iter(mk)))

    mj = mnist_classifier.mnistjax(random_weights_j, random_image_j)
    mj = mj[0]

    max_diff = np.max(np.abs(mj - mk))

    max_mj = np.max(np.abs(mj))
    max_mk = np.max(np.abs(mk))
    rel_err = max_diff / (max_mj + max_mk)

    print(max_mj)
    print(max_mk)
    print(max_diff)
    print(rel_err)

    # This max relative error is fairly precisely chosen.  It turns
    # out that the convolution biases don't have much effect on the
    # results and removing them only just crosses this relative error
    # boundary.
    max_rel_err = 1e-6

    if rel_err > max_rel_err:
        print("DIFFERENCE TOO BIG!")
        raise Exception
    else:
        print("Difference OK")

if __name__ == '__main__': test()
