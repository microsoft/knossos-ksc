# Copyright (c) Microsoft Corporation.
# Licensed under the MIT license.
#
# Based on code under the following license:
#
# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""A basic MNIST convnet example using JAX with the mini-libraries
stax and optimizers.

"""

import jax.numpy as np
from jax import jit, random
from jax.experimental import stax
from jax.experimental.stax import Dense, Relu, Conv, MaxPool, glorot, randn


def DenseND(out_dim, W_init=glorot(), b_init=randn()):
    """Layer constructor function for a dense n-dimensional (fully-connected) layer.
  It will accept any shape of tensor as input and return a vector as output."""

    def init_fun(rng, input_shape):
        output_shape = (input_shape[0], out_dim)
        k1, k2 = random.split(rng)
        W, b = W_init(k1, input_shape[1:] + (out_dim,)), b_init(k2, (out_dim,))
        return output_shape, (W, b)

    def apply_fun(params, inputs, **_):
        W, b = params
        N = len(inputs.shape) - 1
        N_ = len(W.shape) - 1
        assert N == N_
        return np.tensordot(inputs, W, axes=N) + b

    return init_fun, apply_fun


def Conv5x5(out_chan):
    return Conv(out_chan=out_chan, filter_shape=(5, 5), padding="same")


MaxPool2x2 = MaxPool(window_shape=(2, 2), strides=(2, 2), padding="same")

convnet = [
    Conv5x5(out_chan=32),
    Relu,
    MaxPool2x2,
    Conv5x5(out_chan=64),
    Relu,
    MaxPool2x2,
    DenseND(1024),
    Relu,
    Dense(10),
]

init_mnistjax, mnistjax = stax.serial(*convnet)
mnistjax = jit(mnistjax)
