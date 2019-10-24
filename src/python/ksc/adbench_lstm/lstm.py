# A reference implementation of D-LSTM, so that we can test our
# Knossos implementation.
#
# Please note that this is an LSTM with diagonal weight matrices, not
# a full LSTM.  Its purpose is to calculate the exact same thing that
# ADBench's D-LSTM implementation calculates, not to implement a full
# LSTM.  If and when ADBench changes its implementation to a full
# LSTM, we will too.
#
# This code is an as-close-as-possible port of ADBench's Tensorflow
# implementation to numpy.  The only reason it is changed at all is
# that I want to avoid picking up a Tensorflow dependency.  See
#
#     https://github.com/awf/ADBench/blob/e5f72ab5dcb453b1bb72dd000e5add6b90502ec4/src/python/modules/Tensorflow/TensorflowLSTM.py

import numpy as np

def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-x))

def lstm_model(weight, bias, hidden, cell, inp):
    gates = np.concatenate((inp, hidden, inp, hidden), 0) * weight + bias
    hidden_size = hidden.shape[0]

    forget  = sigmoid(gates[0:hidden_size])
    ingate  = sigmoid(gates[hidden_size:2*hidden_size])
    outgate = sigmoid(gates[2*hidden_size:3*hidden_size])
    change  = np.tanh(gates[3*hidden_size:])

    cell   = cell * forget + ingate * change
    hidden = outgate * np.tanh(cell)

    return (hidden, cell)

def lstm_predict(w, w2, s, x):
    s2 = s.copy()
    # NOTE not sure if this should be element-wise or matrix multiplication
    x = x * w2[0]
    for i in range(0, len(s), 2):
        (s2[i], s2[i + 1]) = lstm_model(w[i], w[i + 1], s[i], s[i + 1], x)
        x = s2[i]
    return (x * w2[1] + w2[2], s2)

def lstm_objective(main_params, extra_params, state, sequence, _range=None):
    if _range is None:
        _range = range(0, len(sequence) - 1)

    total = 0.0
    count = 0
    _input = sequence[_range[0]]
    all_states = [state]
    for t in _range:
        ypred, new_state = lstm_predict(main_params, extra_params, all_states[t], _input)
        all_states.append(new_state)
        ynorm = ypred - np.log(sum(np.exp(ypred), 2))
        ygold = sequence[t + 1]
        total += sum(ygold * ynorm)
        count += ygold.shape[0]
        _input = ygold
    return -total / count
