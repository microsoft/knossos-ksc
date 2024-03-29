# Remarkably, adbench-lstm.ks agrees exactly with our reference
# implementation, so we set the almost equal check to check to a very
# large number of decimal places.

import adbench_lstm as a
import ksc.adbench_lstm.lstm as k
import random
import numpy as np


def r():
    return random.random() * 2 - 1


def rv(n):
    return [r() for _ in range(n)]


def rvv(n, m):
    return [rv(m) for _ in range(n)]


def concat(l):
    return sum(l, [])


# The ks::vec __iter__ method that is automatically generated by
# pybind11 is one that keeps going off the end of the vec and never
# stops.  Until I get time to dig into how to make it generate a
# better one, here's a handy utility function.
def to_list(x):
    return [x[i] for i in range(len(x))]


def main():
    assert_equal_model()
    assert_equal_predict_and_objective()
    print("The assertions didn't throw any errors, so " "everything must be good!")


def assert_equal_model():
    h = 2

    w1 = rv(h)
    w2 = rv(h)
    w3 = rv(h)
    w4 = rv(h)

    b1 = rv(h)
    b2 = rv(h)
    b3 = rv(h)
    b4 = rv(h)

    hidden = rv(h)
    cell = rv(h)
    input_ = rv(h)

    weight = concat([w1, w2, w3, w4])
    bias = concat([b1, b2, b3, b4])

    (ao0, ao1) = a.lstm_model(w1, b1, w2, b2, w3, b3, w4, b4, hidden, cell, input_,)

    ao0l = to_list(ao0)
    ao1l = to_list(ao1)

    nd_weight = np.array(weight)

    (mo0, mo1) = k.lstm_model(
        nd_weight, np.array(bias), np.array(hidden), np.array(cell), np.array(input_)
    )

    print(mo0)
    print(ao0l)
    print(mo1)
    print(ao1l)

    np.testing.assert_almost_equal(ao0l, mo0, decimal=5, err_msg="Model 1")
    np.testing.assert_almost_equal(ao1l, mo1, decimal=5, err_msg="Model 2")


def assert_equal_predict_and_objective():
    l = 2
    h = 10

    w1 = rvv(l, h)
    w2 = rvv(l, h)
    w3 = rvv(l, h)
    w4 = rvv(l, h)

    b1 = rvv(l, h)
    b2 = rvv(l, h)
    b3 = rvv(l, h)
    b4 = rvv(l, h)

    hidden = rvv(l, h)
    cell = rvv(l, h)

    input_ = rv(h)

    input_weight = rv(h)
    output_weight = rv(h)
    output_bias = rv(h)

    tww = np.array(
        concat(
            [concat([w1i, w2i, w3i, w4i]), concat([b1i, b2i, b3i, b4i])]
            for (w1i, w2i, w3i, w4i, b1i, b2i, b3i, b4i) in zip(
                w1, w2, w3, w4, b1, b2, b3, b4
            )
        )
    )

    ts = np.array(concat(([hiddeni, celli] for (hiddeni, celli) in zip(hidden, cell))))

    tww2 = np.array([input_weight, output_weight, output_bias])

    tinput_ = np.array(input_)

    print(tww.shape)
    print(tww2.shape)
    print(ts.shape)
    print(tinput_.shape)

    (tp0, tp1) = k.lstm_predict(tww, tww2, ts, tinput_)

    tp0l = tp0.tolist()
    tp1l = tp1.tolist()

    wf_etc = [tuple(tu) for tu in zip(w1, b1, w2, b2, w3, b3, w4, b4, hidden, cell)]

    (v, vtvv) = a.lstm_predict(
        wf_etc, input_weight, output_weight, output_bias, input_,
    )

    vl = to_list(v)
    vtvvl = concat([to_list(v1), to_list(v2)] for (v1, v2) in to_list(vtvv))

    to = k.lstm_objective(tww, tww2, ts, [tinput_, tinput_])
    tol = to.tolist()

    print(tol)

    aol = a.lstm_objective(
        wf_etc, input_weight, output_weight, output_bias, [(input_, input_)],
    )

    print(tp0l)
    print(vl)
    print(tp1l)
    print(vtvvl)
    print(tol)
    print(aol)

    np.testing.assert_almost_equal(tp0l, vl, decimal=5, err_msg="Predict 1")
    np.testing.assert_almost_equal(tp1l, vtvvl, decimal=5, err_msg="Predict 2")
    np.testing.assert_almost_equal(tol, aol, decimal=5, err_msg="Objective")


if __name__ == "__main__":
    main()
