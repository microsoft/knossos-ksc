import pytest
import ksc

import numpy as np

from ksc.tracing.node import Node
import ksc.tracing.functions as F

@pytest.fixture()
def backend(pytestconfig):
    return pytestconfig.getoption("backend")

@ksc.trace
def my_add(a, b):
    return F.add(a, b)

def test_add(backend):
    out = F.add(1, 2)
    assert out.get_data_with_backend(backend) == 3

    with pytest.raises(AssertionError):
        out = F.add(1.0, 2)

    out = F.add(np.array([1.0, 0.0]), np.array([2.0, 3.0]))
    assert np.allclose(out.data, [3.0, 3.0])

def test_my_add(backend):
    out = my_add(1.0, 2.0)
    assert out.get_data_with_backend(backend) == 3.0

@ksc.trace
def add3(a, b, c):
    return F.add(F.add(a, b), c)

@ksc.trace
def square(a):
    return F.mul(a, a)

@ksc.trace
def cube(a):
    return F.mul(F.mul(a, a), a)

def test_user_function(backend):
    out = add3(1.0, 3.0, 4.0)
    assert out.get_data_with_backend(backend) == 8.0

    out = square(2.0)
    assert out.get_data_with_backend(backend) == 4.0

def test_wrong_type():
    with pytest.raises(AssertionError):
        out = add3(1.0, 3.0, 4)

def test_jit_wrong_type():
    out = add3(1.0, 3.0, 4.0)
    jitted = out.creator._jitted
    with pytest.raises(AssertionError):
        jitted(1, 2, 3)

def test_jit_anonymous(backend):
    out = F.add(F.add(1, 2,), 3)
    assert out.get_data_with_backend(backend) == 6
    jitted = out.creator._jitted
    assert jitted.name == "_anonymous@iii"
    assert len(jitted.arg_names) == 3
    assert jitted(2, 3, 4) == 9

@ksc.trace
def nested(a, b):
    return square(F.add(a, b))

def test_user_function_nested():
    out = nested(1.0, 2.0)
    assert out.data == 9.0

@ksc.trace
def need_let(a, b):
    a2 = square(a)
    return F.add(a2, F.sub(a2, b))

def test_need_let(backend):
    out = need_let(3, 1)
    assert "(let ((v0 (square@i a)))" in out.creator._jitted.ks_str
    assert out.get_data_with_backend(backend) == 17 # 9 + 8

@ksc.trace
def add_const_float(a):
    return F.add(a, 2.5)

def test_constant():
    out = add_const_float(1.0)
    assert out.data == 3.5

def test_delayed():
    out = F.add(F.add(F.add(1, 2), 3), 4)
    assert out.data == 10

def test_operator_overloading():
    x = Node.from_data(np.array([1.0, -2.0]))
    out = x + np.array([0.5, -0.5])
    assert np.allclose(out.data, np.array([1.5, -2.5]))

    x = Node.from_data(np.array([1.0, -2.0]))
    out = x - np.array([0.5, -0.5])
    assert np.allclose(out.data, np.array([0.5, -1.5]))

    x = Node.from_data(np.array([1.0, -2.0]))
    out = x * np.array([0.5, -0.5])
    assert np.allclose(out.data, np.array([0.5, 1.0]))

    x = Node.from_data(np.array([1.0, -2.0]))
    out = x / np.array([0.5, -0.5])
    assert np.allclose(out.data, np.array([2.0, 4.0]))

def test_flatten():
    x = np.random.normal(0, 1, (3, 4, 5, 6))
    out = F.flatten(x)
    assert out.shape_type.shape == (3, 4 * 5 * 6)
    assert np.allclose(out.data, x.reshape((3, 4 * 5 * 6)))

def test_to_float():
    x = np.arange(9)
    out = F.to_float(x)
    assert np.allclose(out.data, x.astype(np.float32))

def test_multiple_defs():
    out = F.to_float(F.add(1, 2)) + F.add(2.0, 3.0)
    print(out.data)
    print(out.creator._jitted.ks_str)

@ksc.trace
def my_flatten(a):
    return F.flatten(a)

def test_traced_flatten_shape():
    x1 = np.random.normal(0, 1, (2, 3, 4))
    o = my_flatten(x1)
    assert o.shape_type.shape == (2, 12)
    assert o.data.shape == (2, 12)

    jitted = o.creator._jitted
    x2 = np.random.normal(0, 1, (3, 4, 5))
    o = jitted(x2) # returns an array
    assert o.shape == (3, 20)

def test_reuse_result(backend):
    x = F.add(F.add(1, 2), 3)
    assert x.data == 6

    y = F.mul(F.add(x, 3), x)
    assert y.data == 54

    z = y - x
    assert z.get_data_with_backend(backend) == 48
