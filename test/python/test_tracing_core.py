import pytest
import ksc

import numpy as np
import sys

from ksc.tracing.node import Node
from ksc.tracing.functions import core
import ksc.tracing.functions as F
from ksc.type import Type
from ksc.shape import Shape, TensorShape, ScalarShape

@pytest.fixture()
def backend(pytestconfig):
    return pytestconfig.getoption("backend")

@ksc.trace
def my_add(a, b):
    return F.add(a, b)

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_add(backend):
    out = F.add(1, 2)
    assert out.get_data_with_backend(backend) == 3

    with pytest.raises(ValueError):
        out = F.add(1.0, 2)

    # C++ backend doesn't yet support vector addition, so the rest use the
    # default jax backend:
    a = np.ones((2, 3))
    b = np.ones((3, 2))

    out = F.add(a, a)
    assert np.allclose(out.data, np.tile(2.0, (2, 3)))

    # We're allowed to add scalars to vectors:
    out = F.add(2.0, a)
    assert np.allclose(out.data, np.tile(3.0, (2, 3)))
    out = F.add(b, 1.0)
    assert np.allclose(out.data, np.tile(2.0, (3, 2)))
    # But not if the types don't match:
    with pytest.raises(ValueError):
        out = F.add(2, a)

    # But if we add vectors, their shapes must match:
    with pytest.raises(ValueError):
        out = F.add(b, a)

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
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

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_user_function(backend):
    out = add3(1.0, 3.0, 4.0)
    assert out.get_data_with_backend(backend) == 8.0

    out = square(2.0)
    assert out.get_data_with_backend(backend) == 4.0

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_wrong_type():
    with pytest.raises(ValueError):
        _ = add3(1.0, 3.0, 4)

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_jit_wrong_type():
    out = add3(1.0, 3.0, 4.0)
    jitted = out.creator._jitted
    with pytest.raises(AssertionError):
        jitted(1, 2, 3)

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_jit_anonymous(backend):
    out = F.add(F.add(1, 2,), 3)
    assert out.get_data_with_backend(backend) == 6
    jitted = out.creator._jitted
    assert jitted.name == "_anonymous"
    assert len(jitted.arg_names) == 3
    assert jitted(2, 3, 4) == 9

@ksc.trace
def nested(a, b):
    return square(F.add(a, b))

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_user_function_nested():
    out = nested(1.0, 2.0)
    assert out.data == 9.0

@ksc.trace
def need_let(a, b):
    a2 = square(a)
    return F.add(a2, F.sub(a2, b))

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_need_let(backend):
    out = need_let(3, 1)
    assert "(let (tmpvar__0 (square a))" in out.creator._jitted.ks_str
    assert out.get_data_with_backend(backend) == 17 # 9 + 8

@ksc.trace
@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def add_const_float(a):
    return F.add(a, 2.5)

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_constant():
    out = add_const_float(1.0)
    assert out.data == 3.5

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
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
    assert out.shape_type.shape.dims == (3, 4 * 5 * 6)
    np.testing.assert_array_equal(out.data, x.reshape((3, 4 * 5 * 6)))
    # creator of out is an anonymous function. So the shape$
    # function of flatten must be in before.
    before, _ = out.creator._jitted.all_called_functions()
    shape_def = next(f for key, f in before.items()
                     if key[0] == "shape$flatten")
    assert shape_def(x) == ((3, 4 * 5 * 6), ())

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
    assert o.shape_type.shape == TensorShape((2, 12), ScalarShape)
    assert o.data.shape == (2, 12)

    jitted = o.creator._jitted
    x2 = np.random.normal(0, 1, (3, 4, 5))
    o = jitted(x2) # returns an array
    assert o.shape == (3, 20)

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_reuse_result(backend):
    x = F.add(F.add(1, 2), 3)
    assert x.data == 6

    y = F.mul(F.add(x, 3), x)
    assert y.data == 54

    z = y - x
    assert z.get_data_with_backend(backend) == 48

def test_get_tensor_element():
    xn = np.arange(24).reshape((2, 3, 4))
    x = Node.from_data(xn)
    v = x[1,2,3]
    assert v.data == 23

    v0 = core.get_tensor_element0(x)
    assert v0.data == 0.0

    # TODO: add slicing tests when we have a "slice" primitive
    # o = x[0]
    # assert o.shape_type.shape == (3, 4)

def test_tensor_size():
    xn = np.arange(24).reshape((2, 3, 4))
    x = Node.from_data(xn)
    assert core.get_tensor_size(x).data == (2,3,4)

def get_ks_shape(x):
    return Shape.from_ks_shape(x.shape_program.data, x.type)

def test_tensor_shape():
    x = Node.from_data(np.arange(24).reshape((2, 3, 4)))
    assert get_ks_shape(x) == TensorShape((2, 3, 4), ScalarShape)

    x = Node.from_data(np.random.normal(0, 1, (5, 3, 9)))
    assert get_ks_shape(x) == TensorShape((5, 3, 9), ScalarShape)

    x = Node.from_data((np.arange(6).reshape((2, 3)), np.arange(12).reshape((3, 4))))
    assert get_ks_shape(x) == (TensorShape((2, 3), ScalarShape), TensorShape((3, 4), ScalarShape))

def test_tensor_num_elements():
    x = Node.from_data(np.arange(24).reshape((2, 3, 4)))
    assert core.numel_program(x).data == 24

    x1 = Node.from_data([np.ones((2,3)) for _ in range(11)])
    x2 = Node.from_data(np.ones((5,7)))
    x = core.make_tuple(x2, x1)
    assert core.numel_program(x).data == 5*7 + 2*3*11

@pytest.mark.skipif(sys.platform == 'win32', reason="Has a GCC backend currently, which we don't provide on Windows")
def test_floor_div(backend):
    x = Node.from_data(10)
    o = x // 3
    assert o.get_data_with_backend(backend) == 3

def test_elementwise_or_scalar():
    from ksc.tracing.functions.type_propagation_rules import elementwise_or_scalar
    from ksc.shape import ShapeType

    f = Node.from_data(1.0)
    i = Node.from_data(1)
    a = Node.from_data(np.ones((2, 3)))
    b = Node.from_data(np.ones((3, 2)))
    c = Node.from_data(np.ones((2, 3), dtype=np.int32))

    assert elementwise_or_scalar(f, a, f) == a.shape_type
    assert elementwise_or_scalar(a, f, a) == a.shape_type
    with pytest.raises(ValueError):
        _ = elementwise_or_scalar(f, a, i)
    with pytest.raises(ValueError):
        _ = elementwise_or_scalar(a, f, c)
    with pytest.raises(ValueError):
        _ = elementwise_or_scalar(b, f, a)
