import pytest

import math
import torch
import numpy

from ksc import utils
from ksc.type import Type
from ksc.torch_frontend import ts2mod


def bar1(a: int, x: float, b: str):
    if a < 0:
        t = -0.125 * x
    else:
        t = 1 / 2 * x ** 2
    return torch.sin(t) * t


def grad_bar1(a: int, x: float, b: str):
    """
    Hand-written gradient of bar1, used to test AD
    """
    if a < 0:
        t = -0.125 * x
        dtdx = -0.125
    else:
        t = 1 / 2 * x ** 2
        dtdx = x
    return torch.sin(t) + t * torch.cos(t)


def relux(x: float):
    if x < 0.0:
        return 0.1 * x
    else:
        return x * x


def grad_relux(x: float):
    """
    Hand-written gradient of relux, used to test AD
    """
    if x < 0.0:
        return 0.1
    else:
        return 2 * x


def f(x: float):
    r1 = relux(x)
    r2 = relux(-r1)
    return r2


ks_relux = None


def compile_relux():
    global ks_relux
    if ks_relux is None:
        print("Compiling relux")
        torch_extension_name = "ksc_test_ts2k_relux"
        ks_relux = ts2mod(relux, (1.0,), torch_extension_name)


def test_ts2k_relux():
    compile_relux()
    ks_ans = ks_relux.py_mod.forward(2.0)
    ans = relux(2.0)
    assert pytest.approx(ks_ans, 1e-6) == ans


def test_ts2k_relux_grad():
    compile_relux()
    ks_ans = ks_relux.py_mod.backward(1.3, 1.0)
    ans = grad_relux(1.3)
    assert pytest.approx(ks_ans, 1e-6) == ans


def bar(a: int, x: float):
    y = torch.tensor([[1.1, -1.2], [2.1, 2.2]])

    b = len(y.size())
    if a < 0:
        t = -0.125 * x
    else:
        t = 1 / 2 * x * float(b)
    return math.sin(t) * t


def grad_bar(a: int, x: float):
    """
    Hand-written gradient of bar, used to test AD
    """
    b = 2
    if a < 0:
        t = -0.125 * x
        dtdx = -0.125
    else:
        t = 1 / 2 * x * float(b)
        dtdx = float(b) / 2
    dda = ()
    ddx = (math.sin(t) + t * math.cos(t)) * dtdx
    return dda, ddx


def test_bar():
    a, x = 1, 12.34
    torch_extension_name = "ksc_test_ts2k_bar"
    ks_bar = ts2mod(bar, (a, x), torch_extension_name)

    # Check primal
    ks_ans = ks_bar.py_mod.forward(a, x)
    ans = bar(a, x)
    assert pytest.approx(ks_ans, 1e-5) == ans

    # Check grad
    ks_ans = ks_bar.py_mod.backward((a, x), 1.0)
    ans = grad_bar(a, x)
    assert pytest.approx(ks_ans[1], 1e-5) == ans[1]


def far(x: torch.Tensor, y: torch.Tensor):
    xx = torch.cat([x, y], dim=1)
    xbar = torch.mean(xx)
    if xbar < 0.0:
        t = -0.125 * x
    else:
        t = 1 / 2 * x ** 2
    return torch.mean(torch.sin(t) * xbar * t)


def test_far():
    x = torch.randn(2, 3)
    y = torch.randn(2, 5)
    torch_extension_name = "ksc_test_ts2k_far"
    ks_far = ts2mod(far, (x, y), torch_extension_name)

    ks_ans = ks_far.py_mod.forward(ks_far.adapt(x), ks_far.adapt(y))
    ans = far(x, y)
    assert pytest.approx(ks_ans, 1e-6) == ans.item()


def test_cat():
    def f(x: torch.Tensor, y: torch.Tensor):
        return torch.cat([x, y], dim=1)

    x = torch.randn(2, 3)
    y = torch.randn(2, 5)
    torch_extension_name = "ksc_test_ts2k_cat"
    ks_f = ts2mod(f, (x, y), torch_extension_name)
    ks_ans = ks_f.py_mod.forward(ks_f.adapt(x), ks_f.adapt(y))
    ks_ans_np = numpy.array(ks_ans, copy=True)
    py_ans = f(x, y)
    assert (ks_ans_np == py_ans.numpy()).all()  # non-approx


def relu3(x: float) -> float:
    if x < 0.0:
        return 0.0
    elif x < 1.0:
        return 1 / 3 * (x ** 3)
    else:
        return x - 2 / 3


def grad_relu3(x: float) -> float:
    if x < 0.0:
        return 0.0
    elif x < 1.0:
        return x * x
    else:
        return 1.0


@pytest.mark.parametrize("generate_lm", [True, False])
def test_relu3(generate_lm):
    x = 0.5

    torch_extension_name = "ksc_test_ts2k_relu3" + ("_lm" if generate_lm else "")
    ks_relu3 = ts2mod(relu3, (x,), torch_extension_name, generate_lm)

    for x in [-0.1, 0.31221, 2.27160]:
        # Test function: ks == py
        py_ans = relu3(x)
        ks_ans = ks_relu3.py_mod.forward(x)

        assert pytest.approx(ks_ans, 1e-6) == py_ans

        # Check manual gradient using finite differences
        delta = 1e-6
        py_ans_fd = (relu3(x + delta) - relu3(x)) / delta
        assert pytest.approx(py_ans_fd, 1e-4) == grad_relu3(x)

        # Test gradient ks == py
        py_ans = grad_relu3(x)
        ks_ans = ks_relu3.py_mod.backward(x, 1.0)

        assert pytest.approx(ks_ans, 1e-6) == py_ans


# def test_Vec_init():
#     def f(x : float):
#         return torch.tensor([x, 2.2])

#     ts2mod(f, [Type.Float], Type.Vec(Type.Float))
