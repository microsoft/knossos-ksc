import pytest

import math
import torch
import numpy

torch.set_default_dtype(torch.float64)

from ksc import utils
from ksc.type import Type
from ksc.ts2ks import ts2mod


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
        ks_relux = ts2mod(relux, (1.0,))


def test_ts2k_relux():
    compile_relux()
    assert ks_relux.py_mod.entry(2.0) == relux(2.0)


def test_ts2k_relux_grad():
    compile_relux()
    assert ks_relux.py_mod.rev_entry(1.3, 1.0) == grad_relux(1.3)


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
    ks_bar = ts2mod(bar, (a, x))
    ks_ans = ks_bar.py_mod.entry(a, x)
    ans = bar(a, x)
    assert ans == ks_ans

    ks_grad = ks_bar.py_mod.rev_entry((a, x), 1.0)
    assert ks_grad == grad_bar(a, x)


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
    ks_far = ts2mod(far, (x, y))
    ks_ans = ks_far.py_mod.entry(ks_far.adapt(x), ks_far.adapt(y))
    ans = far(x, y)
    assert pytest.approx(ks_ans, 1e-8) == ans.item()


def test_cat():
    def f(x: torch.Tensor, y: torch.Tensor):
        return torch.cat([x, y], dim=1)

    x = torch.randn(2, 3)
    y = torch.randn(2, 5)
    ks_f = ts2mod(f, (x, y))
    ks_ans = ks_f.py_mod.entry(ks_f.adapt(x), ks_f.adapt(y))
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

    ks_relu3 = ts2mod(relu3, (x,), generate_lm)

    for x in [-0.1, 0.31221, 2.27160]:
        # Test function: ks == py
        py_ans = relu3(x)
        ks_ans = ks_relu3.py_mod.entry(x)

        assert pytest.approx(ks_ans, 1e-8) == py_ans

        # Check manual gradient using finite differences
        delta = 1e-8
        py_ans_fd = (relu3(x + delta) - relu3(x)) / delta
        assert pytest.approx(py_ans_fd, 1e-5) == grad_relu3(x)

        # Test gradient ks == py
        py_ans = grad_relu3(x)
        grad_fun = (
            ks_relu3.py_mod.rev_entry if generate_lm else ks_relu3.py_mod.sufrev_entry
        )
        ks_ans = grad_fun(x, 1.0)

        assert pytest.approx(ks_ans, 1e-8) == py_ans


# def test_Vec_init():
#     def f(x : float):
#         return torch.tensor([x, 2.2])

#     ts2mod(f, [Type.Float], Type.Vec(Type.Float))
