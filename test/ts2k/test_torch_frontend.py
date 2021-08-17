import pytest

import math
import torch
import numpy

import ksc.torch_frontend as knossos
from ksc.torch_utils import elementwise_apply


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


@knossos.register
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


def test_ts2k_relux():
    ks_ans = relux._entry(2.0)
    ans = relux.raw_f(2.0)
    assert pytest.approx(ks_ans, 1e-6) == ans


def test_ts2k_relux_grad():
    relux.ensure_compiled((2.0,))  # TODO: remove when entry_vjp knows how to compile
    ks_ans = relux._entry_vjp(1.3, 1.0)
    ans = grad_relux(1.3)
    assert pytest.approx(ks_ans, 1e-6) == ans


def relux_pt(x: float):
    return (x < 0) * (0.1 * x) + (x > 0) * (x * x)


vrelux = knossos.register_direct(relux, elementwise=True)


def test_ts2k_vrelux():
    y = torch.tensor([[1.1, -1.2], [2.1, 2.2]])
    ks_ans = vrelux(y)
    pt_ans = elementwise_apply(relux_pt, y)

    assert torch.isclose(pt_ans, ks_ans, rtol=1e-05, atol=1e-06, equal_nan=False).all()


@knossos.register(generate_lm=True)
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

    # Check primal
    ks_ans = bar._entry(a, x)
    ans = bar.raw_f(a, x)
    assert pytest.approx(ks_ans, 1e-5) == ans

    # Check grad
    ks_ans = bar._entry_vjp((a, x), 1.0)
    ans = grad_bar(a, x)
    assert pytest.approx(ks_ans[1], 1e-5) == ans[1]


@knossos.register(generate_lm=True)
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

    ks_ans = far._entry(x, y)
    ans = far.raw_f(x, y)
    assert pytest.approx(ks_ans, 1e-5) == ans.item()


def test_cat():
    @knossos.register(generate_lm=True)
    def f(x: torch.Tensor, y: torch.Tensor):
        return torch.cat([x, y], dim=1)

    x = torch.randn(2, 3)
    y = torch.randn(2, 5)

    ks_ans = f._entry(x, y)
    ks_ans_np = numpy.array(ks_ans, copy=True)
    py_ans = f.raw_f(x, y)
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

    @knossos.register(generate_lm=generate_lm)
    def ks_relu3(x: float):
        return relu3(x)

    for x in [-0.1, 0.31221, 2.27160]:
        # Test function: ks == py
        py_ans = relu3(x)
        ks_ans = ks_relu3._entry(x)

        assert pytest.approx(ks_ans, 1e-6) == py_ans

        # Check manual gradient using finite differences
        delta = 1e-6
        py_ans_fd = (relu3(x + delta) - relu3(x)) / delta
        assert pytest.approx(py_ans_fd, 1e-4) == grad_relu3(x)

        # Test gradient ks == py
        py_ans = grad_relu3(x)
        ks_ans = ks_relu3._entry_vjp(x, 1.0)

        assert pytest.approx(ks_ans, 1e-6) == py_ans
