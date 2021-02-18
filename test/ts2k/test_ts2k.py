import pytest

import math
import torch

torch.set_default_dtype(torch.float64)

from ksc import utils
from ksc.type import Type
from ts2ks import ts2mod


def bar1(a : int, x : float, b : str):
    if a < 0:
        t = -0.125*x
    else:
        t = 1/2 * x ** 2
    return torch.sin(t)*t

def grad_bar1(a : int, x : float, b : str):
    """
    Hand-written gradient of bar1, used to test AD
    """
    if a < 0:
        t = -0.125*x
        dtdx = -0.125
    else:
        t = 1/2 * x ** 2
        dtdx = x
    return torch.sin(t) + t*torch.cos(t)

def relux(x: float):
    if x < 0.0:
        return 0.1*x
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

def f(x : float):
    r1 = relux(x)
    r2 = relux(-r1)
    return r2

ks_relux = None
def compile_relux():
    global ks_relux
    if ks_relux is None:
        print("Compiling relux")
        ks_relux = ts2mod(relux, (1.0,))

@pytest.mark.skip(reason="Does not support gdef or structured names")
def test_ts2k_relux():
    compile_relux()
    assert ks_relux(2.0) == relux(2.0)

@pytest.mark.skip(reason="no way of currently testing this")
def test_ts2k_relux_grad():
    compile_relux()
    assert ks_relux.rev(1.3, 1.0) == grad_relux(1.3)

def bar(a : int, x : float):
    y = torch.tensor([[1.1, -1.2], [2.1, 2.2]])

    b = len(y.size())
    if a < 0:
        t = -0.125*x
    else:
        t = 1/2 * x * float(b)
    return math.sin(t)*t

def grad_bar(a : int, x : float):
    """
    Hand-written gradient of bar, used to test AD
    """
    b = 2
    if a < 0:
        t = -0.125*x
        dtdx = -0.125
    else:
        t = 1/2 * x * float(b)
        dtdx = float(b)/2
    dda = ()
    ddx = math.sin(t) + t*math.cos(t)
    return dda,ddx

@pytest.mark.skip(reason="no way of currently testing this")
def test_bar():
    a,x = 1,12.34
    ks_bar = ts2mod(bar, (a,x))
    ks_ans = ks_bar(a,x)
    ans = bar(a,x)
    assert ans == ks_ans

    assert ks_bar.rev((a,x),1.0) == grad_bar(a,x)

def far(x : torch.Tensor):
    y = torch.mean(x)
    if y < 0:
        t = -0.125*x
    else:
        t = 1/2 * x ** 2
    return torch.mean(torch.sin(t)*t)

@pytest.mark.skip(reason="no way of currently testing this")
def test_far():
    x = torch.randn(2,3)
    ks_far = ts2mod(far, (x,))
    ks_ans = ks_far(ks_far.adapt(x))
    ans = far(x)
    assert pytest.approx(ks_ans, 1e-8) == ans.item()


    #assert ks_far.rev((a,x),1.0) == grad_far(a,x)

# def test_Vec_init():
#     def f(x : float):
#         return torch.tensor([x, 2.2])

#     ts2mod(f, [Type.Float], Type.Vec(Type.Float))
