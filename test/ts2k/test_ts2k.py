
from ts2ks import ts2mod
import torch
from ksc import utils
from ksc.type import Type

def bar(a : int, x : float, b : str):
    if a < 0:
        t = -0.125*x
    else:
        t = 1/2 * x ** 2
    return torch.sin(t)*t

def relux(x: float):
    if x < 0.0:
        return 0.1*x
    else:
        return x * x

def f(x : float):
    r1 = relux(x)
    r2 = relux(-r1)
    return r2

def test_ts2k_relux():
    ks_relux = ts2mod(relux, [Type.Float])

    assert ks_relux(2.0) == relux(2.0)
