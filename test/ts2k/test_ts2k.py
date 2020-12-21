
import ts2ks
import torch


def relux(x: float):
    if x < 0.0:
        return 0.1*x
    else:
        return x * x

def f(x : float):
    r1 = relux(x)
    r2 = relux(-r1)
    return r2


def test_ts2k():
    val = f(2.0)
    assert val == -0.4

    f_fast = torch.jit.script(f)

    val = f_fast(2.0)
    assert val == -0.4

    f_ks = ts2ks.ts2ks("obj/ts2ks_tmp", True, f_fast)
