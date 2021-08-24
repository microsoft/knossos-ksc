import torch
import ksc.torch_frontend as knossos

# run-bench: Knossos source, and "nice" PyTorch implementation
# DOC-KS
@knossos.register
def sqrl(x: torch.Tensor):
    """
    sqrl: Squared Leaky Relu
    Like a capsule from /Stuck in a Rut/
    Typically x is a 4x4 tensor, possibly
    packed in a 4n x 4m array
    """
    y = torch.sum(x)
    if y < 0.0:
        t = -0.125 * x
    else:
        t = 1 / 2 * x ** 2
    return torch.mean(torch.sin(t) * t)


# ENDDOC-KS

# run-bench: PyTorch "fast" implementation
def sqrl_pytorch(x: torch.Tensor):
    return sqrl.raw_f(x)


# run-bench: PyTorch "nice" implementation
def sqrl_pytorch_nice(x: torch.Tensor):
    return sqrl.raw_f(x)


# run-bench: Define a range of values at which to call the methods
def sqrl_bench_configs():
    yield torch.randn((4, 4))
    yield torch.randn((16, 16))


#################################
#
# vsqrl - vectorized sqrl
#

vsqrl = knossos.register_direct(sqrl, vmap=True, generate_lm=True)  # TODO: Carbuncle


def sqrl_pytorch_where(x):
    """
    Replace "if" with "where" to get torch.vmap to work
    """
    y = torch.sum(x)
    t = torch.where(y < 0, -0.125 * x, 1 / 2 * x ** 2)
    tsint = torch.sin(t) * t
    return torch.mean(tsint)


import torch._vmap_internals

vsqrl_pytorch_nice = torch._vmap_internals.vmap(sqrl_pytorch_where)


def vsqrl_pytorch(x):
    """
    Hand-vectorized pytorch implementation, assuming x is rank 3
    """
    y = torch.sum(x, (1, 2), keepdim=True)
    y_lt_0 = (y < 0).repeat((1, *x.size()[1:]))
    t = torch.where(y_lt_0, -0.125 * x, 1 / 2 * x ** 2)
    tsint = torch.sin(t) * t
    return torch.mean(tsint, (1, 2))


# run-bench: Define a range of values at which to call the methods
def vsqrl_bench_configs():
    yield torch.randn((10, 4, 4))
    yield torch.randn((1000, 4, 4))
    yield torch.randn((1000, 16, 16))
