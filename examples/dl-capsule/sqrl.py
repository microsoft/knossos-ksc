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

vsqrl = knossos.vmap(sqrl)


# run-bench: Define a range of values at which to call the methods
def vsqrl_bench_configs():
    yield torch.randn((10, 4, 4))
    yield torch.randn((1000, 4, 4))
    yield torch.randn((1000, 16, 16))
