from typing import Callable
import torch


# Slow implementation, to be improved
# See https://github.com/pytorch/pytorch/issues/8304
@torch.jit.ignore
def elementwise_apply_pt18(f, x: torch.Tensor) -> torch.Tensor:
    # TODO: torch.vmap in 1.9
    # NOTE: torch.vmap still isn't in stable in PyTorch 1.9, it can be called via internal apis: torch._vmap_internals
    # https://github.com/pytorch/pytorch/issues/42368
    y = torch.zeros_like(x)
    sz = x.shape
    if len(sz) == 1:
        for i in range(sz[0]):
            y[i] = f(x[i])

    elif len(sz) == 2:
        for i in range(sz[0]):
            for j in range(sz[1]):
                y[i, j] = f(x[i, j])

    elif len(sz) == 3:
        for i in range(sz[0]):
            for j in range(sz[1]):
                for k in range(sz[2]):
                    y[i, j, k] = f(x[i, j, k])

    else:
        raise NotImplementedError("lvmap")

    return y


# Slow implementation, to be improved
def elementwise_apply(f: Callable[[float], float], x: torch.Tensor):
    return elementwise_apply_pt18(f, x)


@torch.jit.ignore
def elementwise_apply_hack(f: str, x: torch.Tensor):
    import inspect

    module = inspect.getmodule(inspect.currentframe().f_back)
    for fn in inspect.getmembers(module):
        print(fn)
        fn_name, fn_obj = fn
        if fn_name == f:
            return elementwise_apply_pt18(fn_obj, x)

    assert False
