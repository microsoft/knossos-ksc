from typing import Iterator, Callable
import torch


# Slow implementation, to be improved
# See https://github.com/pytorch/pytorch/issues/8304
def elementwise_apply_pt18(f, x : torch.Tensor):
  # TODO: torch.vmap in 1.9
  y = torch.zeros_like(x)
  sz = x.shape
  if len(sz) == 1:
    for i in range(sz[0]):
      y[i] = f(x[i])

  elif len(sz) == 2:
    for i in range(sz[0]):
      for j in range(sz[1]):
        y[i,j] = f(x[i,j])

  elif len(sz) == 3:
    for i in range(sz[0]):
      for j in range(sz[1]):
        for k in range(sz[2]):
          y[i,j,k] = f(x[i,j,k])

  else:
    raise NotImplementedError("lvmap")
  
  return y

# Slow implementation, to be improved
def elementwise_apply(f : Callable[[float], float], x : torch.Tensor):
  return elementwise_apply_pt18(f, x)
