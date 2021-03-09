from typing import Iterator, Callable
import torch

# Slow implementation, to be improved
def vmap(f : Callable[[float], float], x : torch.Tensor):
  out = torch.Tensor([f(e) for e in torch.flatten(x)])
  return torch.reshape(out, x.shape)

