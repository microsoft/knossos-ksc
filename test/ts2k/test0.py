import torch


@torch.jit.script
def f(x):
    return x + 4.0


@torch.jit.script
def main():
    t = torch.tensor([[1.2, 3.4, 5.6], [2.3, 4.5, 6.7]])
    t2 = f(t)
    print("Hello world from TorchScript -> Knossos!")
