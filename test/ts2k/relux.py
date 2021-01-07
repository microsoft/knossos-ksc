import torch


@torch.jit.script
def relux(x: float):
    if x < 0.0:
        return 0.0
    else:
        return x * x


@torch.jit.script
def main():
    r1 = relux(-1.0)
    r2 = relux(2.7)
    print(r2)
