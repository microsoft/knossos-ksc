import torch

import ksc.torch_frontend as knossos


@knossos.register(generate_lm=True)
def conv1(a: torch.Tensor, b: torch.Tensor) -> torch.Tensor:
    (na,) = a.size()
    (nb,) = b.size()
    out = torch.empty(na - nb + 1)
    for ia in range(na - nb + 1):
        out[ia] = sum(torch.tensor([a[ia + ib] * b[ib] for ib in range(na)]))

    return out


def test_conv1():
    a = torch.randn(128)
    b = torch.randn(3)
    c = conv1(a, b)
    assert c.size() == (126,)


@knossos.register(generate_lm=True)
def conv2(a: torch.Tensor, b: torch.Tensor) -> torch.Tensor:
    ma, na = a.size()
    mb, nb = b.size()
    out = torch.empty(ma - mb + 1, na - nb + 1)
    for ia in range(ma - mb + 1):
        for ja in range(na - nb + 1):
            out[ia, ja] = sum(
                torch.tensor(
                    [
                        sum(
                            torch.tensor(
                                [a[ia + ib, ja + jb] * b[ib, jb] for ib in range(ma)]
                            )
                        )
                        for jb in range(na)
                    ]
                )
            )
    return out


def test_conv2d():
    a = torch.randn(128, 128)
    b = torch.randn(3, 3)
    c = conv2(a, b)
    assert c.size() == (126, 126)
