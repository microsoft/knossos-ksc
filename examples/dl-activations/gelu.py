from math import sqrt, tanh, erf, exp
import torch

import ksc.torch_frontend as knossos

from ksc.torch_utils import elementwise_apply_hack


def sigmoid(x):
    return 1 / (1 + exp(-x))


# Gelu and activations
@knossos.register
def gelu(x: float) -> float:
    return 0.5 * x * (1.0 + erf(x / sqrt(2)))


def gelu_approx_sigmoid(x: float) -> float:
    # From https://github.com/hendrycks/GELUs: fast but somewhat inaccurate
    return sigmoid(1.702 * x) * x


def gelu_approx_tanh(x: float) -> float:
    # From https://github.com/microsoft/onnxruntime/pull/3398/files
    B = 0.7978845608028654  # sqrt(2.0 / M_PI)
    C = 0.035677408136300125  # 0.044715 * sqrt(2.0 / M_PI)

    return 0.5 * (1 + tanh(x * (C * x * x + B))) * x


@knossos.register
def vgelu(x: torch.Tensor):
    return elementwise_apply_hack("gelu", x)


def vgelu_pytorch(x: torch.Tensor):
    return 0.5 * x * (1 + torch.erf(x / sqrt(2)))


def vgelu_bench_configs():
    yield torch.randn((4,))
    yield torch.randn((16,))
