import torch
from torch import nn
import os
from ksc.torch_utils import elementwise_apply_hack, call_ks
from collections import OrderedDict

# BEGINDOC
def relu3(x: float) -> float:
    """
    Like ReLu, but smoother
    Like GeLu, but cheaper
  """
    if x < 0.0:
        return 0.0
    elif x < 1.0:
        return 1 / 3 * x ** 3
    else:
        return x - 2 / 3


# ENDDOC


# run-bench: Knossos implementation
def vrelu3(x: torch.Tensor):
    return elementwise_apply_hack("relu3", x)


# run-bench: Knossos hand-coded implementation in ks file
def vrelu3_ks_fast(x: torch.Tensor):
    return call_ks(
        """
(def myrelu3 Float (x : Float)
     (if (lt x 0.0)
         0.0
     (if (lt x 1.0)
         (div (mul x (mul x x)) 3.0)
     (sub x (div 2.0 3.0)))))

(gdef suffwdpass [myrelu3 Float])
(gdef sufrevpass [myrelu3 Float])
;(gdef sufrev [myrelu3 Float])

; (def [sufrev [myrelu3 Float]] Float ((x : Float) (ddr : Float)) 0.0)

(def [sufrev [myrelu3 Float]] Float ((x : Float) (ddr : Float))
    (if (lt x 0.0)
        0.0
    (if (lt x 1.0)
        (mul x (mul x ddr))
    ddr)))

;; This is providing the definition for vrelu3_ks_fast_AUX, 
;; which is the "call_ks" target below
(def vrelu3_ks_fast_AUX (Vec Float) (t : Vec Float)
     (map (lam (ti : Float) (myrelu3 ti)) t))

;; we don't need the sufrev of vrelu3_ks_fast_AUX, as it's never called
; (gdef sufrev vrelu3_ks_fast_AUX)

; When ts2ks processes vrelu3_ks_fast, it emits:
(edef vrelu3_ks_fast (Vec Float) (Vec Float))

;; But we provide the definition for sufrev of the *emitted* function
(def [sufrev [vrelu3_ks_fast (Vec Float)]] (Vec Float)
     ((t : Vec Float) (dret : Vec Float))
     ; TODO: 1.0 should be dret[i] - luckily we are called with dret==1.0
     (map (lam (ti : Float) ([sufrev [myrelu3 Float]] ti 1.0)) t))

""",
        "vrelu3_ks_fast_AUX",
        x,
    )


# run-bench: PyTorch reference implementation
def vrelu3_pytorch(x: torch.Tensor):
    mask1_inf = x > 1.0
    mask0_1 = (x > 0.0) & ~mask1_inf
    val_0_1 = 1 / 3 * x ** 3
    val_1_inf = x - 2 / 3

    return mask0_1 * val_0_1 + mask1_inf * val_1_inf


# run-bench: PyTorch "nice" implementation
def relu3_pytorch_nice(x: float) -> float:
    if x < 0.0:
        return 0.0 * x  # Needed for PyTorch, not for Knossos.  See [Note: zeros]
    elif x < 1.0:
        return 1 / 3 * x ** 3
    else:
        return x - 2 / 3


vrelu3_pytorch_nice = torch.vmap(relu3_pytorch_nice)


def vrelu3_cuda_init():
    this_dir = os.path.dirname(__file__)

    vrelu3_cuda = torch.utils.cpp_extension.load(
        "vrelu3_module",
        sources=[
            os.path.join(this_dir, "vrelu3_cuda.cpp"),
            os.path.join(this_dir, "vrelu3_cuda_kernel.cu"),
        ],
    )

    class VReLu3Function(torch.autograd.Function):
        @staticmethod
        def forward(ctx, input):
            output = vrelu3_cuda.forward(input)
            ctx.save_for_backward(input)
            return output

        @staticmethod
        def backward(ctx, grad):
            return vrelu3_cuda.backward(grad.contiguous(), *ctx.saved_variables)

    class VReLu3(torch.nn.Module):
        def __init__(self):
            super(VReLu3, self).__init__()

        def forward(self, input):
            return VReLu3Function.apply(input)

    return VReLu3()


# run-bench: Define a range of values at which to call the methods
def vrelu3_bench_configs():
    yield torch.randn((4,))
    yield torch.randn((16,))
    yield torch.randn((100 * 1000))
    yield torch.randn((1000 * 1000))
    yield torch.randn((2 * 1000 * 1000))


# Note: zeros
# Need to multiply by x for pytorch to get the gradient back through x.
# This is something of a long story, related to tracing.
# Sort of related discussion https://discuss.pytorch.org/t/custom-loss-function-error-element-0-of-tensors-does-not-require-grad-and-does-not-have-grad-fn/87944/16


def plot_relu3(filename):
    import matplotlib.pyplot as plt
    import numpy as np

    t = torch.arange(-3, 3, step=0.1)

    plt.plot(t, vrelu3(t), "b")
    plt.plot(t, vrelu3_pytorch(t), "r--")
    plt.gca().set_aspect(1)
    print(f"Saving figure to {filename}")
    plt.savefig(filename)


def relu3_in_fcdnn():
    # Initialize the model using nn.Sequential
    model = nn.Sequential(
        OrderedDict(
            [
                ("fc1", nn.Linear(784, 256)),
                ("activation1", vrelu3),
                ("fc2", nn.Linear(256, 128)),
                ("bn2", nn.BatchNorm1d(num_features=128)),
                ("activation2", vrelu3),
                ("dropout", nn.Dropout(0.3)),
                ("fc3", nn.Linear(128, 64)),
                ("bn3", nn.BatchNorm1d(num_features=64)),
                ("activation3", vrelu3),
                ("logits", nn.Linear(64, 10)),
                ("logsoftmax", nn.LogSoftmax(dim=1)),
            ]
        )
    )

    # Run training
    # train_model(model)
