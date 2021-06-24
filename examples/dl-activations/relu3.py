import torch
from torch import nn
import os
from ksc.torch_utils import elementwise_apply_hack
from collections import OrderedDict
import ksc.expr as expr
from ksc.type import Type
from ksc.torch_frontend import (
    ksc_string_to_autograd_function,
    cpp_string_to_autograd_function,
)
import torch._vmap_internals

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


def vrelu3_embedded_ks_checkpointed_map():
    return ksc_string_to_autograd_function(
        """(def relu3 Float (x : Float)
             (if (lt x 0.0)
                 0.0
             (if (lt x 1.0)
                 (div (mul x (mul x x)) 3.0)
             (sub x (div 2.0 3.0)))))

           (gdef suffwdpass [relu3 Float])
           (gdef sufrevpass [relu3 Float])
           (gdef sufrev [relu3 Float])

           (def [vrelu3 (Vec Float)] (Vec Float)
                (t : Vec Float)
                (map (lam (ti : Float) (relu3 ti)) t))

           (def [sufrev [vrelu3 (Vec Float)]] (Vec Float)
                ((t : Vec Float) (dret : Vec Float))
                ; TODO: 1.0 should be dret[i] - luckily we are called with dret==1.0
                (map (lam (ti : Float) ([sufrev [relu3 Float]] ti 1.0)) t))
        """,
        expr.StructuredName(("vrelu3", Type.Tensor(1, Type.Float))),
        generate_lm=False,
    )


def vrelu3_embedded_cpp_inlined_map():
    return cpp_string_to_autograd_function(
        """
        namespace ks{
        tensor<1, double> vrelu3(ks::allocator * $alloc, tensor<1, double> t) {
            auto tdata = t.data();
            auto ret = tensor<1, double>::create($alloc, t.size());
            auto retdata = ret.data();
            for (int i = 0, ne = t.num_elements(); i != ne; ++i) {
                double c$1;
                double x = tdata[i];
                if (x < 0.0) {
                    c$1 = 0.0;
                } else {
                    if (x < 1.0) {
                        c$1 = x * x * x / 3.0;
                    } else {
                        c$1 = x - 2.0 / 3.0;
                    }
                }
                retdata[i] = c$1;
            }
            return ret;
        }

        tensor<1, double> sufrev_vrelu3(ks::allocator * $alloc, tensor<1, double> t, tensor<1, double> dret) {
            auto tdata = t.data();
            auto dretdata = dret.data();
            auto ret = tensor<1, double>::create($alloc, t.size());
            auto retdata = ret.data();
            for (int i = 0, ne = t.num_elements(); i != ne; ++i) {
                double c$1;
                double x = tdata[i];
                double dreti = dretdata[i];
                if (x < 0.0) {
                    c$1 = 0.0;
                } else {
                    if (x < 1.0) {
                        c$1 = x * x;
                    } else {
                        c$1 = 1.0;
                    }
                }
                retdata[i] = c$1 * dreti;
            }
            return ret;
        }
        }
        """,
        "vrelu3",
        generate_lm=False,
    )


def vrelu3_embedded_cpp_mask():
    return cpp_string_to_autograd_function(
        """
        namespace ks{
        tensor<1, double> vrelu3(ks::allocator * $alloc, tensor<1, double> t) {
            auto tdata = t.data();
            auto ret = tensor<1, double>::create($alloc, t.size());
            auto retdata = ret.data();
            for (int i = 0, ne = t.num_elements(); i != ne; ++i) {
                double c$1;
                double x = tdata[i];

                auto val0to1 = x * x * x / 3.0;
                auto val1up = x - 2.0 / 3.0;
                auto in0to1 = x <= 1;

                c$1 = (x>0)*(in0to1*val0to1 + (!in0to1)*val1up);

                retdata[i] = c$1;
            }
            return ret;
        }

        tensor<1, double> sufrev_vrelu3(ks::allocator * $alloc, tensor<1, double> t, tensor<1, double> dret) {
            auto tdata = t.data();
            auto dretdata = dret.data();
            auto ret = tensor<1, double>::create($alloc, t.size());
            auto retdata = ret.data();
            for (int i = 0, ne = t.num_elements(); i != ne; ++i) {
                double c$1;
                double x = tdata[i];
                double dreti = dretdata[i];
                auto val0to1 = x * x * dreti;

                auto in0to1 = x <= 1;

                c$1 = (x>0) ? (in0to1 ? val0to1 : dreti) : 0.0;

                retdata[i] = c$1;
            }
            return ret;
        }
        }
        """,
        "vrelu3",
        generate_lm=False,
    )


def vrelu3_embedded_INCORRECT_cpp_inlined_map_no_if():
    return cpp_string_to_autograd_function(
        """
        namespace ks{
        tensor<1, double> vrelu3(ks::allocator * $alloc, tensor<1, double> t) {
            auto tdata = t.data();
            auto ret = tensor<1, double>::create($alloc, t.size());
            auto retdata = ret.data();
            for (int i = 0, ne = t.num_elements(); i != ne; ++i) {
                double x = tdata[i];
                retdata[i] = x * x * x / 3.0;
            }
            return ret;
        }

        tensor<1, double> sufrev_vrelu3(ks::allocator * $alloc, tensor<1, double> t, tensor<1, double> dret) {
            auto tdata = t.data();
            auto dretdata = dret.data();
            auto ret = tensor<1, double>::create($alloc, t.size());
            auto retdata = ret.data();
            for (int i = 0, ne = t.num_elements(); i != ne; ++i) {
                double x = tdata[i];
                double dreti = dretdata[i];
                retdata[i] = x * x * dreti;
            }
            return ret;
        }
        }
        """,
        "vrelu3",
        generate_lm=False,
    )


def vrelu3_embedded_ks_checkpointed_map_handwritten_relu3():
    return ksc_string_to_autograd_function(
        """(def relu3 Float (x : Float)
             (if (lt x 0.0)
                 0.0
             (if (lt x 1.0)
                 (div (mul x (mul x x)) 3.0)
             (sub x (div 2.0 3.0)))))

           (def [sufrev [relu3 Float]] Float ((x : Float) (ddr : Float))
               (if (lt x 0.0)
                   0.0
               (if (lt x 1.0)
                   (mul x (mul x ddr))
               ddr)))

           (def [vrelu3 (Vec Float)] (Vec Float)
                (t : Vec Float)
                (map (lam (ti : Float) (relu3 ti)) t))

           (def [sufrev [vrelu3 (Vec Float)]] (Vec Float)
                ((t : Vec Float) (dret : Vec Float))
                ; TODO: 1.0 should be dret[i] - luckily we are called with dret==1.0
                (map (lam (ti : Float) ([sufrev [relu3 Float]] ti 1.0)) t))
        """,
        expr.StructuredName(("vrelu3", Type.Tensor(1, Type.Float))),
        generate_lm=False,
    )


def vrelu3_embedded_ks_checkpointed_map_handwritten_inlined_relu3():
    return ksc_string_to_autograd_function(
        """(def [vrelu3 (Vec Float)] (Vec Float)
                (t : Vec Float)
                (map (lam (x : Float)
             (if (lt x 0.0)
                 0.0
             (if (lt x 1.0)
                 (div (mul x (mul x x)) 3.0)
             (sub x (div 2.0 3.0))))) t))

           (def [sufrev [vrelu3 (Vec Float)]] (Vec Float)
                ((t : Vec Float) (dret : Vec Float))
                ; TODO: should be multiplied by dret[i] - luckily we are called with dret==1.0
                (map (lam (x : Float)
               (if (lt x 0.0)
                   0.0
               (if (lt x 1.0)
                   (mul x x)
               1.0))) t))
        """,
        expr.StructuredName(("vrelu3", Type.Tensor(1, Type.Float))),
        generate_lm=False,
    )


def vrelu3_embedded_ks_checkpointed_map_mask():
    return ksc_string_to_autograd_function(
        """(def [vrelu3 (Vec Float)] (Vec Float)
                (t : Vec Float)
                (map (lam (x : Float)
                   (let (val0to1 (mul x (mul x (div x 3.0))))
                   (let (val1up (sub x (div 2.0 3.0)))
                   (let (in0to1 (lte x 1.0))
                      (mul (bool_to_float (gt x 0.0))
                           (add (mul (bool_to_float in0to1) val0to1)
                                (mul (bool_to_float (not in0to1)) val1up))))))) t))

           (def [sufrev [vrelu3 (Vec Float)]] (Vec Float)
                ((t : Vec Float) (dret : Vec Float))
                (map2 (lam (x_ddri : Tuple Float Float)
                   (let ((x ddri) x_ddri)
                   (let (val0to1 (mul x x))
                   (let (val1up 1.0)
                   (let (in0to1 (lte x 1.0))
                      (mul (mul (bool_to_float (gt x 0.0))
                                (add (mul (bool_to_float in0to1) val0to1)
                                     (mul (bool_to_float (not in0to1)) val1up)))
                           ddri)))))) t dret))
        """,
        expr.StructuredName(("vrelu3", Type.Tensor(1, Type.Float))),
        generate_lm=False,
    )


def vrelu3_embedded_INCORRECT_ks_upper_bound_via_map():
    return ksc_string_to_autograd_function(
        """(def relu3 Float (x : Float) 0.0)

           (def [sufrev [relu3 Float]] Float ((x : Float) (ddr : Float)) ddr)

           (def [vrelu3 (Vec Float)] (Vec Float)
                (t : Vec Float)
                (map (lam (ti : Float) (relu3 ti)) t))

           (def [sufrev [vrelu3 (Vec Float)]] (Vec Float)
                ((t : Vec Float) (dret : Vec Float))
                ; TODO: 1.0 should be dret[i] - luckily we are called with dret==1.0
                (map (lam (ti : Float) ([sufrev [relu3 Float]] ti 1.0)) t))
        """,
        expr.StructuredName(("vrelu3", Type.Tensor(1, Type.Float))),
        generate_lm=False,
    )


def vrelu3_embedded_INCORRECT_ks_upper_bound():
    return ksc_string_to_autograd_function(
        """; These are not correct but they are as fast as a Knossos
           ; implementation could possibly be.
           (def [vrelu3 (Vec Float)] (Vec Float)
                (t : Vec Float) t)

           (def [sufrev [vrelu3 (Vec Float)]] (Vec Float)
                ((t : Vec Float) (dret : Vec Float))
                dret)
        """,
        expr.StructuredName(("vrelu3", Type.Tensor(1, Type.Float))),
        generate_lm=False,
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


# With torch 1.9.0 this leads to
# RuntimeError: Batching rule not implemented for aten::is_nonzero. We could not generate a fallback.
# See https://msrcambridge.visualstudio.com/Knossos/_backlogs/backlog/Knossos%20Team/Goals/?workitem=19587
# vrelu3_pytorch_nice = torch._vmap_internals.vmap(relu3_pytorch_nice)


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
    yield torch.randn((16,))
    yield torch.randn((255 * 255,))
    yield torch.randn((1000 * 1000,))


# yield torch.randn((256,256)) too slow to bench...


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
