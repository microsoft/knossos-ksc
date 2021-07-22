import torch
import ksc.torch_frontend as knossos

import ksc.compile
from ksc.torch_frontend import cpp_string_to_autograd_function

embedded_cflags = ksc.compile.default_cflags


embedded_cflags_opts = ksc.compile.CFlags.GCCOnly(
    ["-march=native", "-funroll-loops", "-ffast-math", "-mprefer-vector-width=512",]
)


cpp_inlined_map = """
        #include "knossos.h"

        namespace ks{
        tensor<2, ks::Float> sqrl(ks::allocator * $alloc, tensor<2, ks::Float> t) {
            auto tdata = t.data();
            ks::Float sum = 0.0;
            auto ne = t.num_elements();

            for (int i = 0; i != ne; ++i) {
                sum += tdata[i];
            }

            ks::Float outsum = 0.0;

            if (sum < 0.0) {
                for (int i = 0; i != ne; ++i) {
                    ks::Float r = -0.125 * tdata[i];
                    outsum += sin(r) * r;
                }
            } else {
                for (int i = 0; i != ne; ++i) {
                    ks::Float r = 0.5 * tdata[i] * tdata[i];
                    outsum += sin(r) * r;
                }
            }

            ks::Float outmean = outsum / ne;

            auto outtensor = tensor<2, ks::Float>::create($alloc, make_Tuple(1, 1));
            outtensor.data()[0] = outmean;
            return outtensor;
        }

        tensor<2, ks::Float> sufrev_sqrl(ks::allocator * $alloc, tensor<2, ks::Float> t, tensor<2, ks::Float> dret) {
            auto tdata = t.data();
            ks::Float sum = 0.0;
            auto ne = t.num_elements();

            auto dretdata = dret.data()[0];
            auto ret = tensor<2, ks::Float>::create($alloc, t.size());
            auto retdata = ret.data();

            for (int i = 0; i != ne; ++i) {
                sum += tdata[i];
            }

            if (sum < 0.0) {
                for (int i = 0; i != ne; ++i) {
                    ks::Float r = -0.125 * tdata[i];
                    ks::Float dr = (sin(r) + cos(r) * r) / ne;
                    retdata[i] = -0.125 * dr;
                }
            } else {
                for (int i = 0; i != ne; ++i) {
                    ks::Float r = 0.5 * tdata[i] * tdata[i];
                    ks::Float dr = (sin(r) + cos(r) * r) / ne;
                    retdata[i] = tdata[i] * dr;
                }
            }

            return ret;
        }
        }
        """


embedded_cpp_entry_points = """
#include "knossos-entry-points-torch.h"

torch::Tensor entry(torch::Tensor t) {
    using namespace ks::entry_points;
    auto ks_t = convert_argument<ks::tensor<2, ks::Float>>(t);
    auto ks_ret = ks::sqrl(&g_alloc, ks_t);
    return convert_return_value<torch::Tensor>(ks_ret);
}

torch::Tensor entry_vjp(torch::Tensor t, torch::Tensor dret) {
    using namespace ks::entry_points;
    auto ks_t = convert_argument<ks::tensor<2, ks::Float>>(t);
    auto ks_dret = convert_argument<ks::tensor<2, ks::Float>>(dret);
    auto ks_ret = ks::sufrev_sqrl(&g_alloc, ks_t, ks_dret);
    return convert_return_value<torch::Tensor>(ks_ret);
}
"""


def sqrl_embedded_cpp_inlined_map():
    return cpp_string_to_autograd_function(
        cpp_inlined_map + embedded_cpp_entry_points,
        "ksc_dl_activations__manual__sqrl_embedded_cpp_inlined_map",
        extra_cflags=embedded_cflags,
    )


def sqrl_embedded_cpp_inlined_map_flags():
    return cpp_string_to_autograd_function(
        cpp_inlined_map + embedded_cpp_entry_points,
        "ksc_dl_activations__manual__sqrl_embedded_cpp_inlined_map_flags",
        extra_cflags=embedded_cflags + embedded_cflags_opts,
    )


# run-bench: Knossos source, and "nice" PyTorch implementation
# BEGINDOC
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


# ENDDOC

# run-bench: PyTorch "fast" implementation
def sqrl_pytorch(x: torch.Tensor):
    return sqrl(x)


# run-bench: PyTorch "nice" implementation
def sqrl_pytorch_nice(x: torch.Tensor):
    return sqrl(x)


# run-bench: Define a range of values at which to call the methods
def sqrl_bench_configs():
    yield torch.randn((4, 4))
    yield torch.randn((16, 16))
    # yield torch.randn((128, 64))  TODO: uncomment after fixing SUF OOM issue
