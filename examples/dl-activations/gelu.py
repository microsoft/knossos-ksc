from math import sqrt, tanh, erf, exp
import torch

import ksc.torch_frontend as knossos

import ksc.compile
from ksc.torch_frontend import cpp_string_to_autograd_function


embedded_cflags = ksc.compile.default_cflags


embedded_cflags_opts = ksc.compile.CFlags.GCCOnly(
    ["-march=native", "-funroll-loops", "-ffast-math", "-mprefer-vector-width=512"]
)


embedded_cflags_opts_extra = ksc.compile.CFlags.GCCOnly(
    ["-ffp-contract=fast", "-flto", "-fno-semantic-interposition"]
)


cpp_inlined_map = """
        #include "knossos.h"

        namespace ks{
        tensor<1, ks::Float> vgelu(ks::allocator * $alloc, tensor<1, ks::Float> t) {
            auto tdata = t.data();
            auto ret = tensor<1, ks::Float>::create($alloc, t.size());
            auto retdata = ret.data();
            auto sqrt_2 = sqrt(2.0);
            for (int i = 0, ne = t.num_elements(); i != ne; ++i) {
                ks::Float c$1;
                ks::Float x = tdata[i];
                c$1 = 0.5 * x * (1.0 + erf(x / sqrt_2));
                retdata[i] = c$1;
            }
            return ret;
        }

        tensor<1, ks::Float> sufrev_vgelu(ks::allocator * $alloc, tensor<1, ks::Float> t, tensor<1, ks::Float> dret) {
            auto tdata = t.data();
            auto dretdata = dret.data();
            auto ret = tensor<1, ks::Float>::create($alloc, t.size());
            auto retdata = ret.data();
            auto sqrt_2_div_pi = sqrt(2.0 / 3.14159);
            auto sqrt_2 = sqrt(2.0);
            for (int i = 0, ne = t.num_elements(); i != ne; ++i) {
                ks::Float c$1;
                ks::Float x = tdata[i];
                ks::Float dreti = dretdata[i];
                c$1 = 0.5 * (1.0 + erf(x / sqrt_2) + x * sqrt_2_div_pi * exp(-x*x/2.0));
                retdata[i] = c$1 * dreti;
            }
            return ret;
        }
        }
        """


embedded_cpp_entry_points = """
#include "knossos-entry-points-torch.h"

torch::Tensor entry(torch::Tensor t) {
    using namespace ks::entry_points;
    auto ks_t = convert_to_ks_viewing_tensordata<ks::tensor<1, ks::Float>>(t);
    auto ks_ret = ks::vgelu(&g_alloc, ks_t);
    return convert_from_ks<torch::Tensor>(ks_ret);
}

torch::Tensor entry_vjp(torch::Tensor t, torch::Tensor dret) {
    using namespace ks::entry_points;
    auto ks_t = convert_to_ks_viewing_tensordata<ks::tensor<1, ks::Float>>(t);
    auto ks_dret = convert_to_ks_viewing_tensordata<ks::tensor<1, ks::Float>>(dret);
    auto ks_ret = ks::sufrev_vgelu(&g_alloc, ks_t, ks_dret);
    return convert_from_ks<torch::Tensor>(ks_ret);
}
"""


def sigmoid(x):
    return 1 / (1 + exp(-x))


# Gelu and activations
@knossos.elementwise
def vgelu(x: float) -> float:
    return 0.5 * x * (1.0 + erf(x / sqrt(2)))


def vgelu_embedded_cpp_inlined_map():
    return cpp_string_to_autograd_function(
        cpp_inlined_map + embedded_cpp_entry_points,
        "ksc_dl_activations__manual__vgelu_embedded_cpp_inlined_map",
        extra_cflags=embedded_cflags,
    )


def vgelu_embedded_cpp_inlined_map_flags():
    return cpp_string_to_autograd_function(
        cpp_inlined_map + embedded_cpp_entry_points,
        "ksc_dl_activations__manual__vgelu_embedded_cpp_inlined_map_flags",
        extra_cflags=embedded_cflags + embedded_cflags_opts,
    )


def vgelu_embedded_cpp_inlined_map_flags_extra():
    return cpp_string_to_autograd_function(
        cpp_inlined_map + embedded_cpp_entry_points,
        "ksc_dl_activations__manual__vgelu_embedded_cpp_inlined_map_flags_extra",
        extra_cflags=embedded_cflags
        + embedded_cflags_opts
        + embedded_cflags_opts_extra,
    )


def vgelu_embedded_cpp_aten():
    return cpp_string_to_autograd_function(
        """
    torch::Tensor entry(torch::Tensor x) {
        return 0.5 * x * (1.0 + erf(x / sqrt(2.0)));
    }

    torch::Tensor entry_vjp(torch::Tensor x, torch::Tensor dret) {
        auto sqrt_2_div_pi = sqrt(2.0 / 3.14159);
        return 0.5 * (1.0 + erf(x / sqrt(2.0)) + x * sqrt_2_div_pi * exp(-x*x/2.0)) * dret;
    }
    """,
        "ksc_dl_activations__manual__vgelu_embedded_cpp_aten",
        extra_cflags=embedded_cflags_opts + embedded_cflags,
    )


def gelu_approx_sigmoid(x: float) -> float:
    # From https://github.com/hendrycks/GELUs: fast but somewhat inaccurate
    return sigmoid(1.702 * x) * x


def gelu_approx_tanh(x: float) -> float:
    # From https://github.com/microsoft/onnxruntime/pull/3398/files
    B = 0.7978845608028654  # sqrt(2.0 / M_PI)
    C = 0.035677408136300125  # 0.044715 * sqrt(2.0 / M_PI)

    return 0.5 * (1 + tanh(x * (C * x * x + B))) * x


def vgelu_pytorch(x: torch.Tensor):
    return 0.5 * x * (1 + torch.erf(x / sqrt(2)))


def vgelu_bench_configs():
    yield torch.randn((16,))
    yield torch.randn((255 * 255,))
    yield torch.randn((1024 * 1024,))
