from functools import wraps

import ksc
from ksc.tracing.function import Trace, TraceableFunction
from ksc.tracing.jitting import make_edef
from ksc.tracing.functions import core
from ksc.tracing.functions.type_propagation_rules import (
    elementwise,
    first_arg,
    pooling_type_prop_rule,
    conv_2d_type_prop_rule_from_padding_type
)
from ksc.tracing.node import Node
from ksc.utils import ShapeType

relu = make_edef(
    "relu", ["x"], elementwise,
    lambda x: x.shape,
    lambda x: x.size
)

sigmoid = make_edef(
    "sigmoid", ["x"], elementwise,
    lambda x: x.shape,
    lambda x: x.size
)

normalize_2d = make_edef(
    "normalize_2d", ["x", "weights"], first_arg,
    lambda x, w: x.shape,
    lambda x, w: x.size * 3
)

batch_norm_2d = make_edef(
    "batch_norm_2d", ["x", "weights"], first_arg,
    lambda x, w: x.shape,
    lambda x, w: x.size * 10
)

def _get_paddings(shape_in, shape_out, window_sizes, strides):
    def get_padding_1d(size_in, size_out, window_size, stride):
        pad_len = max(0, (size_out - 1) * stride + window_size - size_in)
        return (pad_len // 2, pad_len - pad_len // 2)

    w, h = shape_in
    w_o, h_o = shape_out
    stride_w, stride_h = strides.data
    k_w, k_h = window_sizes.data
    return (get_padding_1d(w, w_o, k_w, stride_w), get_padding_1d(h, h_o, k_h, stride_h))

class RequirePadding2d(TraceableFunction):
    is_edef = False
    is_builtin = False
    def __init__(self, name, arg_names, padding, shape_prop_function, shape_def=None, cost_def=None):
        super().__init__(f"{name}_{{0}}{{1}}{{2}}{{3}}", arg_names)
        self._internal_function = make_edef(
            name,
            arg_names + ["paddings"],
            self._internal_shape_prop_function,
            shape_def,
            cost_def
        )
        self.padding = padding
        self._proto_shape_prop_function = shape_prop_function
        self._shape_prop_function = shape_prop_function(padding)

    def _internal_shape_prop_function(self, *args):
        x = args[0]
        paddings = args[-1]
        x_shape, x_type = x.shape_type
        pad_w, pad_h = paddings.data
        b, c, w, h = x_shape
        new_x_shape = (b, c, w + pad_w[0] + pad_w[1], h + pad_h[0] + pad_h[1])
        new_x = Node(x.name, new_x_shape, x_type)
        type_prop_rule = self._proto_shape_prop_function("VALID")
        return type_prop_rule(new_x, *args[1:-1])

    def trace(self, *args):
        shape, type = self._shape_prop_function(*args)
        x = args[0]
        ksizes = args[-2]
        strides = args[-1]
        w, h = x.shape_type.shape[2:]
        w_o, h_o = shape[2:]
        paddings = _get_paddings((w, h), (w_o, h_o), ksizes, strides)
        print(f"In RequirePadding2d.trace(): padding_type={self.padding}, paddings={paddings}")
        body = self._internal_function(*args, paddings)
        shape_types = tuple([arg.shape_type for arg in args])

        # specialize the name to avoid cache clash
        self._name = self._name.format(*paddings[0], *paddings[1])
        return Trace(body, ShapeType(shape, type), shape_types)

def cost_conv_2d_no_bias(c0, c1, c2):
    def f(x, weights, ksizes, strides, paddings):
        b, c, w, h = x.shape
        k_w, k_h = ksizes
        stride_w, stride_h = strides
        m = core.get_vector_size(weights)
        w_o = core.to_float(w // stride_w)
        h_o = core.to_float(h // stride_h)
        flops = (core.to_float(b)
                * w_o * h_o
                * core.to_float(k_w *k_h)
                * core.to_float(c)
                * core.to_float(m))

        return c0 + c1 * (flops ** 0.5) + c2 * flops
    return f

@ksc.trace
def _ceil_div(x, y):
    return (x + y - 1) // y

def shape_conv_2d_no_bias(x, weights, ksizes, strides, paddings):
    b, _, w, h = x.shape
    m, _, _, _ = weights.shape
    pad_w, pad_h = paddings
    k_w, k_h = ksizes
    stride_w, stride_h = strides
    pad_w, pad_h = paddings
    w_new = _ceil_div(w + pad_w[0] + pad_w[1] - k_w + 1, stride_w)
    h_new = _ceil_div(h + pad_h[0] + pad_h[1] - k_h + 1, stride_h)
    return core.make_tuple(b, m, w_new, h_new)

def conv_2d_no_bias(x, weights, ksizes, strides, padding="SAME"):
    conv =RequirePadding2d(
        "conv_2d_no_bias",
        ["x", "weights", "ksizes", "strides"],
        padding,
        conv_2d_type_prop_rule_from_padding_type,
        shape_conv_2d_no_bias,
        cost_conv_2d_no_bias(0.0, 0.0, 1.0)
    )
    return conv(x, weights, ksizes, strides)

def _pooling_name(pooling_type, padding):
    return f"{pooling_type}_{padding.lower()}"

def max_pool(x, pool_size, strides, padding="VALID"):
    pool = RequirePadding2d(
        "max_pool",
        ["x", "pool_size", "strides"],
        padding,
        pooling_type_prop_rule
    )
    return pool(x, pool_size, strides)

def avg_pool(x, pool_size, strides, padding="VALID"):
    pool = RequirePadding2d(
        "avg_pool",
        ["x", "pool_size", "strides"],
        padding,
        pooling_type_prop_rule
    )
    return pool(x, pool_size, strides)

log_softmax = make_edef(
    "log_softmax", ["x"], first_arg,
    lambda x: x.shape,
    lambda x: x.size * 10
)
