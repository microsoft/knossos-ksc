from functools import wraps

from ksc.tracing.function import Trace, TraceableFunction
from ksc.tracing.jitting import make_edef
from ksc.tracing.functions.type_propagation_rules import (
    elementwise,
    first_arg,
    pooling_type_prop_rule,
    conv_2d_type_prop_rule_from_padding_type
)
from ksc.tracing.node import Node
from ksc.utils import ShapeType

relu = make_edef("relu", ["x"], elementwise)

normalize_2d = make_edef("normalize_2d", ["x", "weights"], first_arg)
batch_norm_2d = make_edef("batch_norm_2d", ["x", "weights"], first_arg)

def _get_padding(shape_in, shape_out, window_sizes, strides):
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
    def __init__(self, name, arg_names, padding, shape_prop_function):
        super().__init__(f"{name}_{{0}}{{1}}{{2}}{{3}}", arg_names)
        self._internal_function = make_edef(
            name,
            arg_names + ["padding"],
            self._internal_shape_prop_function
        )
        self.padding = padding
        self._proto_shape_prop_function = shape_prop_function
        self._shape_prop_function = shape_prop_function(padding)

    def _internal_shape_prop_function(self, *args):
        x = args[0]
        padding = args[-1]
        x_shape, x_type = x.shape_type
        pad_w, pad_h = padding.data
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
        padding = _get_padding((w, h), (w_o, h_o), ksizes, strides)
        print(f"In RequirePadding2d.trace(): padding_type={self.padding}, padding={padding}")
        body = self._internal_function(*args, padding)
        shape_types = tuple([arg.shape_type for arg in args])

        # specialize the name to avoid cache clash
        self._name = self._name.format(*padding[0], *padding[1])
        return Trace(body, ShapeType(shape, type), shape_types)

def conv_2d_no_bias(x, weights, ksizes, strides, padding="SAME"):
    conv =RequirePadding2d(
        "conv_2d_no_bias",
        ["x", "weights", "ksizes", "strides"],
        padding,
        conv_2d_type_prop_rule_from_padding_type
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

log_softmax = make_edef("log_softmax", ["x"], first_arg)
