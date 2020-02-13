from functools import wraps

from ksc.tracing import node
from ksc.tracing.jitting import make_edef
from ksc.tracing.functions.type_propagation_rules import (
    elementwise,
    first_arg,
    pooling_type_prop_rule,
    conv_2d_type_prop_rule
)
from ksc.utils import ShapeType

relu = make_edef("relu", ["x"], elementwise)

normalize_2d = make_edef("normalize_2d", ["x", "weights"], first_arg)
batch_norm_2d = make_edef("batch_norm_2d", ["x", "weights"], first_arg)

def conv_2d_no_bias(x, weights, ksizes, strides, padding="SAME"):
    ss = "s{0}{1}".format(*strides)
    return make_edef(
        f"conv_2d_no_bias_{padding.lower()}",
        ["x", "weights", "ksizes", "strides"],
        conv_2d_type_prop_rule(padding)
    )(x, weights, ksizes, strides)

def _pooling_name(pooling_type, padding):
    return f"{pooling_type}_{padding.lower()}"

def max_pool(x, pool_size, strides, padding="VALID"):
    return make_edef(
        _pooling_name("max_pool", padding),
        ["x", "pool_size", "strides"],
        pooling_type_prop_rule(padding)
    )(x, pool_size, strides)

def avg_pool(x, pool_size, strides, padding="VALID"):
    return make_edef(
        _pooling_name("avg_pool", padding),
        ["x", "pool_size", "strides"],
        pooling_type_prop_rule(padding)
    )(x, pool_size, strides)

log_softmax = make_edef("log_softmax", ["x"], first_arg)
