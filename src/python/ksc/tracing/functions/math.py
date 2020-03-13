import ksc
from ksc.type import Type
from ksc.tracing.functions import core
from ksc.tracing.functions.type_propagation_rules import unique_element_type
from ksc.utils import ShapeType

def mat_mat_mul_prop_rule(x, y):
    x_shape, x_type = x.shape_type
    y_shape, y_type = y.shape_type
    el_type = unique_element_type([x_type, y_type])
    output_type = Type.Vec(Type.Vec(el_type))
    assert x_type.ndim == 2
    assert y_type.ndim == 2

    m, k1 = x_shape
    k2, n = y_shape
    assert k1 == k2
    return ShapeType((m, n), output_type)

def mat_vec_broadcast_prop_rule(m, v):
    m_shape, m_type = m.shape_type
    v_shape, v_type = v.shape_type
    el_type = unique_element_type([m_type, v_type])
    assert m_type.ndim == 2
    assert v_type.ndim == 1

    n, d = m_shape
    d2 = v_shape[0]
    assert d == d2
    return ShapeType((n, d), Type.Vec(Type.Vec(el_type)))

dot = ksc.tracing.make_edef(
    "dot", ["x", "y"], mat_mat_mul_prop_rule,
    # the shape is (m, n) if x = (m, k) and y = (k, n)
    lambda x, y: core.make_tuple(x.shape[0], y.shape[1]),
    # just count the number of flops
    lambda x, y: (lambda s_x, s_y: s_x[0] * s_x[1] * s_y[1] * 2)(x.shape, y.shape)
)

broadcast_add = ksc.tracing.make_edef(
    "broadcast_add", ["m", "v"], mat_vec_broadcast_prop_rule,
    lambda m, v: m.shape,
    lambda m, v: m.size
)

def transpose_prop_rule(x):
    x_shape, x_type = x.shape_type
    assert x_type.ndim == 2
    m, n = x_shape
    return ShapeType((n, m), x_type)

transpose = ksc.tracing.make_edef("transpose", ["x"], transpose_prop_rule)