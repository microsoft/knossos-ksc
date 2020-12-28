import ksc
from ksc.type import Type
from ksc.tracing.node import Node
from ksc.tracing.functions import core
from ksc.tracing.functions.type_propagation_rules import unique_element_type
from ksc.utils import ShapeType, TensorShape, ScalarShape

def mat_mat_mul_prop_rule(x, y):
    el_type = unique_element_type([x.get_type, y.get_type])
    output_type = Type.Tensor(2, el_type)
    assert el_type.is_scalar
    assert x.get_type.tensor_rank == 2
    assert y.get_type.tensor_rank == 2
    assert isinstance(x.get_shape, TensorShape) and len(x.get_shape.dims) == 2
    assert isinstance(y.get_shape, TensorShape) and len(y.get_shape.dims) == 2

    m, k1 = x.get_shape.dims
    k2, n = y.get_shape.dims
    assert k1 == k2
    out_shape = TensorShape((m, n), ScalarShape)
    return ShapeType(out_shape, output_type)

def mat_vec_broadcast_prop_rule(m, v) -> ShapeType:
    el_type = unique_element_type([m.get_type, v.get_type])
    assert el_type.is_scalar
    assert m.get_type.tensor_rank == 2
    assert v.get_type.tensor_rank == 1
    assert isinstance(m.get_shape, TensorShape) and len(m.get_shape.dims) == 2
    assert isinstance(v.get_shape, TensorShape) and len(v.get_shape.dims) == 1

    assert m.get_shape.dims[1] == v.get_shape.dims[0]
    return ShapeType(m.get_shape, Type.Tensor(2, el_type))

dot = ksc.tracing.make_edef(
    "dot", ["x", "y"], mat_mat_mul_prop_rule,
    # the shape is ((m, n), el_shape) if x_shape = ((m, k), el_shape) and y_shape = ((k, n), el_shape)
    # And all el_shape are scalar atm
    lambda x, y: core.make_tuple(core.make_tuple(x.shape_program[0][0], y.shape_program[0][1]), y.shape_program[1]),
    # just count the number of flops
    lambda x, y: (lambda s_x, s_y: s_x[0] * s_x[1] * s_y[1] * 2)(x.shape_program[0], y.shape_program[0]) 
)

broadcast_add = ksc.tracing.make_edef(
    "broadcast_add", ["m", "v"], mat_vec_broadcast_prop_rule,
    lambda m, v: m.shape_program,
    lambda m, v: core.numel_program(m) 
)

def transpose_shape(s):
    assert len(s.dims) == 2
    m, n = s.dims
    return TensorShape((n, m), s.elem_shape)

def transpose_prop_rule(x):
    assert x.get_type.tensor_rank == 2
    return ShapeType(transpose_shape(x), x.get_type)

transpose = ksc.tracing.make_edef(
    "transpose", ["x"], transpose_prop_rule,
    lambda x: transpose_shape(x.shape_program),
    lambda x: Node.from_data(0)
)

reducemean = ksc.tracing.make_edef(
    "reducemean", ["x"],
    lambda x: ShapeType(ScalarShape, Type.Float),
    lambda x: Node.from_data(0.0).shape_program,
    lambda x: Node.from_data(0)
)
