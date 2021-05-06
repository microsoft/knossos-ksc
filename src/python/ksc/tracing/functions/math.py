import ksc
from ksc.type import Type
from ksc.tracing.node import Node
from ksc.tracing.functions import core
from ksc.tracing.functions.type_propagation_rules import unique_element_type
from ksc.shape import ShapeType, TensorShape, ScalarShape


def mat_mat_mul_prop_rule(x, y):
    el_type = unique_element_type([x.type, y.type])
    output_type = Type.Tensor(2, el_type)
    assert el_type.is_scalar
    assert x.type.tensor_rank == 2
    assert y.type.tensor_rank == 2
    assert isinstance(x.shape, TensorShape) and len(x.shape.dims) == 2
    assert isinstance(y.shape, TensorShape) and len(y.shape.dims) == 2

    m, k1 = x.shape.dims
    k2, n = y.shape.dims
    assert k1 == k2
    out_shape = TensorShape((m, n), ScalarShape)
    return ShapeType(out_shape, output_type)


def mat_vec_broadcast_prop_rule(m, v) -> ShapeType:
    el_type = unique_element_type([m.type, v.type])
    assert el_type.is_scalar
    assert m.type.tensor_rank == 2
    assert v.type.tensor_rank == 1
    assert isinstance(m.shape, TensorShape) and len(m.shape.dims) == 2
    assert isinstance(v.shape, TensorShape) and len(v.shape.dims) == 1

    assert m.shape.dims[1] == v.shape.dims[0]
    return ShapeType(m.shape, Type.Tensor(2, el_type))


dot = ksc.tracing.make_edef(
    "dot",
    ["x", "y"],
    mat_mat_mul_prop_rule,
    # the shape is ((m, n), el_shape) if x_shape = ((m, k), el_shape) and y_shape = ((k, n), el_shape)
    # And all el_shape are scalar atm
    lambda x, y: core.make_tuple(core.make_tuple(x.shape_program[0][0], y.shape_program[0][1]), y.shape_program[1]),
    # just count the number of flops
    lambda x, y: (lambda s_x, s_y: s_x[0] * s_x[1] * s_y[1] * 2)(x.shape_program[0], y.shape_program[0]),
)

broadcast_add = ksc.tracing.make_edef(
    "broadcast_add",
    ["m", "v"],
    mat_vec_broadcast_prop_rule,
    lambda m, v: m.shape_program,
    lambda m, v: core.numel_program(m),
)


def transpose_prop_rule(x):
    assert x.type.tensor_rank == 2

    def transpose_shape(s):
        assert len(s.dims) == 2
        m, n = s.dims
        return TensorShape((n, m), s.elem_shape)

    return ShapeType(transpose_shape(x.shape), x.type)


transpose = ksc.tracing.make_edef(
    "transpose",
    ["x"],
    transpose_prop_rule,
    lambda x: (lambda s: core.make_tuple(core.make_tuple(s[0][1], s[0][0]), s[1]))(x.shape_program),
    lambda x: Node.from_data(0),
)

reducemean = ksc.tracing.make_edef(
    "reducemean",
    ["x"],
    lambda x: ShapeType(ScalarShape, Type.Float),
    lambda x: Node.from_data(0.0).shape_program,
    lambda x: Node.from_data(0),
)
