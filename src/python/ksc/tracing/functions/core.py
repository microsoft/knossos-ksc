from functools import reduce

import ksc
from ksc.type import Type, SizeType
from ksc.shape import ShapeType, Shape, ScalarShape

from ksc.tracing import node
from ksc.tracing.node import Node
from ksc.tracing.jitting import make_edef
from ksc.tracing.function import Trace, TraceableFunction
from ksc.tracing.functions.type_propagation_rules import (
    elementwise,
    elementwise_or_scalar,
    first_arg,
    flatten_type_prop_rule,
    keep_shape_prop_rule,
)


def numel_program(node):
    """ 
    Returns a program to compute the number of elements

    Algebra is:
        numel(Scalar) = 1
        numel(Tuple vs) = sum(numel(v) for v in vs)
        numel(Tensor dims v) = prod(dims(v)) * numel(v[0,...,0])

    """
    if node.type.is_scalar:
        return Node.from_data(1)

    if node.type.is_tuple:
        return sum(numel_program(get_tuple_element(i, node)) for i in range(node.type.tuple_len))

    if node.type.is_tensor:
        # TODO is assuming scalar elts
        return reduce(mul, node.shape_program[0]) * numel_program(get_tensor_element0(node))

    raise NotImplementedError


add = make_edef("add", ["a", "b"], elementwise_or_scalar, lambda a, b: a.shape_program, lambda a, b: numel_program(a))

sub = make_edef("sub", ["a", "b"], elementwise_or_scalar, lambda a, b: a.shape_program, lambda a, b: numel_program(a))

neg = make_edef("neg", ["x"], first_arg, lambda x: x.shape_program, lambda x: numel_program(x))

mul = make_edef(
    "mul", ["a", "b"], elementwise_or_scalar, lambda a, b: a.shape_program, lambda a, b: numel_program(a) * 2
)

div = make_edef(
    "div", ["a", "b"], elementwise_or_scalar, lambda a, b: a.shape_program, lambda a, b: numel_program(a) * 2
)

pow = make_edef("pow", ["a", "b"], elementwise)

log = make_edef("log", ["a"], first_arg, lambda x, y: x.shape_program, lambda x, y: numel_program(x) * 10)

flatten = make_edef(
    "flatten",
    ["x"],
    flatten_type_prop_rule,
    # shape$flatten of x is ((s0, srest...), el_shape), result is ((s0, reduce(mul, srest...)), el_shape)
    lambda x: make_tuple(
        (lambda s: make_tuple(s[0], reduce(mul, s[1:], node.Node.from_data(1))))(x.shape_program[0]), x.shape_program[1]
    ),
    # cost$flatten
    lambda x: node.Node.from_data(0),
)

to_float = make_edef("to_float", ["x"], keep_shape_prop_rule(Type.Float))


def make_builtin(name, arg_names, shape_prop_function):
    class Builtin(TraceableFunction):
        is_edef = False
        is_builtin = True

        def __init__(self):
            super().__init__(name, arg_names)

        def trace(self, *args):
            assert len(args) == len(arg_names)
            ost = shape_prop_function(*args)
            body = node.Node(
                name=self.name, shape=ost.shape, type=ost.type, children=args, shape_prop_function=shape_prop_function
            )
            shape_types = tuple(arg.shape_type for arg in args)
            return Trace(body, ShapeType(ost.shape, ost.type), shape_types)

    return Builtin()


# ks::index
def get_tensor_element(index, x):
    def shape_prop_function(index, x):
        xst = x.shape_type
        assert xst.type.is_tensor
        return ShapeType(xst.shape.elem_shape, xst.type.tensor_elem_type)

    f = make_builtin("index", ["index", "x"], shape_prop_function)
    return f(index, x)


def get_tensor_element0(x):
    # Get the (0,...,0) element of a tensor
    assert x.type.is_tensor
    dims = x.shape.dims
    if len(dims) == 1:
        return get_tensor_element(0, x)
    else:
        return get_tensor_element(tuple(0 for _ in dims), x)


# ks::size
def get_tensor_size(x):
    def shape_prop_function(x: Node):
        x_type = x.type
        assert x_type.is_tensor
        # Even if tensor is of compound type, the *size* is just an ntuple of ints
        shape_of_result = Shape.of_Index_of_Tensor_of_rank(x_type.tensor_rank)
        return ShapeType(shape_of_result, SizeType.from_rank(x_type.tensor_rank))

    f = make_builtin("size", ["x"], shape_prop_function)
    return f(x)


# "tuple"
def make_tuple(*args):
    def shape_prop_function(*args):
        shapes = tuple(arg.shape for arg in args)
        types = tuple(arg.type for arg in args)
        return ShapeType(shapes, Type.Tuple(*types))

    arg_names = [f"arg{i}" for i in range(len(args))]
    f = make_builtin("tuple", arg_names, shape_prop_function)
    return f(*args)


# "get$1$2"
def get_tuple_element(index, x):
    tuple_size = len(x)

    def shape_prop_function(arg):
        return ShapeType(arg.shape[index], arg.type.tuple_elem(index))

    f = make_builtin(f"get${index+1}${tuple_size}", ["x"], shape_prop_function)
    return f(x)
