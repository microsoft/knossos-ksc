from functools import reduce

import ksc
from ksc.type import Type
from ksc.tracing import node
from ksc.tracing.jitting import make_edef
from ksc.utils import ShapeType
from ksc.tracing.function import Trace, TraceableFunction
from ksc.tracing.functions.type_propagation_rules import (
    elementwise,
    first_arg,
    flatten_type_prop_rule,
    keep_shape_prop_rule
)

add = make_edef(
    "add", ["a", "b"], elementwise,
    lambda a, b: a.shape,
    lambda a, b: a.size
)

sub = make_edef(
    "sub", ["a", "b"], elementwise,
    lambda a, b: a.shape,
    lambda a, b: a.size
)

mul = make_edef(
    "mul", ["a", "b"], elementwise,
    lambda a, b: a.shape,
    lambda a, b: a.size * 2
)

div = make_edef(
    "div", ["a", "b"], elementwise,
    lambda a, b: a.shape,
    lambda a, b: a.size * 2
)

pow = make_edef(
    "pow", ["a", "b"], elementwise,
    lambda x, y: x.shape,
    lambda x, y: x.size * 10
)

flatten = make_edef(
    "flatten", ["x"], flatten_type_prop_rule,
    lambda x: (lambda s: make_tuple(s[0], reduce(mul, s[1:], node.Node.from_data(1))))(x.shape),
    lambda x: node.Node.from_data(0)
)

to_float = make_edef(
    "to_float", ["x"],
    keep_shape_prop_rule(Type.Float),
    lambda x: x.shape,
    lambda x: x.size
)

def make_builtin(name, arg_names, shape_prop_function):
    class Builtin(TraceableFunction):
        is_edef = False
        is_builtin = True
        def __init__(self):
            super().__init__(name, arg_names)
        def trace(self, *args):
            assert len(args) == len(arg_names)
            o_shape, o_type = shape_prop_function(*args)
            body = node.Node(
                name=self.name,
                shape=o_shape,
                type=o_type,
                children=args,
                shape_prop_function=shape_prop_function)
            shape_types = tuple(arg.shape_type for arg in args)
            return Trace(body, ShapeType(o_shape, o_type), shape_types)
    return Builtin()

def get_vector_element(index, x):
    def shape_prop_function(index, x):
        x_shape, x_type = x.shape_type
        assert x_type.kind == "Vec"
        return ShapeType(x_shape[1:], x_type.children[0])
    f = make_builtin("index", ["index", "x"], shape_prop_function)
    return f(index, x)

def get_vector_size(x):
    def shape_prop_function(x):
        _, x_type = x.shape_type
        assert x_type.kind == "Vec"
        return ShapeType((), Type.Integer)
    f = make_builtin("size", ["x"], shape_prop_function)
    return f(x)

def make_tuple(*args):
    def shape_prop_function(*args):
        shapes, types = zip(*[arg.shape_type for arg in args])
        return ShapeType(tuple(shapes), Type.Tuple(*types))
    arg_names = [f"arg{i}" for i in range(len(args))]
    f = make_builtin("tuple", arg_names, shape_prop_function)
    return f(*args)

def get_tuple_element(index, x):
    size = len(x)
    def shape_prop_function(arg):
        x_shape, x_type = arg.shape_type
        return ShapeType(x_shape[index], x_type.children[index])
    f = make_builtin(f"get${index+1}${size}", ["x"], shape_prop_function)
    return f(x)

def type_recursion_helper(fid, fadd, fmulpre, fmulpost, current):
    type = current._type
    if type.kind == "Vec":
        self_shape = get_vector_size(current)
        return fmulpre(self_shape, type_recursion_helper(fid, fadd, fmulpre, fmulpost, get_vector_element(0, current)))
    elif type.kind == "Tuple":
        children = [fmulpost(type_recursion_helper(fid, fadd, fmulpre, fmulpost, get_tuple_element(i, current))) for i in range(len(type.children))]
        return fadd(*children)
    else:
        return fid()
