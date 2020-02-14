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

add = make_edef("add", ["a", "b"], elementwise)

sub = make_edef("sub", ["a", "b"], elementwise)

mul = make_edef("mul", ["a", "b"], elementwise)

div = make_edef("div", ["a", "b"], elementwise)

flatten = make_edef("flatten", ["x"], flatten_type_prop_rule)

to_float = make_edef("to_float", ["x"], keep_shape_prop_rule(Type.Float))

max = make_edef("max", ["x", "y"], elementwise)

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
