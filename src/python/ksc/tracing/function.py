from collections import namedtuple

from ksc.tracing import jitting
from ksc.tracing import node
from ksc.shape import shape_type_from_object

Trace = namedtuple("Trace", ["body", "shape_type", "arg_shape_types"])


class TraceableFunction:
    def __init__(self, name, arg_names=None):
        self._name = name
        self._arg_names = arg_names

    @property
    def name(self):
        return self._name

    @property
    def arg_names(self):
        return self._arg_names

    def trace(self, *args):
        body = self.forward(*args)
        shape_types = tuple(arg.shape_type for arg in args)
        return Trace(body, body.shape_type, shape_types)

    def forward(self, *args):
        raise NotImplementedError

    def __call__(self, *args):
        print(f"Handling call to {self.name}")
        converted_args = []
        for i, arg in enumerate(args):
            if isinstance(arg, node.Node):
                if arg._data is not None and not arg.data_ready:
                    converted_args.append(arg.creator)
                else:
                    converted_args.append(arg)
            else:
                st = shape_type_from_object(arg)
                converted_args.append(node.Node("", st.shape, st.type, data=arg))
            print(f"Processing the {i+1}th argument to {self.name} with {converted_args[-1].shape_type}")
        jitted = jitting.get_or_trace_function(self, converted_args)
        st = jitted.shape_type(*converted_args)  # call type_prop_function
        print(f"Shape and type of {jitted.name} is {st}")
        func_node = node.Node(jitted.name, st.shape, st.type, children=converted_args, jitted=jitted)
        value_node = node.Node("_identity", st.shape, st.type, children=[func_node], data="__not_ready__")
        for arg in converted_args:
            arg.add_user(func_node)
        return value_node

    def __repr__(self):
        return f"TraceableFunction(name={self._name}, args={self._arg_names})"
