import hashlib

import ksc
from ksc.tracing import function
from ksc.tracing import jitting
from ksc import utils
from ksc.utils import ShapeType

class Node:
    def __init__(self, name, shape=None, type=None, data=None, children=[], jitted=None, shape_prop_function=None):
        self._name = name
        # only for Value
        self._data = data
        # shape and type of the value (filled in through tracing)
        self._shape = shape
        self._type = type
        self._children = children
        self._jitted = jitted
        self._shape_prop_function = shape_prop_function
        if jitted is not None:
            jitted.origin = self
        self._users = []

    @staticmethod
    def from_data(value):
        if isinstance(value, Node):
            return value
        shape, type = utils.shape_type_from_object(value)
        return Node("", shape, type, data=value)

    def add_user(self, node):
        self._users.append(node)

    @property
    def data(self):
        return self.get_data_with_backend("jax")

    def get_data_with_backend(self, backend):
        if self._data is not None and not self.data_ready:
            value_node = jitting.jit_and_execute_annonymous_function(self.creator, backend)
            self._children = value_node.children
            self._data = value_node.data
        return self._data

    @property
    def data_ready(self):
        return (self._data is not None
                and (not isinstance(self._data, str)
                     or self._data != "__not_ready__"))

    @property
    def jitted(self):
        return self._jitted

    @property
    def name(self):
        return self._name

    @property
    def users(self):
        return self._users

    @property
    def shape_type(self):
        return ShapeType(self._shape, self._type)

    @property
    def children(self):
        return self._children

    @property
    def shape_prop_function(self):
        if self._shape_prop_function is not None:
            # edef
            return self._shape_prop_function
        else:
            return self._jitted._shape_prop_function

    @property
    def creator(self):
        assert self._data is not None, "Called creator on a non-value node"
        return self._children[0]

    def __len__(self):
        assert self._type.kind == "Tuple", f"Tried to call an iterator on a non-tuple {self}"
        return len(self._type)

    def __iter__(self):
        assert self._type.kind == "Tuple", f"Tried to call an iterator on a non-tuple {self}"
        from ksc.tracing.functions import core
        return (core.get_tuple_element(i, self) for i in range(len(self._type)))

    def __add__(self, other):
        from ksc.tracing.functions import core
        return core.add(self, Node.from_data(other))

    def __sub__(self, other):
        from ksc.tracing.functions import core
        return core.sub(self, Node.from_data(other))

    def __mul__(self, other):
        from ksc.tracing.functions import core
        return core.mul(self, Node.from_data(other))

    def __truediv__(self, other):
        from ksc.tracing.functions import core
        return core.div(self, Node.from_data(other))

    def __getitem__(self, index):
        assert self._type.kind == "Vec", f"Tried to call index on a non-vec {self}"
        from ksc.tracing.functions import core
        return core.get_vector_element(index, self)

    @property
    def shape(self):
        from ksc.tracing.functions import core
        def helper(current, type):
            if type.kind == "Vec":
                self_shape = (core.get_vector_size(current),)
                return self_shape + helper(core.get_vector_element(0, current), type.children[0])
            elif type.kind == "Tuple":
                child_shapes = [helper(core.get_tuple_element(i, current), ct) for i, ct in enumerate(type.children)]
                # make sure that child_shapes is a list of nodes
                # rather than list of tuple of nodes
                child_shapes = [core.make_tuple(*cs) if isinstance(cs, tuple) else cs for cs in child_shapes]
                print(f"In shape: child_shapes={child_shapes}")
                return core.make_tuple(*child_shapes)
            else:
                return ()
        shape = helper(self, self.shape_type.type)
        print(f"In shape: shape={shape}")
        if isinstance(shape, tuple):
            shape = core.make_tuple(*shape)
        return shape

    def __repr__(self):
        if self._data is not None:
            if len(self.children) == 0:
                # constant
                return f"Value(data={self._data})"
            else:
                return f"Value(data={self._data}, creator={self.creator.name})"
        elif len(self._children) > 0:
            return f"Function(name={self._name}, shape={self._shape}, type={self._type}, children={[c.name for c in self._children]})"
        return f"Node(name={self._name}, shape={self._shape}, type={self._type}, children={[c.name for c in self._children]})"
