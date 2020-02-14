import hashlib

import ksc
from ksc.type import Type
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

    def __radd__(self, other):
        return self.__add__(other)

    def __sub__(self, other):
        from ksc.tracing.functions import core
        return core.sub(self, Node.from_data(other))

    def __mul__(self, other):
        from ksc.tracing.functions import core
        return core.mul(self, Node.from_data(other))

    def __rmul__(self, other):
        return self.__mul__(other)

    def __floordiv__(self, other):
        from ksc.tracing.functions import core
        assert self.shape_type.type == Type.Integer
        if isinstance(other, Node):
            assert other.shape_type.type == Type.Integer
        return core.div(self, Node.from_data(other))

    def __truediv__(self, other):
        from ksc.tracing.functions import core
        return core.div(self, Node.from_data(other))

    def __pow__(self, other):
        from ksc.tracing.functions import core
        return core.pow(self, Node.from_data(other))

    def __getitem__(self, index):
        from ksc.tracing.functions import core
        if self._type.kind == "Tuple":
            if isinstance(index, slice):
                return (core.get_tuple_element(i, self) for i in range(*index.indices(len(self._type.children))))
            return core.get_tuple_element(index, self)
        elif self._type.kind == "Vec":
            return core.get_vector_element(index, self)
        else:
            raise ValueError(f"Tried to call __getitem__ on {self} which is not Tuple or Vec.")

    @property
    def shape(self):
        """ Note: in contrast to shape_type, this function returns a program to compute the shape
        """
        from ksc.tracing.functions import core
        if self._type.kind not in ["Vec", "Tuple"]:
            # because 0-tuple is not allowed in ks, we return an integer constant
            return Node.from_data(0)
        shape = core.type_recursion_helper(
            tuple,
            core.make_tuple,
            lambda a, b: (a,) + b,
            lambda x: core.make_tuple(*x) if isinstance(x, tuple) else x,
            self
        )
        print(f"In shape: shape={shape}")
        if isinstance(shape, tuple):
            shape = core.make_tuple(*shape)
        return shape

    @property
    def size(self):
        """ returns a program to compute the number of elements
        """
        from ksc.tracing.functions import core
        return core.type_recursion_helper(
            lambda : Node.from_data(1),
            sum,
            core.mul,
            lambda x: x,
            self
        )

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
