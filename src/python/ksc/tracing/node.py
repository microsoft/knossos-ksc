import hashlib

import ksc
from ksc.type import Type
from ksc.abstract_value import AbstractValue
from ksc.tracing import function
from ksc.tracing import jitting
from ksc.shape import shape_type_from_object


class Node(AbstractValue):
    def __init__(self, name, shape=None, type=None, data=None, children=[], jitted=None, shape_prop_function=None):
        super().__init__(shape, type, data)
        self._name = name
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
        st = shape_type_from_object(value)
        return Node("", st.shape, st.type, data=value)

    def add_user(self, node):
        self._users.append(node)

    @property
    def data(self):
        return self.get_data_with_backend("jax")

    def get_data_with_backend(self, backend):
        if self._data is not None and not self.data_ready:
            value_node = jitting.jit_and_execute_anonymous_function(self.creator, backend)
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
        assert self._type.is_tuple, f"Tried to call an iterator on a non-tuple {self}"
        return self._type.tuple_len

    def __iter__(self):
        assert self._type.is_tuple, f"Tried to call an iterator on a non-tuple {self}"
        from ksc.tracing.functions import core
        return (core.get_tuple_element(i, self) for i in range(self._type.tuple_len))

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
        if self._type.is_tuple:
            if isinstance(index, slice):
                return (core.get_tuple_element(i, self) for i in range(*index.indices(len(self._type.children))))
            return core.get_tuple_element(index, self)
        
        if self._type.is_tensor:
            if isinstance(index, slice):
                raise NotImplementedError
            return core.get_tensor_element(index, self)
        
        raise ValueError(f"Tried to call __getitem__ on {self} which is not a Tuple.  Use index for tensors.")

    @property
    def shape_program(self):
        """ Note: in contrast to shape_type, this function returns a program to compute the shape

        From make_edef:
        ; shape algebra is as follows:
        ;    shape of scalar is ()
        ;    shape of tensor is (tuple (tuple dim1 dim2 ... dimN) element_shape)
        ;    shape of tuple is (tuple shape1 shape2 .. shapeN)

        """
        from ksc.tracing.functions import core
        if self.type.is_scalar:
            return core.make_tuple()

        if self.type.is_tensor:

            dims = core.get_tensor_size(self) # ks::size
            # TOUNDO: ks::size returns an int for rank 1,need to tuple it
            if self.type.tensor_rank == 1:
                dims = core.make_tuple(dims)

            element = core.get_tensor_element0(self) # ks::index
            return core.make_tuple(dims, element.shape_program)
            
        if self.type.is_tuple:
            children = (core.get_tuple_element(i, self).shape_program for i in range(self.type.tuple_len))
            return core.make_tuple(*children)

        raise NotImplementedError

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
