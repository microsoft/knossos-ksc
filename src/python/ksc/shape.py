from dataclasses import dataclass
from typing import Tuple, Union
import numpy as np

from ksc.type import Type

# make_dims(1) = (1,)
# make_dims([1, 2, 3]) = (1,2,3)
# make_dims((1,2,3)) = (1,2,3)
def make_dims(val) -> Tuple[int]:
    if isinstance(val, int):
        return (val,)

    if isinstance(val, tuple):
        assert (isinstance(v, int) for v in val)
        return val

    raise NotImplementedError("make_dims")

    if isinstance(val, list):
        return tuple(*val)

# Shape of a scalar is just just empty tuple
ScalarShape = ()

# Shape class hierarchy
class Shape:
    """
    Shape classes.  
    Shapes in the abstract interpreter follow the algebra described in "make_edef"
    
    """
    pass

    @staticmethod
    def from_ks_shape(val, type):
        """
        Translate from ks_value as returned by shape_def to Shape class
        """
        if type.is_scalar:
            assert val == ()
            return ScalarShape

        if type.is_tensor:
            dims, el_shape = val
            return TensorShape(dims, Shape.from_ks_shape(el_shape, type.tensor_elem_type))

        if type.is_tuple:
            assert isinstance(val, tuple)
            assert len(val) == type.tuple_len
            return tuple(Shape.from_ks_shape(val[i], type.tuple_elem(i)) for i in range(len(val)))

        assert False

    @staticmethod
    def of_Index_of_Tensor_of_rank(rank : int):
        """
        Make the shape of the return of the ks builtin "size" function, i.e. a Tuple of ScalarShapes
        """
        if rank == 1: 
            return ScalarShape # Returns an int
        else:
            return tuple(ScalarShape for _ in range(rank)) 

@dataclass(frozen=True)
class TensorShape(Shape):
    dims : Tuple[int]
    elem_shape : Shape

def make_TensorShape(dims: Union[int, Tuple[int]], elem_shape : Shape):
    return TensorShape(make_dims(dims), elem_shape)

@dataclass(frozen=True)
class ShapeType:
    shape: Shape
    type: Type

def shape_type_matches(s : Shape, t : Type):
    if t.is_tensor:
        return isinstance(s, TensorShape) and len(s.dims) == t.tensor_rank \
            and shape_type_matches(s.elem_shape, t.tensor_elem_type)

    if t.is_tuple:
        return isinstance(s, tuple) and all(shape_type_matches(sh, ty) for sh,ty in zip(s, t.tuple_elems()))

    if t.is_scalar:
        return s == ScalarShape

    raise NotImplementedError

def shape_type_from_object(o):
    # import here to avoid circular dependencies
    from ksc.abstract_value import AbstractValue
    if hasattr(o, "shape") and hasattr(o, "dtype"):
        el = shape_type_from_object(np.asarray(o).flat[0].item())
        return ShapeType(TensorShape(o.shape, el.shape), Type.Tensor(o.ndim, el.type))
    elif isinstance(o, AbstractValue):
        return o.shape_type
    elif hasattr(o, "data") and o.data is not None:
        # value node
        return shape_type_from_object(o.data)
    elif isinstance(o, list):
        el = shape_type_from_object(o[0])
        assert all(shape_type_from_object(e) == el for e in o)
        dims = make_dims(len(o))
        return ShapeType(TensorShape(dims, el.shape), Type.Tensor(1, el.type))
    elif isinstance(o, tuple):
        els = tuple(shape_type_from_object(e) for e in o)
        tup_shape = tuple(e.shape for e in els)
        tup_type = Type.Tuple(*(e.type for e in els))
        return ShapeType(tup_shape, tup_type)
    elif isinstance(o, bool):
        return ShapeType((), Type.Bool)
    elif isinstance(o, int):
        return ShapeType((), Type.Integer)
    elif isinstance(o, float):
        return ShapeType((), Type.Float)
    else:
        raise ValueError(f"Cannot handle object {o}")
