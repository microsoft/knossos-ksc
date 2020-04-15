import numpy as np
from ksc.type import Type
from ksc.utils import ShapeType


def unique_element(s):
    assert len(s) == 1
    return next(iter(s))

def unique_element_type(types):
    el_type = unique_element(types[0].all_element_types())
    for t in types:
        assert el_type == unique_element(t.all_element_types())
    return el_type

def elementwise(*args):
    a = args[0]
    for arg in args:
        assert a.shape_type == arg.shape_type, f"Expected {a.shape_type}, but got {arg.shape_type}"
    return args[0].shape_type

def elementwise_or_scalar(*args):
    '''Type propagation rule for operations like elementwise add/multiply,
    that allows arguments to be scalar. Rules:
    - All vectors in `args` must have the same shape and element type.
    - Any scalars in `args` must have the same type, and this type must
        match the element type of the vectors, if any.
    Example ShapeTypes and return ShapeType:
        ((), Float) ((2, 3), Vec(Vec(Float))) ((), Float)
            -> ((2, 3), Vec(Vec(Float)))
        ((2, 3), Vec(Vec(Float))) ((), Float) ((2, 3), Vec(Vec(Float)))
            -> ((2, 3), Vec(Vec(Float)))
        ((), Float) ((2, 3), Vec(Vec(Float))) ((), Int)
            -> ValueError
        ((2, 3), Vec(Vec(Float))) ((), Float) ((2, 3), Vec(Vec(Int)))
            -> ValueError
        ((3, 2), Vec(Vec(Float))) ((), Float) ((2, 3), Vec(Vec(Float)))
            -> ValueError
    '''
    def _get_type(x):
        '''Gets type if scalar, and element type if vector.'''
        if x.kind == 'Vec':
            return _get_type(x.children[0])
        return x
    assert len(args) > 0
    # All types must be equal:
    types = set(_get_type(a.shape_type.type) for a in args)
    if len(types) > 1:
        raise ValueError(f'Arguments have unsupported types: {types}')
    # If there are any non-scalars, then their shapes must be equal:
    shapes = set(a.shape_type.shape for a in args if a.shape_type.shape != ())
    if len(shapes) > 1:
        raise ValueError(f'Arguments have incompatible shapes: {shapes}')
    # Return shape is the shape of the vector, if there is one:
    if len(shapes) > 0:
        return ShapeType(shapes.pop(), types.pop())
    return ShapeType((), types.pop())

def first_arg(*args):
    return args[0].shape_type

def flatten_type_prop_rule(x):
    x_shape, x_type = x.shape_type
    el_type = unique_element_type([x_type])
    out_shape = x_shape[:1] + (np.prod(x_shape[1:]),)
    out_type = Type.Vec(Type.Vec(el_type))
    return ShapeType(out_shape, out_type)

def keep_shape_prop_rule(new_type):
    def type_prop_rule(x):
        x_shape, x_type = x.shape_type
        def to_new_type(type):
            if type.kind == "Tuple":
                return Type.Tuple(*[to_new_type(c) for c in type.children])
            elif type.kind == "Vec":
                return Type.Vec(to_new_type(type.children[0]))
            else:
                return new_type
        return ShapeType(x_shape, to_new_type(x_type))
    return type_prop_rule

def conv_2d_type_prop_rule_from_padding_type(padding):
    def type_prop_rule(x, weights, ksizes, strides):
        stride_w, stride_h = strides.data
        if weights.shape_type.type.kind == "Tuple":
            # has bias
            w_shape = weights.shape_type.shape[0]
        else:
            w_shape = weights.shape_type.shape

        k_w, k_h = ksizes.data

        x_shape, x_type = x.shape_type
        b, c1, w, h = x_shape
        m, c2, k_w_, k_h_ = w_shape
        assert (k_w, k_h) == (k_w_, k_h_), f"Expected kernel size {(k_w, k_h)}, but got {(k_w_, k_h_)}"
        assert c1 == c2, f"Expected {c2} input channels, but got {c1}"
        y_w = _get_output_length(w, k_w, stride_w, padding)
        y_h = _get_output_length(h, k_h, stride_h, padding)
        out_shape = (b, m, y_w, y_h)
        return ShapeType(out_shape, x_type)
    return type_prop_rule

def pooling_type_prop_rule(padding):
    def type_prop_rule(x, pool_size, strides):
        pool_w, pool_h = pool_size.data
        stride_w, stride_h = strides.data
        x_shape, x_type = x.shape_type
        b, c, w, h = x_shape

        y_w = _get_output_length(w, pool_w, stride_w, padding)
        y_h = _get_output_length(h, pool_h, stride_h, padding)

        out_shape = (b, c, y_w, y_h)
        return ShapeType(out_shape, x_type)
    return type_prop_rule

def _ceil_div(x, y):
    return (x + y - 1) // y

def _get_output_length(x_len, window_size, stride, padding):
    if padding.upper() == "VALID":
        y_len = x_len - window_size + 1
    elif padding.upper() == "SAME":
        y_len = x_len
    else:
        raise ValueError(f"Unknown padding option {padding}")
    return _ceil_div(y_len, stride)
