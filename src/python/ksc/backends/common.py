import numpy as np

from ksc.utils import ndgrid_inds

def add(a, b):
    return a + b

def sub(a, b):
    return a - b

def mul(a, b):
    return a * b

def div_ii(a, b):
    return a // b

def div_ff(a, b):
    return a / b

def div(a, b):
    if isinstance(a, int):
        return a // b
    else:
        return a / b

def eq(a, b):
    return a == b

def lt(a, b):
    return a < b

def gt(a, b):
    return a > b

def lte(a, b):
    return a <= b

def gte(a, b):
    return a >= b

def or_(a, b):
    return a or b

def and_(a, b):
    return a and b

def abs_(a):
    return abs(a)

def max_(a, b):
    return max(a, b)

def neg(a):
    return -a

def pow(a, b):
    return a ** b

def log(a):
    return np.log(a)

def to_float_i(a):
    return float(a)

def build(sz, f):
    # Integer sz will build a 1D tensor, as a list
    if isinstance(sz, int):
        return [f(i) for i in range(sz)]

    # Tuple sz will build a n-D tensor, as a ndarray, so leaf type needs to be scalar
    assert isinstance(sz, tuple)
    return np.reshape([f(ind) for ind in ndgrid_inds(sz)], sz)

def sumbuild(n, f):
    return sum(f(i) for i in range(n))

def index(i, v):
    return v[i]

def size(v):
    if isinstance(v, list):
        return len(v)

    #TOUNDO: size(Tensor 1 T) returns int
    dims = v.shape
    if len(dims) == 1:
        return dims[0]
    else:
        return dims

def fold(f, s0, xs):
    s = s0
    for x in xs:
        s = f((s, x))
    return s

def make_tuple(*args):
    return tuple(args)

def get_tuple_element(i, tup):
    return tup[i]

def let(tupled, var, body):
    if tupled:
        return body(*var)
    else:
        return body(var)

def if_then_else(cond, then_branch, else_branch):
    return then_branch() if cond else else_branch()

def assert_(cond, body):
    assert cond
    return body

_core_built_ins = [
    "build",
    "sumbuild",
    "size",
    "index",
    "fold",
    "make_tuple",
    "get_tuple_element",
    "let",
    "if_then_else",
    "assert_",
]

_built_ins = _core_built_ins + [
    "add",
    "sub",
    "mul",
    "div_ii",
    "div_ff",
    "div",
    "eq",
    "lt",
    "gt",
    "lte",
    "gte",
    "or_",
    "and_",
    "abs_",
    "max_",
    "neg",
    "pow",
]
