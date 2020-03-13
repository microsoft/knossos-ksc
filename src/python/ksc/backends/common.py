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

def to_float_i(a):
    return float(a)

def build(n, f):
    return [f(i) for i in range(n)]

def sumbuild(n, f):
    return sum(f(i) for i in range(n))

def index(i, v):
    return v[i]

def size(v):
    return len(v)

def fold(f, s0, xs):
    s = s0
    for x in xs:
        s = f((s, x))
    return s

def make_tuple(*args):
    return tuple(args)

def get_tuple_element(i, tup):
    return tup[i]

def let(var, body):
    return body(var)

def if_then_else(cond, then_branch, else_branch):
    return then_branch() if cond else else_branch()

_built_ins = [
    "build",
    "sumbuild",
    "size",
    "index",
    "fold",
    "make_tuple",
    "get_tuple_element",
    "let",
    "if_then_else"
]
