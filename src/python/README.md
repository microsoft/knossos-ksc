# ksc python package

## Installing
In this directory
```
pip install --editable .
```

## Compiling ksc from Python

```python
from ksc import KsFunction
from ksc.type import Type

f = KsFunction("add3", Type.Integer, [("x", Type.Integer), ("y", Type.Integer), ("z", Type.Integer)], """
(edef add@ii Integer (Integer Integer))
(def add3 Integer ((x : Integer) (y : Integer) (z : Integer))
    (add@ii (add@ii x y) z)
)
""")
print(f(3, 4, 5, backend="cpp"))
```

It should print `12`.

## Using the Python frontend with the ksc cpp backend

```python
import ksc

@ksc.trace
def add3(x, y, z):
    return x + y + z

out = add3(3, 4, 5)
print(out.get_data_with_backend("cpp"))
```

## Using the Python frontend with the python backend

(optional) install Jax
```
pip install -U jax
```

```python
import ksc
import numpy as np
from ksc.tracing.functions import math

@ksc.trace
def dense(x, W, b):
    return math.broadcast_add(math.dot(x, W), b)

x = np.random.normal(0, 1, (16, 100))
W = np.random.normal(0, np.sqrt(1.0 / 100), (100, 100))
b = np.random.normal(0, 1, (100,))
out = dense(x, W, b)

print(out.data)
```

## Defining new edefs

A new edef can be defined using `make_edef` function from `ksc.tracing.jitting`.

```python
from ksc.tracing.jitting import make_edef

def check_type(a, b, c):
    """ checks consistency of the arguments and
        returns ShapeType of the output
    """
    assert a.shape_type == b.shape_type
    assert a.shape_type == c.shape_type
    return a.shape_type

add3 = make_edef(
    "add3",                     # name of the function
    ["a", "b", "c"],            # name of the arguments
    check_type,                 # see above
    lambda a, b, c: a.shape,    # shape of the output
    lambda a, b, c: a.size * 2  # cost of the computation
)
```

The last two arguments for `make_edef` will turn into `shape$add3` and `cost$add3` functions. To see this, let's call `add3` with concrete inputs.

First, let's try

```python
out_s = add3(1.0, 2.0, 3.0)
print(out_s.data)
```

This will result in error `ImportError: cannot import name 'add3' from 'ksc.backends.jax'` but this is expected because we just declared a new function without an implementation.

The following ks string must have been printed out before the error

```lisp
(edef mul@ii Integer (Integer Integer))

(def shape$mul@ii Integer ((a : Integer) (b : Integer))
  0
)

(def cost$mul@ii Integer ((a : Integer) (b : Integer))
  (mul@ii 1 2)
)

(edef add3@fff Float (Float Float Float))

(def shape$add3@fff Integer ((a : Float) (b : Float) (c : Float))
  0
)

(def cost$add3@fff Integer ((a : Float) (b : Float) (c : Float))
  (mul@ii 1 2)
)
```

You can find `shape$add3@fff` and `cost$add3@fff` for `add3@fff`, which are all specialized for the input type of three floats. You also see the edef of `mul@ii` function and its `shape$` and `cost$` functions, which are needed for computing the cost of `add3@fff`.

Next, let's try

```python
import numpy as np
a = np.random.normal(0, 1, (3, 4))
b = np.random.normal(0, 1, (3, 4))
c = np.random.normal(0, 1, (3, 4))
out_m = add3(a, b, c)
print(out_m.data)
```

We will get another `ImportError` but that's fine. What is interesting this time is that the same edef now generates the following `cost$` and `shape$` functions


```lisp
(def shape$add3@vvfvvfvvf (Tuple Integer Integer) ((a : (Tensor 2 Float)) (b : (Tensor 2 Float)) (c : (Tensor 2 Float)))
  (let ((v0 (index 0 a)))
  (tuple (size a) (size v0))
  )
)

(def cost$add3@vvfvvfvvf Integer ((a : (Tensor 2 Float)) (b : (Tensor 2 Float)) (c : (Tensor 2 Float)))
  (let ((v0 (index 0 a)))
  (mul@ii (mul@ii (size a) (mul@ii (size v0) 1)) 2)
  )
)
```

These two functions are written in Knossos IR and informs any downstream process about the shape of the output of function `add3@vvfvvfvvf` and its cost.

## Abstract backend

The new edef function cannot be called because we haven't defined its implementation. However we can evaluate its cost because we have the `cost$` function. This can be achieved by using the `abstract` backend.

```python
from ksc.abstract_value import ExecutionContext
with ExecutionContext() as ctx:
    out_m.get_data_with_backend("abstract")
print(ctx.costs)
```

It should print `defaultdict(<class 'float'>, {None: 24.0})`, which makes sense because we have added 12 numbers two times.

To see that the cost is based on the actual shape of the input to `add3`, let's try

```python
a = np.random.normal(0, 1, (3, 7))
b = np.random.normal(0, 1, (3, 7))
c = np.random.normal(0, 1, (3, 7))
with ExecutionContext() as ctx:
    add3(a, b, c).get_data_with_backend("abstract")

print(ctx.costs)
```

It should print `defaultdict(<class 'float'>, {None: 42.0})`.

## Hints for writing `cost$` and `shape$` functions

`cost$` and `shape$` functions are ordinary python functions that take the same arguments as the edef'ed function. They are implicitly decorated by `@ksc.trace`. So you can assume that the inputs are instances of `Node` objects and you need to make sure to return `Node` instances. For example, you can call `shape` and `size` member functions and they will return instances of `Node` objects. But calling `shape_type` function is a bad idea because it returns a `ShapeType`.

Here are more concrete hints:

1. `cost$` and `shape$` functions are optional. If they are missing, you won't be able to compute the cost of your edef.
2. Zero cost needs to be expressed as `Node.from_data(0)`.
3. Scalar shape needs to be expressed as integer 0. See `shape$add3@fff` above. This is because Knossos doesn't allow empty tuple.
4. `input.shape` can be used in a `cost$` function. See edef for `dot` in [math.py](ksc/tracing/functions/math.py). If `input` is a n-nested vector, `input.shape` returns a n-ary tuple. If `input` is a tuple of vectors, it returns a tuple of tuple.
5. For the most elaborate example, see `cost_conv_2d_no_bias` in [nn.py](ksc/tracing/functions/nn.py).

Examples of `cost$` and `shape$` functions can be [core.py](ksc/tracing/functions/core.py), [math.py](ksc/tracing/functions/math.py), and [nn.py](ksc/tracing/functions/nn.py) files under `ksc/tracing/functions`.
