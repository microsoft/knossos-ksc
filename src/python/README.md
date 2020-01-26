# ksc python package

## Installing
In this directory
```
pip install -U .
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
