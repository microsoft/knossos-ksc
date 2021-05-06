import pytest
import ksc

import numpy as np

from ksc.tracing.node import Node
from ksc.tracing.functions import core
import ksc.tracing.functions.math as M
from ksc.type import Type
from ksc.shape import Shape, TensorShape, ScalarShape


def test_transpose():
    x = np.random.normal(0, 1, (3, 7))
    out = M.transpose(x)
    assert out.shape_type.shape.dims == (7, 3)
    assert np.ma.allequal(out.data, x.T)
    # creator of out is an anonymous function. So the shape$
    # function of flatten must be in before.
    before, _ = out.creator._jitted.all_called_functions()
    shape_def = next(f for key, f in before.items() if key[0] == "shape$transpose")
    assert shape_def(x) == ((7, 3), ())
