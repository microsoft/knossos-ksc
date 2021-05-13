import numpy as np
import pytest

import ksc
from ksc.tracing.functions import math
from ksc.abstract_value import AbstractValue, ExecutionContext
from ksc.type import Type
from ksc.shape import shape_type_from_object, ShapeType, TensorShape
from ksc.utils import translate_and_import
from ksc.tracing.functions import core


def test_get_shape():
    ks_str = """
(def get_shape (Tuple Integer Integer Integer Integer) (x : (Tensor 4 Float))
   (size x))
"""
    x = AbstractValue(TensorShape((1, 3, 224, 224), ()), Type.Tensor(4, Type.Float))
    m = translate_and_import(__file__, ks_str, "abstract")
    with ExecutionContext():
        assert m.get_shape(x).data == (1, 3, 224, 224)


def test_get_tuple_element():
    ks_str = """
(def flatten (Tuple Integer Integer Integer) ((x : (Tuple (Tuple Integer Integer) Integer)))
  (tuple (get$1$2 (get$1$2 x)) (get$2$2 (get$1$2 x)) (get$2$2 x))
)
"""
    d = AbstractValue.from_data(((1, 2,), 3))
    m = translate_and_import(__file__, ks_str, "abstract")
    with ExecutionContext():
        assert m.flatten(d).data == (1, 2, 3)

    x = AbstractValue.from_data(((3, 4), 5))
    with ExecutionContext():
        assert m.flatten(x).data == (3, 4, 5)


def test_tuple_of_tensors():
    ks_str = """
(def fst (Tensor 2 Float) ((x : (Tuple (Tensor 2 Float) (Vec Float))))
    (get$1$2 x)
)
(def snd (Tensor 2 Float) ((x : (Tuple (Tensor 2 Float) (Vec Float))))
    (get$2$2 x)
)
"""
    m = translate_and_import(__file__, ks_str, "abstract")
    x = AbstractValue.from_data(
        (np.random.normal(0, 1, (3, 4)), np.random.normal(0, 1, (4,)))
    )
    with ExecutionContext():
        assert m.fst(x).shape_type == ShapeType(
            TensorShape((3, 4), ()), Type.Tensor(2, Type.Float)
        )  # (((3, 4),), Type.Tensor(2, Type.Float))
        assert m.snd(x).shape_type == ShapeType(
            TensorShape((4,), ()), Type.Tensor(1, Type.Float)
        )


def test_if_then_else():
    ks_str = """
(edef lt Bool (Tuple Float Float))
(def cost$lt Float ((a : Float) (b : Float)) 1.0)
(def shape$lt Integer ((a : Float) (b : Float)) (tuple))
(edef eq Bool (Tuple Float Float))
(def cost$eq Float ((a : Float) (b : Float)) 1.0)
(def shape$eq Integer ((a : Float) (b : Float)) (tuple))
(def sign Float ((x : Float))
    (if (lt x 0) -1.0 (if (eq x 0) 0.0 1.0))
)
"""
    m = translate_and_import(__file__, ks_str, "abstract")
    with ExecutionContext():
        assert m.sign(2.0) == 1.0
        assert m.sign(-5.0) == -1.0
        assert m.sign(0) == 0.0
        assert m.sign(AbstractValue((), Type.Float)).shape_type == ShapeType(
            (), Type.Float
        )
