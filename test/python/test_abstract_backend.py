import numpy as np
import pytest

import ksc
from ksc.tracing.functions import math
from ksc.abstract_value import AbstractValue, ExecutionContext
from ksc.type import Type
from ksc.utils import translate_and_import, shape_type_from_object
from ksc.tracing.functions import core

def test_get_shape():
    ks_str = """
(def get_shape@vvvvf (Tuple Integer Integer Integer Integer) ((x : (Vec (Vec (Vec (Vec Float))))))
  (let ((v0 (index@ivvvvf 0 x)))
  (let ((v1 (index@ivvvf 0 v0)))
    (let ((v2 (index@ivvf 0 v1)))
      (tuple@iiii (size@vvvvf x) (size@vvvf v0) (size@vvf v1) (size@vf v2))
      )
    )
  )
)
"""
    x = AbstractValue((1, 3, 224, 224), Type.Vec(Type.Vec(Type.Vec(Type.Vec(Type.Float)))))
    m = translate_and_import(ks_str, "abstract")
    with ExecutionContext():
        assert m.get_shape(x).data == (1, 3, 224, 224)

def test_get_tuple_element():
    ks_str = """
(def flatten (Tuple Integer Integer Integer) ((x : (Tuple (Tuple Integer Integer) Integer)))
  (tuple@iii (get$1$2 (get$1$2 x)) (get$2$2 (get$1$2 x)) (get$2$2 x))
)
"""
    d = AbstractValue.from_data(((1, 2,), 3))
    m = translate_and_import(ks_str, "abstract")
    with ExecutionContext():
        assert m.flatten(d).data == (1, 2, 3)

    x = AbstractValue.from_data(((3, 4), 5))
    with ExecutionContext():
        assert m.flatten(x).data == (3, 4, 5)

def test_tuple_of_tensors():
    ks_str = """
(def fst (Vec (Vec Float)) ((x : (Tuple (Vec (Vec Float)) (Vec Float))))
    (get$1$2 x)
)
(def snd (Vec (Vec Float)) ((x : (Tuple (Vec (Vec Float)) (Vec Float))))
    (get$2$2 x)
)
"""
    m = translate_and_import(ks_str, "abstract")
    x = AbstractValue.from_data((np.random.normal(0, 1, (3, 4)), np.random.normal(0, 1, (4,))))
    with ExecutionContext():
        assert m.fst(x).shape_type == ((3, 4), Type.Vec(Type.Vec(Type.Float)))
        assert m.snd(x).shape_type == ((4,), Type.Vec(Type.Float))

def test_if_then_else():
    ks_str = """
(edef lt Bool (Float Float))
(def cost$lt@ii Float ((a : Float) (b : Float)) 1.0)
(def shape$lt@ii Integer ((a : Float) (b : Float)) 0)
(edef eq Bool (Float Float))
(def cost$eq@ii Float ((a : Float) (b : Float)) 1.0)
(def shape$eq@ii Integer ((a : Float) (b : Float)) 0)
(def sign Float ((x : Float))
    (if (lt x 0) -1.0 (if (eq x 0) 0.0 1.0))
)
"""
    m = translate_and_import(ks_str, "abstract")
    with ExecutionContext():
        assert m.sign(2.0) == 1.0
        assert m.sign(-5.0) == -1.0
        assert m.sign(0) == 0.0
        assert m.sign(AbstractValue((), Type.Float)).shape_type == ((), Type.Float)
