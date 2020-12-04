import numpy as np
import pytest

import ksc
from ksc.abstract_value import AbstractValue, ExecutionContext
from ksc.cost import compute_cost
from ksc.tracing.functions import math
from ksc.type import Type
from ksc.utils import translate_and_import

def test_afe():
    ks_str = """
(edef div Float (Float Float))
(def cost$div Float ((a : Float) (b : Float)) 2.0)
(def shape$div Integer ((a : Float) (b : Float)) 0)
(edef add Float (Float Float))
(def cost$add Float ((a : Float) (b : Float)) 1.0)
(def shape$add Integer ((a : Float) (b : Float)) 0)

(def afe Float ((x : Float))
  (let (a (div 1.0 x))
    (div a (add 1.0 a))
  )
)
"""
    with ExecutionContext() as ctx:
        m = translate_and_import(__file__, ks_str, "abstract")
        m.afe(AbstractValue((), Type.Float))
    assert ctx.costs[None] == 5.1

def test_build():
    ks_str = """
(edef add Float (Float Float))
(def cost$add Float ((a : Float) (b : Float)) 1.0)
(def shape$add Integer ((a : Float) (b : Float)) 0)

(def vec_vec_add (Vec Float) ((a : (Vec Float)) (b : (Vec Float)))
  (build (size a) (lam (i : Integer) (add (index i a) (index i b))))
)
"""
    args = [AbstractValue((100,), Type.Vec(Type.Float)), AbstractValue((100,), Type.Vec(Type.Float))]
    cost = compute_cost(ks_str, "vec_vec_add", args)
    assert cost == 401.0 # (size_cost) + 100 + 100 * (two index and one add)

def test_outer_product():
    ks_str = """
(edef mul Float (Float Float))
(def cost$mul Float ((a : Float) (b : Float)) 2.0)
(def shape$mul Integer ((a : Float) (b : Float)) 0)
(def outer_product (Vec (Vec Float))
 ((var0 : (Tuple (Vec Float) (Vec Float))))
    (let (x (get$1$2 var0))
    (let (y (get$2$2 var0))
    (let (m (size x))
    (let (n (size y))
            (build m (lam (i : Integer)
              (build n (lam (j : Integer)
                (mul
                  (index i x)
                  (index j y)))))))))))
"""
    args = [AbstractValue(((100,), (100,)), Type.Tuple(Type.Vec(Type.Float), Type.Vec(Type.Float)))]
    assert compute_cost(ks_str, "outer_product", args) == 50102.4


def test_sumbuild():
    ks_str = """
(edef add Float (Float Float))
(def cost$add Float ((a : Float) (b : Float)) 1.0)
(def shape$add Integer ((a : Float) (b : Float)) 0)

(def sum_of_vec Float ((a : (Vec Float)))
  (sumbuild (size a) (lam (i : Integer) (index i a)))
)
"""
    args = [AbstractValue((100,), Type.Vec(Type.Float))]
    assert compute_cost(ks_str, "sum_of_vec", args) == 200 # 100 * (index_cost) + 99 (for add) + (size_cost)

def test_rot():
    ks_str = """
(edef mul Float (Float Float))
(def cost$mul Float ((a : Float) (b : Float)) 2.0)
(def shape$mul Integer ((a : Float) (b : Float)) 0)
(edef add Float (Float Float))
(def cost$add Float ((a : Float) (b : Float)) 1.0)
(def shape$add Integer ((a : Float) (b : Float)) 0)
(edef neg Float (Float))
(def cost$neg Float ((a : Float)) 1.0)
(def shape$neg Integer ((a : Float)) 0)

(def rot (Tuple (Vec Float) (Vec Float))
         ((var0 : (Tuple (Vec Float) (Vec Float) Float Float)))
    (let (x (get$1$4 var0))
    (let (y (get$2$4 var0))
    (let (c (get$3$4 var0))
    (let (s (get$4$4 var0))
    (let (n (size x))
      (tuple
        (build n (lam (var1 : Integer)
          (add
              (mul c (index var1 x))
              (mul s (index var1 y)))))
        (build n (lam (i : Integer)
          (add
              (mul (neg s) (index i x))
              (mul c (index i y))))))))))))
"""
    args = [
        AbstractValue(
            (
                (100,),
                (100,),
                (),
                ()
            ),
            Type.Tuple(
                Type.Vec(Type.Float),
                Type.Vec(Type.Float),
                Type.Float,
                Type.Float
            )
        )
    ]
    assert compute_cost(ks_str, "rot", args) == 1701.7

def compile_and_compute_cost(f, args, arg_context=None, exec_context=None):
    # We cannot use variable length arguments because we use
    # inspect.getfullargspec in jitting.py to get arg_names
    @ksc.trace
    def wrapper(args):
        return f(*args)
    o = wrapper(args)
    o.data # use jax backend
    ks_str = o.creator._jitted.combined_ks_str()
    args = [AbstractValue.from_data(args, context=arg_context)]
    return compute_cost(ks_str, "wrapper", args, exec_context)

@pytest.mark.parametrize("shape", [(3,), (3, 2), (3, 2, 5)])
def test_translate(shape):
    x_n = np.random.normal(0, 1, shape)
    y_n = np.random.normal(0, 1, shape)
    assert compile_and_compute_cost(
        lambda x, y: x + y,
        (x_n, y_n)
    ) == np.prod(shape)

@ksc.trace
def dense(x, weights):
    W, b = weights
    return math.broadcast_add(math.dot(x, W), b)

def test_dense_cost():
    W_n = np.random.normal(0, 1, (3, 4))
    b_n = np.random.normal(0, 1, (4,))
    x_n = np.random.normal(0, 1, (5, 3))
    assert compile_and_compute_cost(
        lambda x, weights: dense(x, weights),
        (x_n, (W_n, b_n))
    ) == (3 * 4 * 5 * 2) + (5 * 4)

def test_states():
    x1 = np.random.normal(0, 1, (3, 2))
    y1 = np.random.normal(0, 1, (3, 2))
    x2 = np.random.normal(0, 1, (3, 2, 5))
    y2 = np.random.normal(0, 1, (3, 2, 5))

    f = lambda x, y: x + y
    with ExecutionContext() as ctx:
        compile_and_compute_cost(f, (x1, y1), arg_context=1, exec_context=ctx)
        compile_and_compute_cost(f, (x2, y2), arg_context=2, exec_context=ctx)
    assert ctx.costs[1] == 3 * 2
    assert ctx.costs[2] == 3 * 2 * 5

def test_if_then_else():
    ks_str = """
(edef mul Integer (Integer Integer))
(def cost$mul Float ((a : Integer) (b : Integer)) 2.0)
(def shape$mul Integer ((a : Integer) (b : Integer)) 0)
(edef eq Bool (Float Float))
(def cost$eq Float ((a : Float) (b : Float)) 1.0)
(def shape$eq Integer ((a : Float) (b : Float)) 0)

(def select1 (Vec Integer) ((p : Bool) (x : (Vec Integer)))
  (if p
      (build (size x) (lam (i : Integer) (mul i 2)))  ; expensive branch cost 301
      (build (size x) (lam (i : Integer) (index i x)))   ; cheap branch cost 201
  )
)
(def select2 (Vec Integer) ((p : Bool) (x : (Vec Integer)))
  (if p
      (build (size x) (lam (i : Integer) (index i x)))
      (build (size x) (lam (i : Integer) (mul i 2)))
  )
)
(def select3 (Vec Integer) ((x : (Vec Integer)))
  (if (eq 0 0)
      (build (size x) (lam (i : Integer) (mul i 2)))
      (build (size x) (lam (i : Integer) (index i x)))
  )
)
"""
    cost1 = compute_cost(ks_str,
                         "select1",
                         [
                             AbstractValue((), Type.Bool),
                             AbstractValue((100,), Type.Vec(Type.Integer))
                         ]
    )
    cost2 = compute_cost(ks_str,
                         "select2",
                         [
                             AbstractValue((), Type.Bool),
                             AbstractValue((100,), Type.Vec(Type.Integer))
                         ]
    )
    cost3 = compute_cost(ks_str,
                         "select3",
                         [
                             AbstractValue((100,), Type.Vec(Type.Integer))
                         ]
    )
    assert cost1 == 302.0201
    assert cost1 == cost2
    assert cost3 < cost1
