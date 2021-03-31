import numpy as np
from ksc.utils import translate_and_import

def test_vec_vec_add():
    ks_str = """(edef add Integer (Tuple Integer Integer))
(def test (Vec Integer) ((x : (Vec Integer)) (y : (Vec Integer)))
  (build (size x) (lam (i : Integer) (add (index i x) (index i y))))
)"""
    py_out = translate_and_import(__file__, ks_str, "common")
    x = np.array([0, 1, 2, 3, 4, 5])
    y = np.array([-1, 2, 0, -3, 5, 6])
    expected_output = [-1, 3, 2, 0, 9, 11]
    assert py_out.test(x, y) == expected_output
