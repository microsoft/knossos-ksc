from ksc.utils import translate_and_import

def test_abs():
    ks_str = """
(edef abs Integer (Integer))
(def test Integer ((x : Integer))
    (abs x)
)
"""
    py_out = translate_and_import(ks_str, "common")
    assert py_out.test(0) == 0
    assert py_out.test(1) == 1
    assert py_out.test(-1) == 1

def test_max():
    ks_str = """
(edef max Integer (Integer Integer))
(def test Integer ((x : Integer) (y : Integer) (z : Integer))
    (max (max x y) z)
)
"""
    py_out = translate_and_import(ks_str, "common")
    x, y, z = 1, 2, -1
    assert py_out.test(x, y, z) == 2

def test_or():
    ks_str = """
(edef or Bool (Bool Bool))
(edef lt Bool (Integer Integer))
(edef gt Bool (Integer Integer))
(def test Bool ((x : Integer))
    (or (lt x 0) (gt x 0))
)
"""
    py_out = translate_and_import(ks_str, "common")
    assert py_out.test(1) == True
    assert py_out.test(0) == False
    assert py_out.test(-1) == True

def test_and():
    ks_str = """
(edef and Bool (Bool Bool))
(edef lt Bool (Integer Integer))
(edef gt Bool (Integer Integer))
(def test Bool ((x : Integer))
    (and (gt x 0) (lt x 2))
)
"""
    py_out = translate_and_import(ks_str, "common")
    assert py_out.test(0) == False
    assert py_out.test(1) == True
    assert py_out.test(2) == False
