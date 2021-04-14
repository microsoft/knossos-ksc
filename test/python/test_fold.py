from ksc.utils import translate_and_import

def test_fold():
    ks_str = """
(edef add Integer (Tuple Integer Integer))
(edef sub Integer (Tuple Integer Integer))
(edef mul Integer (Tuple Integer Integer))
(edef div Integer (Tuple Integer Integer))
(edef eq Bool (Tuple Integer Integer))
(def mod (Integer) ((x : Integer) (y : Integer))
  (sub x (mul (div x y) y)))

(def test Integer ((n : Integer))
    (fold (lam (s_x : (Tuple Integer Integer))
        (let (count (get$1$2 s_x))
        (let (x     (get$2$2 s_x))
            (if (eq (mod x 3) 0)
                    (add count 1)
                    count
            ))))
        0
        (build n (lam (i : Integer) i))
    )
)
"""
    py_out = translate_and_import(__file__, ks_str, "common")
    n = 10
    expected_output = 4 # [0, 3, 6, 9]
    assert py_out.test(n) == expected_output
