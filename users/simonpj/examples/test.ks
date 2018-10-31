(def f7 ((x : Vec Float) (y : Vec Float) ) 
    (assert (== (size(x)) (size(y)))
        (sum (build (size x)
                    (lam (i : Integer) (* (index i x) (index i y)))))))

(def test_tuple ((x : Tuple (Vec Float Vec Vec Float Integer)))
    1)

(def ks_main ()
    (let (v1 (build 3 (lam (i : Integer) (* 3.0 i))))
        (f7 v1 v1)))
