(def f7 ((x : Vec Float) (y : Vec Float) ) 
    (assert (== (size(x)) (size(y)))
        (sum (build (size x)
                    (lam (i : Integer) (* (if (< i 3) (index i x) 7.0) (index i y)))))))

(def test_tuple ((x : Tuple (Vec Float Vec Vec Float Integer)))
    (+ 1 (if (< 2 3) 4 5)))

(def main ()
    (let (v1 (build 3 (lam (i : Integer) (* 3.0 i))))
        (pr (f7 v1 v1)
            (D$f7 v1 v1))))
