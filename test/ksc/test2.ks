
(def f1 Float ((x :  Float) (y :  Float) (i : Integer))
        (* (if (< i 3) (+ x 1.0) (* 7.0 (to_float i))) y)
)

(def f2 Float ((x : Vec Float) (y : Vec Float) (i : Integer) )
        (* (if (< i 3) (index i x) 7.0) (index i y))
)

(def f7 Float ((x : Vec Float) (y : Vec Float) )
    (assert (== (size(x)) (size(y)))
        (sum (build (size x)
                    (lam (i : Integer) (* (if (< i 3) (index i x) 7.0) (index i y))))))
)


(def test_tuple Integer ((x : Tuple (Vec Float Vec Vec Float Integer)))
    (+ 1 (if (< 2 3) 4 5)))

(def main Float ()
    (let (v1 (build 3 (lam (i : Integer) (* 3.0 (to_float i)))))
        (pr (f7 v1 v1)
            (D$f7 v1 v1)
            (D$f1 1.1 2.3 2)
            (fwd$f1 1.1 2.3 3 0.0 1.0 0)
            (fwd$f1 1.1 2.3 3 1.0 0.0 0)
            )))
