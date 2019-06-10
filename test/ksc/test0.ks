; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h Integer ((y : Integer))
    (sum (build 3 (lam (y3 : Integer) (+ y 2))))
    )

(def f Float (x : Float)
    ($trace * x x)
)

(def g Float ((n : Integer) (m : Integer))
  (to_float (* n m)))

(def e Float ((vn : Vec n Integer) 
              (t : Tuple (Vec m (Vec n Float)) 
                         (Tuple Integer 
                                (Vec m Float))))
  (+ (sum (build n (lam (i : Integer) (to_float i))))
     (sum (build m (lam (j : Integer) (sum (get$2$2 (get$2$2 t))))))))

(def test_inline (Vec n Integer) (x : Vec n Float)
    (build n (lam (i : Integer) (* i 2))))

(def test_inline2 Integer (x : Vec n Float)
  (let (n1 (size x))
    (let (x 4) x)))

(def main Integer ()
    (pr ; No grad.  See https://github.com/awf/knossos/issues/281 (D$f 9.0)
        (f 9.0)
        (fwd$f 9.0 1.0)
        (build 4 (lam (i : Integer) i))
        (e (build 7 (lam (i : Integer) i))
           (tuple (build 13 (lam (i : Integer) (build 7 (lam (i : Integer) 3.3))))
                  (tuple 23 (build 13 (lam (i : Integer) (to_float i))))))
        (tuple "CHECK=" ($check f rev$f (tuple 1.1) (tuple 0.00001) 0.3))
    )
)
