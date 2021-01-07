; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h Integer ((y : Integer))
    (sum (build 3 (lam (y3 : Integer) (add y 2))))
    )

(def f Float (x : Float)
    ($trace (mul x x))
)

(gdef fwd [f Float])
(gdef rev [f Float])

(def g Float ((n : Integer) (m : Integer))
  (to_float (mul n m)))

(gdef fwd [g (Tuple Integer Integer)])
(gdef rev [g (Tuple Integer Integer)])

(def e Float ((vn : Vec Integer)
              (t : Tuple (Vec (Vec Float))
                         (Tuple Integer
                                (Vec Float))))
  (let ((n (size vn))
        (m (size (get$1$2 t))))
  (add (sum (build n (lam (i : Integer) (to_float i))))
     (sum (build m (lam (j : Integer) (sum (get$2$2 (get$2$2 t)))))))))

(gdef fwd [e (Tuple (Vec Integer)
                    (Tuple (Vec (Vec Float))
                           (Tuple Integer (Vec Float))))])
(gdef rev [e (Tuple (Vec Integer)
                    (Tuple (Vec (Vec Float))
                           (Tuple Integer (Vec Float))))])

(def test_inline (Vec Integer) (x : Vec Float)
    (build (size x) (lam (i : Integer) (mul i 2))))

(gdef fwd [test_inline (Vec Float)])
(gdef rev [test_inline (Vec Float)])

(def test_inline2 Integer (x : Vec Float)
  (let (n1 (size x))
    (let (x 4) x)))

(gdef fwd [test_inline2 (Vec Float)])
(gdef rev [test_inline2 (Vec Float)])

(def main Integer ()
    (print ; No grad.  See https://github.com/awf/knossos/issues/281 (D$f 9.0)
        (f 9.0) "\n"
        ([fwd f] 9.0 1.0) "\n"
        (build 4 (lam (i : Integer) i)) "\n"
        (e (build 7 (lam (i : Integer) i))
           (tuple (build 13 (lam (i : Integer) (build 7 (lam (i : Integer) 3.3))))
                  (tuple 23 (build 13 (lam (i : Integer) (to_float i))))))  "\n"
        "CHECK=" ($check (lam (x : Float) (f x))
                                (lam (t : Tuple Float Float) ([rev f] t))
                                (tuple 1.1)
                                1.1
                                (tuple 0.00001)
                                0.3)  "\n"
    )
)
