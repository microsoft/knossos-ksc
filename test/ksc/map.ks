(def square Float (x : Float) (mul x x))
(gdef suffwdpass [square Float])
(gdef sufrevpass [square Float])
(gdef sufrev [square Float])

(def squareall (Vec Float) (v : Vec Float) (map (lam (x : Float) (square x)) v))
(gdef suffwdpass [squareall (Vec Float)])
(gdef sufrevpass [squareall (Vec Float)])
(gdef sufrev [squareall (Vec Float)])

(def mulvec (Vec Float) ((v : Vec Float) (x : Float))
     (map (lam (y : Float) (mul y x)) v))
(gdef suffwdpass [mulvec (Tuple (Vec Float) (Float))])
(gdef sufrevpass [mulvec (Tuple (Vec Float) (Float))])
(gdef sufrev [mulvec (Tuple (Vec Float) (Float))])

(def squaretensor (Tensor 2 Float) (t : Tensor 2 Float)
     (map (lam (x : Float) (square x)) t))
(gdef suffwdpass [squaretensor (Tensor 2 Float)])
(gdef sufrevpass [squaretensor (Tensor 2 Float)])
(gdef sufrev [squaretensor (Tensor 2 Float)])

(def main Integer ()
     (print (map (lam (x : Float) (mul x 2.0))
                 (Vec_init 1.0 2.0 3.0 4.0))
            "\n"
            ([sufrev square] 3.0 1.0)
            "\n"
            (squareall (Vec_init 1.0 2.0 3.0 4.0))
            "\n"
            ([sufrev squareall] (Vec_init 1.0 2.0 3.0 4.0) (Vec_init 1.0 1.0 1.0 1.0))
            "\n"
            ([sufrev mulvec] (tuple (Vec_init 1.0 2.0 3.0 4.0) 3.0) (Vec_init 1.0 1.0 1.0 1.0))
            "\n"
            ([sufrev squaretensor] (build (tuple 5 5)
                                          (lam (ij : Tuple Integer Integer)
                                               (let ((i j) ij) (add (mul (to_float i) 10.0) (to_float j)))))
                     (build (tuple 5 5)
                            (lam (ij : Tuple Integer Integer) 1.0)))
            ))
