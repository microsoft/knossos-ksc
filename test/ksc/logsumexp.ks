; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def exp$Vec (Vec Float) (v : Vec Float)
     (build (size v) (lam (i : Integer) (exp (index i v)))))

(def logsumexp Float (v : Vec Float)
    (log (sum (exp$Vec v))))

(def logsumexp_inlined Float (v : Vec Float)
    (log (sum (build (size v) (lam (i : Integer) (exp (index i v)))))))

(def sub$VecR$R (Vec Float) ((v : Vec Float) (x : Float))
     (build (size v) (lam (ni : Integer) (sub (index ni v) x))))

(def max_ Float ((x : Float) (y : Float)) (if (gt x y) x y))

(def max$VecR Float (v : Vec Float)
     (let ; We can't write -inf in a .ks file so use something very
          ; small instead
          (neg_infinity -1e38)
       (fold (lam (max_so_far_x : Tuple Float Float)
                  (let ((max_so_far (get$1$2 max_so_far_x))
                        (x (get$2$2 max_so_far_x)))
                    (max_ max_so_far x)))
             neg_infinity
             v)))

(def logsumexp_safe Float (a : Vec Float)
  (let (mx (max$VecR a))
    (let (sum_exp_minus_x (sum (exp$Vec (sub$VecR$R a mx))))
        (add (log sum_exp_minus_x) mx))))

(def main Integer () 0)
