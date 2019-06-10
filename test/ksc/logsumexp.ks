; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def exp. (Vec Float) (v : Vec Float)
    (build (size v) (lam (i : Integer) (exp (index i v)))))

(def logsumexp Float (v : Vec Float)
    (log (sum (exp. v))))

(def logsumexp_inlined Float (v : Vec Float)
    (log (sum (build (size v) (lam (i : Integer) (exp (index i v)))))))

(def logsumexp_safe Float ( a : Vec Float)
  (let (mx (max a))
    (let (sum_exp_minus_x (sum (exp$Vec (sub$Vec<R>$R a mx))))
        (+ (log sum_exp_minus_x) mx))))
