; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float ((x : Vec Float) (y : Vec Float))
    (if (lt 2 3) (index 0 x) 7.0)
)

(def q (Vec Float) ((r : Float) (m : Integer) (a : Vec Float))
  (let (n (size a))
    (build (mul 2 n) (lam (i : Integer) (mul r (index (div i m) a))))))

(def mkvec (Vec Float) (n : Integer)
    (build n (lam (j : Integer) (to_float j))))

(def sqnorm Float (v : Vec Float)
  (sum (build (size v) (lam (i : Integer) (let (vi (index i v)) (mul vi vi))))))

#|

(def g1 (gamma : Float)
    (let (ls     (build 10 (lam (i : Integer) (mkvec 3 gamma))))
         (sqnorm (index 0 ls))))
|#

(def g Float (gamma : Float)
    (let (v     (q gamma 3 (mkvec 3)))
         (sqnorm v)))

#|

(def main Integer ()
    (let ((v1 (build 4 (lam (i : Integer) (to_float i))))
          (delta 0.00001)
          (dv (build 4 (lam (i : Integer) (mul (to_float i) delta))))
          (dq (build 8 (lam (i : Integer) (div delta (to_float (add 3 i)))))))
        (print 1
            ; (D$f v1 v1)
            ; (D$g 1.1)
            (fwd$g 1.1 0.001)
            (rev$g 1.1 0.001)
            (sub (g 1.1001) (g 1.1))
            )))
|#
