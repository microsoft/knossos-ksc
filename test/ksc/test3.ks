; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float ((x : Vec Float) (y : Vec Float))
    (if (lt 2 3) (index 0 x) 7.0)
)

(gdef fwd [f (Tuple (Vec Float) (Vec Float))])
(gdef rev [f (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [f (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [f (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [f (Tuple (Vec Float) (Vec Float))])

(def q (Vec Float) ((r : Float) (m : Integer) (a : Vec Float))
  (let (n (size a))
    (build (mul 2 n) (lam (i : Integer) (mul r (index (div i m) a))))))

(gdef fwd [q (Tuple Float Integer (Vec Float))])
(gdef rev [q (Tuple Float Integer (Vec Float))])
(gdef suffwdpass [q (Tuple Float Integer (Vec Float))])
(gdef sufrevpass [q (Tuple Float Integer (Vec Float))])
(gdef sufrev [q (Tuple Float Integer (Vec Float))])

(def mkvec (Vec Float) (n : Integer)
    (build n (lam (j : Integer) (to_float j))))

(gdef fwd [mkvec Integer])
(gdef rev [mkvec Integer])
(gdef suffwdpass [mkvec Integer])
(gdef sufrevpass [mkvec Integer])
(gdef sufrev [mkvec Integer])

(def sqnorm Float (v : Vec Float)
  (sum (build (size v) (lam (i : Integer) (let (vi (index i v)) (mul vi vi))))))

(gdef fwd [sqnorm (Vec Float)])
(gdef rev [sqnorm (Vec Float)])
(gdef suffwdpass [sqnorm (Vec Float)])
(gdef sufrevpass [sqnorm (Vec Float)])
(gdef sufrev [sqnorm (Vec Float)])

#|

(def g1 (gamma : Float)
    (let (ls     (build 10 (lam (i : Integer) (mkvec 3 gamma))))
         (sqnorm (index 0 ls))))
|#

(def g Float (gamma : Float)
    (let (v     (q gamma 3 (mkvec 3)))
         (sqnorm v)))

(gdef fwd [g Float])
(gdef rev [g Float])
(gdef suffwdpass [g Float])
(gdef sufrevpass [g Float])
(gdef sufrev [g Float])

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

(def main Integer () 0)
