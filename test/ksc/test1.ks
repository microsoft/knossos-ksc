; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def a Float ((x : Float))
    (mul 3.0 x))

(gdef fwd [a Float])
(gdef rev [a Float])
(gdef suffwdpass [a Float])
(gdef sufrevpass [a Float])
(gdef sufrev [a Float])

(def b Float ((y : Float))
    (mul 2.0 y))

(gdef fwd [b Float])
(gdef rev [b Float])
(gdef suffwdpass [b Float])
(gdef sufrevpass [b Float])
(gdef sufrev [b Float])

(def g Float ((x : Float) (y : Float))
    (mul x y))

; f = 3z/2y for z=5x so f = 15x/2y so linear in x
(def f1 Float ((x : Float) (y : Float))
    (let (z (mul 5.0 x))
        (div (a z) (b y))))

(gdef fwd [f1 (Tuple Float Float)])
(gdef rev [f1 (Tuple Float Float)])
(gdef suffwdpass [f1 (Tuple Float Float)])
(gdef sufrevpass [f1 (Tuple Float Float)])
(gdef sufrev [f1 (Tuple Float Float)])

(def f Float ((x : Float) (y : Float))
    (div x y))

(gdef fwd [f (Tuple Float Float)])
(gdef rev [f (Tuple Float Float)])
(gdef suffwdpass [f (Tuple Float Float)])
(gdef sufrevpass [f (Tuple Float Float)])
(gdef sufrev [f (Tuple Float Float)])

(def main Integer ()
    (let ((x 1.1)
          (y 2.2)
          (delta 0.0001)
          (nl "\n"))
      (print
        (f (a 3.0) (b 2.3)) nl 
        (f 0.0 1.0) nl
        ; See https://github.com/awf/knossos/issues/281 (D$f 1.1 2.2 )
        (g x y) nl
        "FD=" (div (sub (f x (add y delta)) (f x y)) delta) 
        " vs " ([fwd f] (tuple x y) (tuple delta delta)) nl
        "CHECK=" ($check (lam (t : Tuple Float Float) (f1 t))
                         (lam (t : Tuple (Tuple Float Float) Float) ([rev f1] t))
                         (tuple x y)
                         (tuple x y)
                         (tuple delta delta)
                         0.1)
      )
    )
)
