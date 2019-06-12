; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f1 Float ((x :  Float) (y :  Float) (i : Integer))
        (* (if (< i 3) (+ x 1.0) (* 7.0 (to_float i))) y)
)

(def f2 Float ((x : Vec n Float) (y : Vec m Float) (i : Integer) )
        (* (if (< i 3) (index i x) 7.0) (index i y))
)

(def f7 Float ((x : Vec n Float) (y : Vec m Float) )
    (assert (== (size(x)) (size(y)))
        (sum (build (size x)
                    (lam (i : Integer) (* (if (< i 3) (index i x) 7.0) (index i y))))))
)



(def main Integer ()
    (let (v1 (build 3 (lam (i : Integer) (* 3.0 (to_float i)))))
        (pr (f7 v1 v1)
            ; See https://github.com/awf/knossos/issues/281 (D$f7 v1 v1)
            ; See https://github.com/awf/knossos/issues/281 (D$f1 1.1 2.3 2)
            (fwd$f1 1.1 2.3 3 0.0 1.0 (tuple))
            (fwd$f1 1.1 2.3 3 1.0 0.0 (tuple))
            )))
