; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(rule "mul2" (v : Float) (mul v 2.0) (add v v))

(def f Float ( x : Float )
     (let (y (mul x 2.0)) (add x y)))

(gdef fwd [f Float])
(gdef rev [f Float])

(def g (Vec Float) (a : Float)
     (let (x a)
       (build 10 (lam (a : Integer)
                      (if (eq a 0) 0.0 x)))))

(gdef fwd [g Float])
(gdef rev [g Float])

(def main Integer () 0)
