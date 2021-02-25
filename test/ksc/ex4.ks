; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f5 Float ( (x : Float) (y : Float) )
        (let ((p (mul 7.0 x))
              (r (div 1.0 y))
              (q (mul p (mul x 5.0)))
              (v (add (mul 2.0 (mul p q)) (mul 3.0 r))))
        v))

(gdef fwd [f5 (Tuple Float Float)])
(gdef rev [f5 (Tuple Float Float)])
(gdef suffwdpass [f5 (Tuple Float Float)])
(gdef sufrevpass [f5 (Tuple Float Float)])
(gdef sufrev [f5 (Tuple Float Float)])

(def main Integer () 0)
