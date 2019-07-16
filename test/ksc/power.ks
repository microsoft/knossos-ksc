; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float ( (n : Integer) (x : Float) )
    (if (== n 1)
        x
        (* x (f (- n 1) x))))

(def main Integer ()
    (let ((n 7)
          (x 1.31))
        (pr
            ; Compute x^3
            (f n x)
            ; Compute the gradient
            (get$2$2 (rev$f n x 1.0))
            ; And forward
            (fwd$f n x (tuple) 1.0)
            ; Is the derivative n*x^(n-1)?
            (* (to_float n) (f (- n 1) x))
        )))
