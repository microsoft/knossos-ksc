; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float ( (n : Integer) (x : Float) )
    (if (eq n 1) 
        x 
        (mul x (f (sub n 1) x))))

(def main Integer ()
    (let ((n 7)
          (x 1.31))
        (pr
            ; Compute x^3
            (f n x)
            ; Compute the gradient
            (get$2$2 (rev$f (tuple n x) 1.0))
            ; And forward
            (fwd$f (tuple n x) (tuple (tuple) 1.0))
            ; Is the derivative n*x^(n-1)?
            (mul (to_float n) (f (sub n 1) x))
        )))
