; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float ( (n : Integer) (x : Float) )
    (if (eq n 1) 
        x 
        (mul x (f (sub n 1) x))))

(gdef fwd$f Float (Integer Float))
(gdef rev$f Float (Integer Float))

(def main Integer ()
    (let ((n 7)
          (x 1.31))
        (print
            "Compute x^3\n"
            (f n x) "\n"
            "Compute the gradient" "\n"
            (get$2$2 (rev$f (tuple n x) 1.0)) "\n"
            "And forward" "\n"
            (fwd$f (tuple n x) (tuple (tuple) 1.0)) "\n"
            "Is the derivative n*x^(n-1)?" "\n"
            (mul (to_float n) (f (sub n 1) x))
        )))
