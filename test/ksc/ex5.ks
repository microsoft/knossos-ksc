; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def mulvec (Vec Float) ( (x : Vec Float) (y : Vec Float) )
     (build (size x) (lam (i : Integer) (mul (index i x) (index i y)))))

(def f6 Float ( (x : Vec Float) (y : Vec Float) )
        (sum (mulvec x y)))

(def main Integer () 0)
