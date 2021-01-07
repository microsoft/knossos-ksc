; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f7 Float ( (x : Vec Float) (y : Vec Float) )
        (sum (build (size x)
                    (lam (i : Integer)
                         (mul (index i x) (index i y))))))

(gdef fwd [f7 (Tuple (Vec Float) (Vec Float))])
(gdef rev [f7 (Tuple (Vec Float) (Vec Float))])

(def main Integer () 0)
