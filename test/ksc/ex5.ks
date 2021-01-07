; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def mulvec (Vec Float) ( (x : Vec Float) (y : Vec Float) )
     (build (size x) (lam (i : Integer) (mul (index i x) (index i y)))))

(gdef fwd [mulvec (Tuple (Vec Float) (Vec Float))])
(gdef rev [mulvec (Tuple (Vec Float) (Vec Float))])

(def f6 Float ( (x : Vec Float) (y : Vec Float) )
        (sum (mulvec x y)))

(gdef fwd [f6 (Tuple (Vec Float) (Vec Float))])
(gdef rev [f6 (Tuple (Vec Float) (Vec Float))])

(def main Integer () 0)
