; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f7 Float (x : Vec Float)
        (sum (build (size x) (lam (i : Integer) (neg (index i x))))))

(gdef fwd [f7 (Vec Float)])
(gdef rev [f7 (Vec Float)])

(def main Integer () 0)
