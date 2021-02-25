; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h (Vec Float) (x : Vec Float)
    (build (size x) (lam (i : Integer) 1.0)))

(gdef fwd [h (Vec Float)])
(gdef rev [h (Vec Float)])
(gdef suffwdpass [h (Vec Float)])
(gdef sufrevpass [h (Vec Float)])
(gdef sufrev [h (Vec Float)])

(def g Float (x : Vec Float)
    (sum (h x)))

(gdef fwd [g (Vec Float)])
(gdef rev [g (Vec Float)])
(gdef suffwdpass [g (Vec Float)])
(gdef sufrevpass [g (Vec Float)])
(gdef sufrev [g (Vec Float)])

(def main Integer () 0)
