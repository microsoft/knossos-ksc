; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f Integer ( (y : Integer) (p : Integer))
    (let (y (add y y))
    (let (z (add y y))
    (let (y (mul p p))
    (sum (build 3 (lam (y : Integer) (add z (div y y)))))
    ))))

(gdef fwd [f (Tuple Integer Integer)])
(gdef rev [f (Tuple Integer Integer)])
(gdef suffwdpass [f (Tuple Integer Integer)])
(gdef sufrevpass [f (Tuple Integer Integer)])
(gdef sufrev [f (Tuple Integer Integer)])

#| Tests capture-avoiding substitution |#

(def main Integer () 0)
