; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f Integer ( (y : Integer) (p : Integer))
    (let (y (add y y))
    (let (z (add y y))
    (let (y (mul@ii p p))
    (sum (build 3 (lam (y : Integer) (add z (div@ii y y)))))
    ))))

#| Tests capture-avoiding substitution |#
