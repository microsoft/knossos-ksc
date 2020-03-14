; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f Integer ( (y : Integer) (p : Integer))
    (let (y (add y y))
    (let (z (add y y))
    (let (y (mul p p))
    (sum (build 3 (lam (y : Integer) (add z (div y y)))))
    ))))

#| Tests capture-avoiding substitution |#
