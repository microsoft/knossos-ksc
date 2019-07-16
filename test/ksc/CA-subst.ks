; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f Integer ( (y : Integer) (p : Integer))
    (let (y (+ y y))
    (let (z (+ y y))
    (let (y (* p p))
    (sum (build 3 (lam (y : Integer) (+ z (/ y y)))))
    ))))

#| Tests capture-avoiding substitution |#
