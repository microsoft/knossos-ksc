; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

#| Temporarily disabled until CatLang supports sum of Integer
(def f Float ( (y : Float) (p : Float))
    (let (y (add y y))
    (let (z (add y y))
    (let (y (mul p p))
    (sum (build 3 (lam (y : Integer) (add z (to_float (div y y))))))
    ))))
|#

#| Tests capture-avoiding substitution |#
