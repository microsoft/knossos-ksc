; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

; This program demonstrates how Conalisation helps
; In reverse mode, in rev$h, we get one, rather than two, calls to rev$f

(def f Float ( x : Float )
       (mul x x))

(def g Float ( x : Float )
       (add x x))

(def h Float ( x : Float )
       (let (z (f x))
            (add z (g z))))
