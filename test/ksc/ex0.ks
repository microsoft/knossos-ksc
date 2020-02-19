; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(rule "mul2" (v : Float) (mul@ff v 2.0) (add v v))
(def f Float (x : Float) (mul@ff 5.0 2.0))


; (rule "mult" (v : Float) (mul@ff (tuple v 2.0)) (add v v))
(def f_working2 Float (x : Float) (mul@ff (tuple 5.0 2.0)))

; (rule "tuple" (v : Float) (tuple v 2.0) (tuple v 3.0))
; (rule "wrong" () 2.0 3.0)
(def f_working Float (x : Float) 2.0)

(def g (Tuple Float Float) ( x : Float )
     (tuple (mul@ff 3.0 2.0) (add 5.0 5.0)))
