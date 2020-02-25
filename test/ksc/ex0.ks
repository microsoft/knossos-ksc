; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(rule "mul2" (v : Float) (mul@ff v 2.0) (add v v))
(rule "add2" (v : Float) (add v v) (mul@ff v 2.0))
(rule "add0" (v : Float) (add v 0.0) v)
(rule "mul0" (v : Float) (mul@ff 0.0 v ) 0.0)

(rule "let.elim" ((e : Float) (a : Float))
      (let (a e) a) e)

(def f (Tuple Float Float) (x : Float)
     (tuple (mul@ff 0.0 (let (a 2.0) a)) (add (mul@ff 5.0 2.0) 0.0)))

; (rule "mult" (v : Float) (mul@ff (tuple v 2.0)) (add v v))
(def f_working2 Float (x : Float) (mul@ff (tuple 5.0 2.0)))

; (rule "tuple" (v : Float) (tuple v 2.0) (tuple v 3.0))
; (rule "wrong" () 2.0 3.0)
(def f_working Float (x : Float) 2.0)

(def g (Tuple Float Float) ( x : Float )
     (tuple (mul@ff 3.0 2.0) (add 5.0 5.0)))
