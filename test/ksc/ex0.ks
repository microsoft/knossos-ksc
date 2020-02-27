; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(rule "mul2" (v : Float) (mul@ff v 2.0) (add v v))
(rule "add2" (v : Float) (add v v) (mul@ff v 2.0))

(rule "add0.elim" (v : Float) (add v 0.0) v)
(rule "mul0.elim" (v : Float) (mul@ff 0.0 v) 0.0)
(rule "mul1.elim" (v : Float) (mul@ff 1.0 v) v)

(rule "add0.intro" (v : Float) v (add v 0.0))
(rule "mul0.intro" (v : Float) 0.0 (mul@ff 0.0 v))
(rule "mul1.intro" (v : Float) v (mul@ff 1.0 v))

(rule "mul.assocr" ((x1 : Float) (x2 : Float) (x3 : Float))
      (mul@ff (mul@ff x1 x2) x3)
      (mul@ff x1 (mul@ff x2 x3)))
(rule "mul.assocl" ((x1 : Float) (x2 : Float) (x3 : Float))
      (mul@ff x1 (mul@ff x2 x3))
      (mul@ff (mul@ff x1 x2) x3))

(rule "let.elim" ((e : Float) (a : Float))
      (let (a e) a) e)

(def f (Tuple Float Float Float) (x : Float)
     (tuple (mul@ff 0.0 (let (a 2.0) a)) (add (mul@ff 5.0 2.0) 0.0)
            (h 0.0)
            ))

; (rule "mult" (v : Float) (mul@ff (tuple v 2.0)) (add v v))
(def f_working2 Float (x : Float) (mul@ff (tuple 5.0 2.0)))

; (rule "tuple" (v : Float) (tuple v 2.0) (tuple v 3.0))
; (rule "wrong" () 2.0 3.0)
(def h Float (x : Float) 2.0)

(def g (Tuple Float Float) ( x : Float )
     (tuple (mul@ff 3.0 2.0) (add 5.0 5.0)))
