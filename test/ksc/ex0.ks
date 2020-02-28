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
(rule "mul.comm" ((x1 : Float) (x2 : Float))
      (mul@ff x1 x2)
      (mul@ff x2 x1))

(rule "add.assocr" ((x1 : Float) (x2 : Float) (x3 : Float))
      (add (add x1 x2) x3)
      (add x1 (add x2 x3)))
(rule "add.assocl" ((x1 : Float) (x2 : Float) (x3 : Float))
      (add x1 (add x2 x3))
      (add (add x1 x2) x3))
(rule "add.comm" ((x1 : Float) (x2 : Float))
      (add x1 x2)
      (add x2 x1))

(rule "let.elim" ((e : Float) (a : Float))
      (let (a e) a) e)

(rule "tuple.if" ((c : Bool) (a1 : Float) (a2 : Float) (b1 : Float) (b2 : Float))
      (tuple (if c a1 b1) (if c a2 b2))
      (if c (tuple a1 a2) (tuple b1 b2)))

(rule "if.tuple" ((c : Bool) (a1 : Float) (a2 : Float) (b1 : Float) (b2 : Float))
      (if c (tuple a1 a2) (tuple b1 b2))
      (tuple (if c a1 b1) (if c a2 b2)))

(rule "or.intro" (p : Bool) p (or p false))
(rule "or.elim" (p : Bool) (or p false) p)

(rule "and.intro" (p : Bool) p (and p true))
(rule "and.elim" (p : Bool) (and p true) p)

(rule "if.true.elim" ((t : Float) (f : Float))
      (if true t f) t)

(rule "if.false.elim" ((t : Float) (f : Float))
      (if false t f) f)

(def f (Tuple Float Float (Tuple Float Float)) (x : Float)
     (tuple (mul@ff 0.0 (let (a 2.0) a)) (add (mul@ff 5.0 2.0) 0.0)
            (tuple (if true 2.0 5.0) (if true 7.0 11.0))
            ))

; (rule "mult" (v : Float) (mul@ff (tuple v 2.0)) (add v v))
(def f_working2 Float (x : Float) (mul@ff (tuple 5.0 2.0)))

; (rule "tuple" (v : Float) (tuple v 2.0) (tuple v 3.0))
; (rule "wrong" () 2.0 3.0)
(def h Float (x : Float) 2.0)

(def g (Tuple Float Float) ( x : Float )
     (tuple (mul@ff 3.0 2.0) (add 5.0 5.0)))
