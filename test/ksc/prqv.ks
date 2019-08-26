; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.


(def d Float ((x : Float))
     (let ((p (mul 7.0 x)))
       p))

(def d1 Float ((x : Float) (y : Float))
     (mul x y))

(def e Float ((x : Float))
     (let ((p (mul 7.0 x))
           (q (mul (mul p x) 5.0)))
       q))

(def e1 Float ((x : Float))
     (let ((p (mul 7.0 x))
           (q (mul p x)))
       q))

(def e2 Float ((x : Float))
     (let ((p (add 7.0 x))
           (q (add p x)))
       q))

(def f Float ((x : Float) (y : Float))
     (let ((p (mul 7.0 x))
           (r (div 11.0 y))
           (q (mul (mul p x) 5.0))
           (v (add (mul (mul 2.0 p) q) (mul 3.0 r))))
       v))

(def f2 Float ((x : Float) (y : Float))
     (let ((p (mul 7.0 x))
           (r (div 11.0 y))
           (q (mul (mul p x) 5.0)))
       (add (mul (mul 2.0 p) q) (mul 3.0 r))))

(def tminka Float ((x : Float) (y : Float))
     (let ((a (add (mul 2.0 x) (mul 3.0 y)))
           (b (add (mul 4.0 a) (mul 5.0 a)))
           (r (add (mul 6.0 b) (mul 7.0 b))))
       r))

(def alog Float ((x : Float) (y : Float))
     (exp (mul x (log (add x y)))))


(def anif Float ((x : Float) (y : Float))
     (if (gt x y)
         (sub x y)
       (let ((p (mul 7.0 x))
             (r (div 11.0 y))
             (q (mul (mul p x) 5.0))
             (v (add (mul (mul 2.0 p) q) (mul 3.0 r))))
         v)))

#|
(def abuild (Vec n Float) ((n : Integer) (x : Float))
     (build n (lam (i : Integer) x)))
|#

(def main Integer ()
     (print "13238.25 = " 13238.25 "\n"
            "f 3.0 4.0 = " (f 3.0 4.0) "\n"
            "revl$f 3.0 4.0 1.0 = " (revl$f 3.0 4.0 1.0) "\n"
            "rev$f 3.0 4.0 1.0 = " (rev$f 3.0 4.0 1.0) "\n"
            "e 3.0 = " (e 3.0) "\n"
            "revl$e 3.0 1.0 = " (revl$e 3.0 1.0) "\n"
            "rev$e 3.0 1.0 = " (rev$e 3.0 1.0) "\n"
            "e1 3.0 = " (e1 3.0) "\n"
            "revl$e1 3.0 1.0 = " (revl$e1 3.0 1.0) "\n"
            "rev$e1 3.0 1.0 = " (rev$e1 3.0 1.0) "\n"
            "e2 3.0 = " (e2 3.0) "\n"
            "revl$e2 3.0 1.0 = " (revl$e2 3.0 1.0) "\n"
            "rev$e2 3.0 1.0 = " (rev$e2 3.0 1.0) "\n"
            "d 3.0 = " (d 3.0) "\n"
            "revl$d 3.0 1.0 = " (revl$d 3.0 1.0) "\n"
            "rev$d 3.0 1.0 = " (rev$d 3.0 1.0) "\n"
            "alog 3.0 4.0 = " (alog 3.0 4.0) "\n"
            "revl$alog 3.0 4.0 1.0 = " (revl$alog 3.0 4.0 1.0) "\n"
            "rev$alog 3.0 4.0 1.0 = " (rev$alog 3.0 4.0 1.0) "\n"
            "anif 3.0 4.0 = " (anif 3.0 4.0) "\n"
            "revl$anif 3.0 4.0 1.0 = " (revl$anif 3.0 4.0 1.0) "\n"
            "rev$anif 3.0 4.0 1.0 = " (rev$anif 3.0 4.0 1.0) "\n"
#|
            "abuild 3.0 = " (abuild 3.0) "\n"
            "revl$d 3.0 1.0 = " (abuildl$d 3.0 1.0) "\n"
            "rev$d 3.0 1.0 = " (abuild$d 3.0 1.0) "\n"
|#
            ))
