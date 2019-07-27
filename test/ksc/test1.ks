; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def a Float ((x : Float))
    (mul 3.0 x))

(def b Float ((y : Float))
    (mul 2.0 y))

(def g Float ((x : Float) (y : Float))
    (mul x y))

; f = 3z/2y for z=5x so f = 15x/2y so linear in x
(def f1 Float ((x : Float) (y : Float))
    (let (z (mul 5.0 x))
        (div (a z) (b y))))

(def f Float ((x : Float) (y : Float))
    (div x y))

(def main Integer ()
    (let ((x 1.1)
          (y 2.2)
          (delta 0.0001))
      (pr 
        (f (a 3.0) (b 2.3))
        (f 0.0 1.0)
        ; See https://github.com/awf/knossos/issues/281 (D$f 1.1 2.2 )
        (g x y)
        "FD=" (div (sub (f x (add y delta)) (f x y)) delta)
        (fwd$f x y delta delta)
        "CHECK=" ($check f1 rev$f1 (tuple x y) (tuple delta delta) 0.1)
      )
    )
)
