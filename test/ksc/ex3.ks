; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h Float ( (x : Float) (y : Float))
       (let (z (add x y))
            (mul y z)))

(def h2 Float (t : (Tuple Float Float))
     (let (((x y) t)
           (z (add x y)))
       (mul y z)))

(def i Float ( (x : Float) (y : Float))
       (let (z (add x y))
            0.0))

(def j0 Bool ((b : Bool) (b1 : Bool))
     (if b b (and b b1)))

(def j Float ((x : Float) (y : Float) (t : (Tuple Float Float)))
     (if (eq (let (z (add x y))
               (mul y z))
             (let (z (add x x))
               (mul z z)))
         (let (((x y) t)
               (z (add x y)))
           (mul y z))
       (let (z (add x y))
         0.0)))

(def d2 Float ((x : Float) (y : Integer) (z : Integer) (t : Integer))
     (if (eq (tuple (add y z) (sub t y))
             (tuple t z))
         (add x (to_float y))
         0.0))

(def main Integer () 0)
