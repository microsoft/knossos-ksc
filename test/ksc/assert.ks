; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f Float ((b : Bool) (x : Float))
     (let (a (assert b (let (n (add x x)) (add n n))))
     (let (n (add a x))
     (add n n))))

(def main Integer () 0)
