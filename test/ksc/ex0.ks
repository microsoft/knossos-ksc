; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(rule "mul2" (v : Float) (* v 2.0) (+ v v))

(def f Float ( x : Float )
     (let (y (* x 2.0)) (+ x y)))
