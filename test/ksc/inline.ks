; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float (x : Float) (mul x x))

(gdef fwd [f Float])
(gdef rev [f Float])

(def g Float (y : Float) (add 1.0 ($inline (f (add y y)))))

(gdef fwd [g Float])
(gdef rev [g Float])

(def main Integer () 0)
