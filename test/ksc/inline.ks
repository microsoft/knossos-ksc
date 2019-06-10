; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float (x : Float) (* x x))

(def g Float (y : Float) (+ 1.0 ($inline (f (+ y y)))))
