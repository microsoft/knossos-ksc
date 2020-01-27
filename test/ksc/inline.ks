; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float (x : Float) (mul@ff x x))

(def g Float (y : Float) (add 1.0 ($inline (f (add y y)))))
