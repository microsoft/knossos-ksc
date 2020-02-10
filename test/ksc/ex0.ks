; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(rule "mul2" (v : Float) (mul@ff v 2.0) (add@ff v v))

(def f Float ( x : Float )
     (let (y (mul@ff x 2.0)) (add@ff x y)))
