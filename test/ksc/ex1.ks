; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def g Float ( x : Float )
     (let (y (mul@ff x x))
     (let (z (add x y))
     (mul@ff y z))))
