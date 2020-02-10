; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def g Float ( x : Float )
     (let (y (mul@ff x x))
     (let (z (add@ff x y))
     (mul@ff y z))))
