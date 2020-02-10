; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h Float ( (x : Float) (y : Float))
       (let (z (add@ff x y))
            (mul@ff y z)))
