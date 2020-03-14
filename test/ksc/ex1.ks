; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def g Float ( x : Float )
     (let (y (mul x x))
     (let (z (add x y))
     (mul y z))))
