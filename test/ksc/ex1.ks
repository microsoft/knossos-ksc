; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def g Float ( x : Float )
     (let (y (mul x x))
     (let (z (add x y))
     (mul y z))))

(gdef fwd [g Float])
(gdef rev [g Float])
(gdef suffwdpass [g Float])
(gdef sufrevpass [g Float])
(gdef sufrev [g Float])

(def main Integer () 0)
