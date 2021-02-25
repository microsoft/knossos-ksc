; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f Float (x : Float) (mul x -1.0))

(gdef fwd [f Float])
(gdef rev [f Float])
(gdef suffwdpass [f Float])
(gdef sufrevpass [f Float])
(gdef sufrev [f Float])

(def main Integer () 0)
