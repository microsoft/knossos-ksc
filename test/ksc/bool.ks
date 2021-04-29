; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def main Integer () (print true false))

(def g Bool () true)
(def h Float () (let (x (sin 2.0)) (assert (g) x)))

(gdef fwd [g (Tuple)])
(gdef rev [g (Tuple)])
(gdef suffwdpass [g (Tuple)])
(gdef sufrevpass [g (Tuple)])
(gdef sufrev [g (Tuple)])
