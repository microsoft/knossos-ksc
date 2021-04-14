; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.

(def f1 Float () 0.0)

(gdef fwd [f1 (Tuple)])
(gdef rev [f1 (Tuple)])
(gdef suffwdpass [f1 (Tuple)])
(gdef sufrevpass [f1 (Tuple)])
(gdef sufrev [f1 (Tuple)])

(def f2 (Tuple Float) () (tuple 0.0))

(gdef fwd [f2 (Tuple)])
(gdef rev [f2 (Tuple)])
(gdef suffwdpass [f2 (Tuple)])
(gdef sufrevpass [f2 (Tuple)])
(gdef sufrev [f2 (Tuple)])

(def main Integer () 0)
