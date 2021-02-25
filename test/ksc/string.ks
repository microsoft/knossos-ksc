; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def main Integer () (print "Hello"))

(def f String (s : String) s)

(gdef fwd [f String])
(gdef rev [f String])
(gdef suffwdpass [f String])
(gdef sufrevpass [f String])
(gdef sufrev [f String])
