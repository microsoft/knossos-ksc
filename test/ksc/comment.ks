; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
; The presence of a comment at the start of file used to cause the
; whole file to be ignored (prior to be6a969574c75d787eee681ccbf8) or
; a compile failure (subsequently).  That no longer happens and this
; file tests it (or at least the latter).

(def f Float () 0.0)

(gdef fwd [f (Tuple)])
(gdef rev [f (Tuple)])
(gdef suffwdpass [f (Tuple)])
(gdef sufrevpass [f (Tuple)])
(gdef sufrev [f (Tuple)])

(def main Integer () 0)
