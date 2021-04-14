; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h (Vec Float) (x : Vec Float)
  (build (mul 2 (size x)) (lam (i : Integer) 1.0)))

(gdef fwd [h (Vec Float)])
(gdef rev [h (Vec Float)])
(gdef suffwdpass [h (Vec Float)])
(gdef sufrevpass [h (Vec Float)])
(gdef sufrev [h (Vec Float)])

(def main Integer ()
  (print 1.1))
