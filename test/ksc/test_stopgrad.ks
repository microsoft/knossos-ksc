; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def tri Integer (n : Integer)
  (div (mul n (sub n 1)) 2))

(gdef fwd [tri Integer])
(gdef rev [tri Integer])
(gdef suffwdpass [tri Integer])
(gdef sufrevpass [tri Integer])
(gdef sufrev [tri Integer])

; NB This doesn't really test "stopgrad" per se anymore, but it is
; correct that we no longer try to differentiate with respect to
; Integer parameters

(def f Float ((x : Float) (n : Integer))
  (mul x (to_float n)))

(gdef fwd [f (Tuple Float Integer)])
(gdef rev [f (Tuple Float Integer)])
(gdef suffwdpass [f (Tuple Float Integer)])
(gdef sufrevpass [f (Tuple Float Integer)])
(gdef sufrev [f (Tuple Float Integer)])

(def main Integer () 0)
