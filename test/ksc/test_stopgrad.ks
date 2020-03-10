; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def tri Integer (n : Integer)
  (div (mul n (sub n 1)) 2))

; NB This doesn't really test "stopgrad" per se anymore, but it is
; correct that we no longer try to differentiate with respect to
; Integer parameters

(def f Float ((x : Float) (n : Integer))
  (mul x (to_float n)))
