; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def tri Integer (n : Integer)
  (div@ii (mul@ii n (sub@ii n 1)) 2))

; NB This doesn't really test "stopgrad" per se anymore, but it is
; correct that we no longer try to differentiate with respect to
; Integer parameters

(def f Float ((x : Float) (n : Integer))
  (mul@ff x (to_float n)))
