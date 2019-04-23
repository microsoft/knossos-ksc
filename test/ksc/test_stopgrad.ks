(def tri Integer (n : Integer)
  (/ (* n (- n 1)) 2))

; NB This doesn't really test "stopgrad" per se anymore, but it is
; correct that we no longer try to differentiate with respect to
; Integer parameters

(def f Float ((x : Float) (n : Integer))
  (* x (to_float n)))
