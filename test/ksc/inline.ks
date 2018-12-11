(def f (x : Float) (* x x))

(def g (y : Float) (+ 1 (inline (f (+ y y)))))
