(def f Float (x : Float) (* x x))

(def g Float (y : Float) (+ 1 (inline (f (+ y y)))))
