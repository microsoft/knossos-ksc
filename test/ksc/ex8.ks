(def doubleFloat Float (x : Float) (+ x x))

(def muld Float ( (x : Float) (y : Float) )
  (* (doubleFloat x) (doubleFloat y)))
