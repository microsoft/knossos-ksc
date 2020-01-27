(def doubleFloat Float (x : Float) (add x x))

(def muld Float ( (x : Float) (y : Float) )
  (mul@ff (doubleFloat x) (doubleFloat y)))
