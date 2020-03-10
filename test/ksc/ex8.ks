(def doubleFloat Float (x : Float) (add@ff x x))

(def muld Float ( (x : Float) (y : Float) )
  (mul (doubleFloat x) (doubleFloat y)))
