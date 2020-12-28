(def doubleFloat Float (x : Float) (add x x))

(def muld Float ( (x : Float) (y : Float) )
  (mul (doubleFloat x) (doubleFloat y)))

(def main Integer () 0)
