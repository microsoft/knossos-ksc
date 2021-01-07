(def doubleFloat Float (x : Float) (add x x))

(gdef fwd [doubleFloat Float])
(gdef rev [doubleFloat Float])

(def muld Float ( (x : Float) (y : Float) )
  (mul (doubleFloat x) (doubleFloat y)))

(gdef fwd [muld (Tuple Float Float)])
(gdef rev [muld (Tuple Float Float)])

(def main Integer () 0)
