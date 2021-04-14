(def doubleFloat Float (x : Float) (add x x))

(gdef fwd [doubleFloat Float])
(gdef rev [doubleFloat Float])
(gdef suffwdpass [doubleFloat Float])
(gdef sufrevpass [doubleFloat Float])
(gdef sufrev [doubleFloat Float])

(def muld Float ( (x : Float) (y : Float) )
  (mul (doubleFloat x) (doubleFloat y)))

(gdef fwd [muld (Tuple Float Float)])
(gdef rev [muld (Tuple Float Float)])
(gdef suffwdpass [muld (Tuple Float Float)])
(gdef sufrevpass [muld (Tuple Float Float)])
(gdef sufrev [muld (Tuple Float Float)])

(def main Integer () 0)
