(def f Float (x : Float) x)

(def g Float (x : Float) (f x))

(gdef fwd [g Float])

(def main Integer () 0)
