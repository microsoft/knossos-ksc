(rule "mul2" Float (v : Float) (* v 2.0) (+ v v))

(def f( x : Float ) Float
     (let (y (* x 2.0)) (+ x y)))
