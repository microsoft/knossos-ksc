(rule "mul2" (v : Float) (* v 2) (+ v v))

(def f( x : Float )
     (let (y (* x 2)) (+ x y)))
