(rule "sqr" (v : Float) (* v v) (square v))

(def f( x : Float )
     (let (y (* x x)) (+ x y)))
