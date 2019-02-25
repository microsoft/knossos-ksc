(def a Float ((x : Float))
    (* 3.0 x))

(def b Float ((y : Float))
    (* 2.0 y))

(def g Float ((x : Float) (y : Float))
    (* x y))

-- f = 3z/2y for z=5x so f = 15x/2y so linear in x
(def f1 Float ((x : Float) (y : Float))
    (let (z (* 5.0 x))
        (/ (a z) (b y))))

(def f Float ((x : Float) (y : Float))
    (/ x y))

(def main Float ()
    (pr (f (a 3.0) (b 2.3))
        (f 0.0 1.0)
        (D$f 1.1 2.2 )
        (g 1.1 2.2)
        (let ((x 1.1)
              (y 2.2)
              (delta 0.0001))
            (/ (- (f x (+ y delta)) (f x y)) delta))
        (fwd$f 1.1 2.2 0.0 1.0)
            ))
