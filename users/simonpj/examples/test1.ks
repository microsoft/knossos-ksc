(def a ((x : Float))
    (* 3.0 x))

(def b ((y : Float))
    (* 2.0 y))

(def g ((x : Float) (y : Float))
    (* x y))


(def f ((x : Float) (y : Float))
    (let (z (* 5.0 x))
        (/ (a z) (b y))))

(def main ()
    (pr (f (a 3.0) (b 2.3))
        (D$f 1.1 2.2)
        (let ((x 1.1)
              (y 2.2)
              (delta 0.1))
            (/ (- (f (+ x delta) y) (f x y)) delta))
        (fwd$f 1.1 2.2 1.0 0.0)
            ))
