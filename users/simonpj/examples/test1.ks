(def a ((x : Float))
    (* 3.0 x))

(def b ((y : Float))
    (* 2.0 y))

(def f ((x : Float) (y : Float))
    (let (z (* 5.0 x))
        (/ (a z) (b y))))
