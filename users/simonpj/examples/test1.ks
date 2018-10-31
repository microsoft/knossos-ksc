(def a ((x : Float))
    (* 3 x))

(def b ((y : Float))
    (* 2 y))

(def f ((x : Float) (y : Float))
    (let (z (* 5 x))
        (/ (a z) (b y))))
