(def a ((R x))
    (* 3.0 x))

(def b ((Int y))
    (* 2 y))

(def f (x y)
    (let (z (* 5 x))
        (/ (a z) (b y))))

