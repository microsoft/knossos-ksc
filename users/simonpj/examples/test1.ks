(def a ((Float x))
    (* 3 x))

(def b ((Float y))
    (* 2 y))

(def f ((Float x) (Float y))
    (let (z (* 5 x))
        (/ (a z) (b y))))
