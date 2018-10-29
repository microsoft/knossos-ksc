(def a (x)
    (* 3 x))

(def b (y)
    (* 2 y))

(def f (x y)
    (let (z (* 5 x))
        (/ (a z) (b y))))

