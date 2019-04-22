(def h (Vec n Float) (x : Vec n Float)
    (build n (lam (i : Integer) 1.0)))

(def g Float (x : Vec p Float)
    (sum (h x)))
