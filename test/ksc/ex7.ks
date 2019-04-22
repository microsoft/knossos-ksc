(def f7 Float (x : Vec n Float)
        (sum (build n (lam (i : Integer) (neg (index i x))))))
