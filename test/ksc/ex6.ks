(def f7 Float ( (x : Vec n Float) (y : Vec n Float) )
        (sum (build n (lam (i : Integer)
                           (* (index i x) (index i y))))))
