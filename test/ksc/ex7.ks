(def f7 Float (x : Vec Float)
        (sum (build (size x, (lam (i : Int) (negate (index i x)))))))
