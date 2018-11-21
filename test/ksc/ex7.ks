(def f7 (x : Vec Float)
        (sum (build (size x, (lam (i : Int) (negate (index i x)))))))
