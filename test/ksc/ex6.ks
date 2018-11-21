(def f7 ( (x : Vec Float) (y : Vec Float) )
        (sum (build (tuple (size x) (lam (i : Integer)
                                       (* (index i x) (index i y)))))))
