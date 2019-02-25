(def f7 Float ( (x : Vec Float) (y : Vec Float) )
        (sum (build (tuple (size x) (lam (i : Integer)
                                       (* (index i x) (index i y)))))))
