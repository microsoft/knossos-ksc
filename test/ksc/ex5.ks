(def mulvec (Vec n Float) ( (x : Vec n Float) (y : Vec n Float) )
     (build n (lam (i : Integer) (* (index i x) (index i y)))))

(def f6 Float ( (x : Vec n Float) (y : Vec n Float) )
        (sum (mulvec x y)))
