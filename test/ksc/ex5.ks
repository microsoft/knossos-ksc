(def mulvec (Vec Float) ( (x : Vec Float) (y : Vec Float) )
     (build (size x) (lam (i : Integer) (* (index i x) (index i y)))))

(def f6 Float ( (x : Vec Float) (y : Vec Float) )
        (sum (mulvec x y)))
