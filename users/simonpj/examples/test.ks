(def f7 ((Vec<Float> x) (Vec<Float> y) ) 
    (assert (== (size(x)) (size(y)))
        (sum (build (size x)
                    (lam (Integer i) (* (index i x) (index i y)))))))

