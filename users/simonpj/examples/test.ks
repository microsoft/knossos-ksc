(def f7 (x y ) (assert (== (size(x)) (size(y)))
                 (sum (build (size x)
                             (lam i (* (index i x) (index i y)))))))

