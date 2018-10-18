(def exp. (v) 
    (build (size v) (lam i (exp (index i v)))))

(def logsumexp (v)
    (log (sum (exp. v))))

(def logsumexp_inlined (v)
    (log (sum (build (size v) (lam i (exp (index i v)))))))
