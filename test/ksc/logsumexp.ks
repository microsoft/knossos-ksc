(def exp. (v) 
    (build (size v) (lam i (exp (index i v)))))

(def logsumexp (v)
    (log (sum (exp. v))))

(def logsumexp_inlined (v)
    (log (sum (build (size v) (lam i (exp (index i v)))))))

(def logsumexp_safe (a)
  (let (mx (max a))
    (let (sum_exp_minus_x (sum (exp$Vec (sub$Vec<R>$R a mx))))
        (+ (log sum_exp_minus_x) mx))))
