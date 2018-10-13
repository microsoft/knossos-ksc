(def gmm_knossos_tri (n)
  (/ (* n (- n 1)) 2))
(def gmm_knossos_makeQ (q l)
    (let (d
      (length q))
    (build d (lam i
        (build d (lam j
          (if (< i j)
            0
            (if (== i j)
              (exp (get q i))
              (index l (+ (apply gmm_knossos_tri (- i 1)) j))))))))))

(def exp. (v) 
    (build (size v) (lam i (exp (index i v)))))

(def logsumexp (v)
    (log (sum (exp. v))))
