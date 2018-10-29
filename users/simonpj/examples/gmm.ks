(def gmm_knossos_tri ((const n))
  (/ (* n (- n 1)) 2))

(def gmm_knossos_makeQ (q l)
    (let (d
      (length q))
    (build d (lam i
        (build d (lam j
          (if (< i j)
            0.000000
            (if (== i j)
              (exp (index q i))
              (index l (+ (gmm_knossos_tri (- i 1)) j))))))))))

(def logsumexp (v)
    (log (sum (exp$Vec v))))

(def gmm_knossos_gmm_objective (x alphas means qs ls wishart_gamma wishart_m)
  (let (n (corelang_length x))
  (let (d (corelang_length (index x 0)))
  (let (K (corelang_length alphas))
      (+ (- (linalg_vectorSum (corelang_build n (lam i
              (logsumexp (corelang_build K (lam k
                (let (mahal_vec
                  (linalg_matrixVectorMult (gmm_knossos_makeQ (index qs k) (index ls k)) 
                                          (linalg_vectorSub (index x i) (index means k))))
                  (- (+ (index alphas k) (linalg_vectorSum (index qs k)))
                    (* 0.500000 (linalg_sqnorm mahal_vec)))))))))) 
            (* (double (cardToInt n)) (gmm_knossos_logsumexp alphas))) 
         (* 0.500000 (linalg_vectorSum (corelang_build K (lam k
                                            (+ (linalg_sqnorm (linalg_vectorMap (lam value
                                                                    (exp value)) (index qs k))) 
                                                (linalg_sqnorm (index ls k))))))))))))

(def tmain ()
  (let (x (build 10 (lam i (build 3 (lam j (* 2.0 j))))))
    (let (alphas (build 10 (lam i 7.0)))
      (gmm_knossos_gmm_objective x alphas x x x 1.3 1.2))))
