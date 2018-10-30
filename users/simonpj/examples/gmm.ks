(def gmm_knossos_tri ((Integer n))
  (/ (* n (- n 1)) 2))

(def exp$VecR ((Vec<Float> v))
  (build (size v) (lam (Integer i) (exp (index i v)))))

(def gmm_knossos_makeQ ((Vec<Float> q) (Vec<Float> l))
    (let (d
      (size q))
    (build d (lam (Integer i)
        (build d (lam (Integer j)
           (if (< i j)
            0.000000
            (if (== i j)
              (exp (index i q))
              (index (+ (gmm_knossos_tri (- i 1)) j) l))
           )  
           ))))))

{-

(def logsumexp ((Vec<Float> v))
    (log (sum (exp$Vec<Float> v))))

(def gmm_knossos_gmm_objective 
      ((Vec<Vec<Float>> x) 
       (Vec<Vec<Float>> alphas) (Vec<Vec<Float>> means) (Vec<Vec<Float>> qs) (Vec<Vec<Float>> ls) 
       (Float wishart_gamma) (Float wishart_m))
  (let (n (corelang_length x))
  (let (d (corelang_length (index 0 x)))
  (let (K (corelang_length alphas))
      (+ (- (linalg_vectorSum (corelang_build n (lam (Integer i)
              (logsumexp (corelang_build K (lam (Integer k)
                (let (mahal_vec
                  (linalg_matrixVectorMult (gmm_knossos_makeQ (index k qs) (index k ls)) 
                                          (linalg_vectorSub (index i x) (index k means))))
                  (- (+ (index k alphas) (linalg_vectorSum (index k qs)))
                    (* 0.500000 (linalg_sqnorm mahal_vec)))))))))) 
            (* (double (cardToInt n)) (gmm_knossos_logsumexp alphas))) 
         (* 0.500000 (linalg_vectorSum (corelang_build K (lam (Integer k)
                                            (+ (linalg_sqnorm (linalg_vectorMap (lam (Float value)
                                                                    (exp value)) (index k qs))) 
                                                (linalg_sqnorm (index k ls))))))))))))

(def ks_main ()
  (let (x (build 10 (lam (Integer i) (build 3 (lam (Integer j) (* 2.0 j))))))
    (let (alphas (build 10 (lam (Integer i) 7.0)))
      (gmm_knossos_gmm_objective x alphas x x x 1.3 1.2))))

-}
