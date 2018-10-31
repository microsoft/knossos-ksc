(def gmm_knossos_tri ((n : Integer))
  (/ (* n (- n 1)) 2))

(def exp$VecR ((v : Vec Float))
  (build (size v) (lam (i : Integer) (exp (index i v)))))

(def gmm_knossos_makeQ ((q : Vec Float) (l : Vec Float))
    (let (d
      (size q))
    (build d (lam (i : Integer)
        (build d (lam (j : Integer)
           (if (< i j)
            0.0
            (if (== i j)
              (exp (index i q))
              (index (+ (gmm_knossos_tri (- i 1)) j) l))
           )  
           ))))))

(def logsumexp ((v : Vec Float))
    (log (sum (exp$VecR v))))

(def gmm_knossos_gmm_objective 
      ((x : Vec Vec Float) 
       (alphas : Vec Vec Float) (means : Vec Vec Float) (qs : Vec Vec Float) (ls : Vec Vec Float) 
       (wishart_gamma : Float) (wishart_m : Float))
  (let (n (size x))
  (let (d (size (index 0 x)))
  (let (K (size alphas))
      (+ (- (linalg_vectorSum (build n (lam (i : Integer)
              (logsumexp (build K (lam (k : Integer)
                (let (mahal_vec
                  (linalg_matrixVectorMult (gmm_knossos_makeQ (index k qs) (index k ls)) 
                                          (linalg_vectorSub (index i x) (index k means))))
                  (- (+ (index k alphas) (linalg_vectorSum (index k qs)))
                    (* 0.500000 (linalg_sqnorm mahal_vec)))))))))) 
            (* n (logsumexp alphas))) 
         (* 0.5 (linalg_vectorSum (build K (lam (k : Integer)
                                            (+ (linalg_sqnorm (exp$VecR (index k qs))) 
                                                (linalg_sqnorm (index k ls))))))))))))

(def ks_main ()
  (let (x (build 10 (lam (i : Integer) (build 3 (lam (j : Integer) (* 2.0 j))))))
    (let (alphas (build 10 (lam (i : Integer) 7.0)))
      (gmm_knossos_gmm_objective x alphas x x x 1.3 1.2))))
