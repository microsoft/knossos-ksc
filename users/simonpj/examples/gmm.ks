(def gmm_knossos_tri ((n : Integer))
  (/ (* n (- n 1)) 2))

(def exp$VecR ((v : Vec Float))
  (build (size v) (lam (i : Integer) (exp (index i v)))))

(def mul$VecR$VecR ((a : Vec Float) (b : Vec Float))
  (assert (== (size a) (size b))
    (build (size a) (lam (i : Integer) (* (index i a) (index i b))))))

(def sub$VecR$VecR ((a : Vec Float) (b : Vec Float))
  (assert (== (size a) (size b))
    (build (size a) (lam (i : Integer) (- (index i a) (index i b))))))

(def dot ((a : Vec Float) (b : Vec Float))
  (sum (mul$VecR$VecR a b)))

(def sqnorm ((v : Vec Float))
  (dot v v))

-- M is vector of rows
(def mul$Mat$Vec ((M : Vec Vec Float) (v : Vec Float))
  (build (size M) (lam (i : Integer) (dot (index i M) v))))

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
       (alphas : Vec Float) (means : Vec Vec Float) (qs : Vec Vec Float) (ls : Vec Vec Float) 
       (wishart_gamma : Float) (wishart_m : Float))
  (let (n (size x))
  (let (d (size (index 0 x)))
  (let (K (size alphas))
      (+ (- (sum (build n (lam (i : Integer)
              (logsumexp (build K (lam (k : Integer)
                (let (mahal_vec
                  (mul$Mat$Vec (gmm_knossos_makeQ (index k qs) (index k ls)) 
                                          (sub$VecR$VecR (index i x) (index k means))))
                  (- (+ (index k alphas) (sum (index k qs)))
                    (* 0.500000 (sqnorm mahal_vec)))))))))) 
            (* (to_float n) (logsumexp alphas))) 
         (* 0.5 (sum (build K (lam (k : Integer)
                                            (+ (sqnorm (exp$VecR (index k qs))) 
                                                (sqnorm (index k ls))))))))))))

(def mkvec ((n : Integer))
    (build n (lam (j : Integer) (* 2.0 (+ 1.0 (to_float j))))))

(def main ()
  (let (x (build 10 (lam (i : Integer) (mkvec 3))))
    (let ((alphas (build 10 (lam (i : Integer) 7.0)))
          (mus    (build 10 (lam (i : Integer) (mkvec 3))))
          (qs     (build 10 (lam (i : Integer) (mkvec 3))))
          (ls     (build 10 (lam (i : Integer) (mkvec 3)))))
      (pr (mul$Mat$Vec (gmm_knossos_makeQ (index 0 qs) (index 0 ls)) (index 0 x))
          (gmm_knossos_gmm_objective x alphas mus qs ls 1.3 1.2)
          (D$gmm_knossos_gmm_objective x alphas mus qs ls 1.3 1.2)
          (fwd$gmm_knossos_gmm_objective x alphas mus qs ls 1.3 1.2
                                         x alphas mus qs ls 1.3 1.2)
          ))))
