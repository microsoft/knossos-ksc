; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def gmm_knossos_tri Integer ((n : Integer))
  (/ (* n (- n 1)) 2))

(def exp$VecR (Vec n Float) ((v : Vec n Float))
  (build n (lam (i : Integer) (exp (index i v)))))

(def mul$R$VecR (Vec n Float) ((r : Float) (a : Vec n Float))
    (build n (lam (i : Integer) (* r (index i a)))))

(def mul$R$VecVecR (Vec m (Vec n Float)) ((r : Float) (a : Vec m (Vec n Float)))
    (build m (lam (i : Integer) (mul$R$VecR r (index i a)))))

(def mul$VecR$VecR (Vec n Float) ((a : Vec n Float) (b : Vec n Float))
  (build n (lam (i : Integer) (* (index i a) (index i b)))))

(def sub$VecR$VecR (Vec n Float) ((a : Vec n Float) (b : Vec n Float))
  (build n (lam (i : Integer) (- (index i a) (index i b)))))

; dotv
(edef dotv Float ((Vec n Float) (Vec n Float)))
(edef D$dotv (LM (Tuple (Vec n Float) (Vec n Float)) Float) 
             ((Vec n Float) (Vec n Float)))
(edef R$dotv (LM Float (Tuple (Vec n Float) (Vec n Float))) ((Vec n Float) (Vec n Float)))
(def fwd$dotv Float ((a : Vec n Float) (b : Vec n Float) (da : Vec n Float) (db : Vec n Float))
    (+ (dotv a db) (dotv da b)))
(def rev$dotv (Tuple (Vec n Float) (Vec n Float))
               ((a : Vec n Float) (b : Vec n Float) (dr : Float))
    (tuple (mul$R$VecR dr b) (mul$R$VecR dr a)))

(def dotvv Float ((a : Vec m (Vec n Float)) (b : Vec m (Vec n Float)))
  (sum (build m (lam (i : Integer) (dotv (index i a) (index i b)))))
  )

(def sqnorm Float ((v : Vec n Float))
  (dotv v v))

; mul Mat Vec
(edef mul$Mat$Vec (Vec m Float) ((Vec m (Vec n Float)) (Vec n Float)))

(edef D$mul$Mat$Vec (LM (Tuple (Vec m (Vec n Float)) (Vec n Float)) (Vec m Float)) 
          ((Vec m (Vec n Float)) (Vec n Float)))

(edef R$mul$Mat$Vec (LM (Vec m Float) (Tuple (Vec m (Vec n Float)) (Vec n Float))) 
          ((Vec m (Vec n Float)) (Vec n Float)))

(def fwd$mul$Mat$Vec (Vec m Float) 
          ((M : Vec m (Vec n Float)) (v : Vec n Float) (dM : Vec m (Vec n Float)) (dv : Vec n Float))
    (+ (mul$Mat$Vec dM v) (mul$Mat$Vec M dv))) 

(edef rev$mul$Mat$Vec (Tuple (Vec m (Vec n Float)) (Vec n Float))
          ((Vec m (Vec n Float)) (Vec n Float) (Vec m Float)))



(def gmm_knossos_makeQ (Vec D (Vec D Float)) ((q : Vec D Float) (l : Vec triD Float))
  (assert (== triD (gmm_knossos_tri D))
    (build D (lam (i : Integer)
        (build D (lam (j : Integer)
           (if (< i j)
            0.0
            (if (== i j)
              (exp (index i q))
              (index (+ (gmm_knossos_tri (- i 1)) j) l))
           )
           ))))))

(def logsumexp Float ((v : Vec n Float))
    (log (sum (exp$VecR v))))

; TODO deriv lgamma - but no deriv wishart_m anyway.
; wishart_m -> int
(def log_gamma_distrib Float ((a : Float) (p : Integer))
    (let ((out (* 0.28618247146235004 (to_float (* p (- p 1)))))) ; 0.25 log pi
      (+ out
         (sum (build p (lam (j : Integer)
                 (lgamma (- a (* 0.5 (to_float j))))))))))

(def log_wishart_prior Float ((wishart : Tuple Float Integer)
                              (log_Qdiag : Vec p Float)
                              (ltri_Q : Vec tri_p Float))
    (let (
          (wishart_gamma (get$1$2 wishart))
          (wishart_m     (get$2$2 wishart))
          (sum_qs        (sum log_Qdiag))
          (Qdiag         (exp$VecR log_Qdiag))

          (n  (+ p (+ wishart_m 1)))
          (C  (- (* (to_float (* n p))
                    (- (log wishart_gamma) 
                       (* 0.5 (log 2.0))))
                 (log_gamma_distrib (* 0.5 (to_float n)) p)))
          (frobenius (+  (sqnorm Qdiag) (sqnorm ltri_Q)))
          (w2f   (* 0.5 (* (* wishart_gamma wishart_gamma) frobenius)))
          )
        (- (- w2f
              (* (to_float wishart_m) 
                  sum_qs))
            C)
    ))

(def gmm_knossos_gmm_objective Float
      ((x : Vec N (Vec D Float))
       (alphas : Vec K Float)
       (means : Vec K (Vec D Float))
       (qs : Vec K (Vec D Float))
       (ls : Vec K (Vec triD Float))
       (wishart : (Tuple Float Integer)))
  (assert (== triD (gmm_knossos_tri D))
    (let ((CONSTANT (* (to_float (* N D)) (neg 0.9189385332046727)) ) ; n * d*-0.5*log(2 * PI)
          (sum_qs   (build K (lam (k12 : Integer) (sum (index k12 qs)))))
          (slse     (sum (build N (lam (i : Integer)
                          (logsumexp (build K (lam (k : Integer)
                            (let ((Q         (gmm_knossos_makeQ (index k qs) (index k ls)))
                                  (mahal_vec (mul$Mat$Vec Q
                                                      (sub$VecR$VecR (index i x) (index k means)))))
                              (- (+ (index k alphas) 
                                    ; (index k sum_qs)
                                    (sum (index k qs))
                              )
                                (* 0.500000  (sqnorm mahal_vec)))
                            ))))
                          ))))
          )
            (+ (+ CONSTANT
                (- slse
                  (* (to_float N) (logsumexp alphas))))
            (sum (build K (lam (k : Integer)
                    (log_wishart_prior wishart (index k qs) (index k ls))))))
    )))
  
(def mkfloat Float ((seed  : Integer)
                    (scale : Float))
       (* ($ranhashdoub seed) scale))

(def mkvec (Vec n Float) ((seed  : Integer)
                          (n     : Integer)
                          (scale : Float))
    (build n (lam (j : Integer) (mkfloat (+ j seed) scale))))

(def mkvecvec (Vec n (Vec m Float)) ((seed  : Integer)
                                     (n     : Integer)
                                     (m     : Integer)
                                     (scale : Float))
     (build n (lam (j : Integer) (mkvec (+ (* j m) seed) m scale))))

(def main Integer ()
    (let ((D 64)
          (N 5)
          (K 64)
          (seed 0)
          (scale_unity 1.0)
          (scale_small 0.1)

          (x       (mkvecvec (+ seed 0)    N D scale_unity))
          (alphas  (mkvec    (+ seed 1000) K   scale_unity))
          (mus     (mkvecvec (+ seed 2000) K D scale_unity))
          (qs      (mkvecvec (+ seed 3000) K D scale_small))
          (ls      (mkvecvec (+ seed 4000) K (gmm_knossos_tri D) scale_unity))
          (wishart (tuple 3.1 7))
        )

      (pr x
          (rev$gmm_knossos_gmm_objective x alphas mus qs ls wishart 1.0)
          )))
