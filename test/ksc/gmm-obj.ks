; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def gmm_knossos_tri Integer ((n : Integer))
  (div@ii (mul n (sub@ii n 1)) 2))

(def exp$VecR (Vec Float) ((v : Vec Float))
  (build (size v) (lam (i : Integer) (exp (index i v)))))

(def mul$R$VecR (Vec Float) ((r : Float) (a : Vec Float))
    (build (size a) (lam (i : Integer) (mul r (index i a)))))

(def mul$R$VecVecR (Vec (Vec Float)) ((r : Float) (a : Vec (Vec Float)))
    (build (size a) (lam (i : Integer) (mul$R$VecR r (index i a)))))

(def mul$VecR$VecR (Vec Float) ((a : Vec Float) (b : Vec Float))
  (build (size a) (lam (i : Integer) (mul (index i a) (index i b)))))

(def sub$VecR$VecR (Vec Float) ((a : Vec Float) (b : Vec Float))
  (build (size a) (lam (i : Integer) (sub@ff (index i a) (index i b)))))

; dotv
(edef dotv Float ((Vec Float) (Vec Float)))
(edef D$dotv (LM (Tuple (Vec Float) (Vec Float)) Float)
             ((Vec Float) (Vec Float)))
(edef Dt$dotv (Tuple Float (LM (Tuple (Vec Float) (Vec Float)) Float))
              ((Vec Float) (Vec Float)))
(edef R$dotv (LM Float (Tuple (Vec Float) (Vec Float))) ((Vec Float) (Vec Float)))
(def fwd$dotv Float ((a : Vec Float) (b : Vec Float) (da : Vec Float) (db : Vec Float))
    (add (dotv a db) (dotv da b)))
(def rev$dotv (Tuple (Vec Float) (Vec Float))
               ((a : Vec Float) (b : Vec Float) (dr : Float))
    (tuple (mul$R$VecR dr b) (mul$R$VecR dr a)))

(def dotvv Float ((a : Vec (Vec Float)) (b : Vec (Vec Float)))
  (sum (build (size a) (lam (i : Integer) (dotv (index i a) (index i b)))))
  )

(def sqnorm Float ((v : Vec Float))
  (dotv v v))

; mul Mat Vec
(edef mul$Mat$Vec (Vec Float) ((Vec (Vec Float)) (Vec Float)))

(edef D$mul$Mat$Vec (LM (Tuple (Vec (Vec Float)) (Vec Float)) (Vec Float))
          ((Vec (Vec Float)) (Vec Float)))
(edef Dt$mul$Mat$Vec (Tuple (Vec Float) (LM (Tuple (Vec (Vec Float)) (Vec Float)) (Vec Float)))
          ((Vec (Vec Float)) (Vec Float)))

(edef R$mul$Mat$Vec (LM (Vec Float) (Tuple (Vec (Vec Float)) (Vec Float)))
          ((Vec (Vec Float)) (Vec Float)))

(def fwd$mul$Mat$Vec (Vec Float)
          ((M : Vec (Vec Float)) (v : Vec Float) (dM : Vec (Vec Float)) (dv : Vec Float))
    (add (mul$Mat$Vec dM v) (mul$Mat$Vec M dv)))

(edef rev$mul$Mat$Vec (Tuple (Vec (Vec Float)) (Vec Float))
          ((Vec (Vec Float)) (Vec Float) (Vec Float)))



(def gmm_knossos_makeQ (Vec (Vec Float)) ((q : Vec Float) (l : Vec Float))
 (let (D (size q))
   (assert (eq (size l) (gmm_knossos_tri D))
    (build D (lam (i : Integer)
        (build D (lam (j : Integer)
           (if (lt@ii i j)
            0.0
            (if (eq i j)
              (exp (index i q))
              (index (add (gmm_knossos_tri (sub@ii i 1)) j) l))
           )
           )))))))

(def logsumexp Float ((v : Vec Float))
    (log (sum (exp$VecR v))))

(edef lgamma Float (Float))
(edef D$lgamma (LM Float Float) (Float))
(edef fwd$lgamma Float (Float Float))
(edef rev$lgamma Float (Float Float))
(edef Dt$lgamma (Tuple Float (LM Float Float)) (Float))

; TODO deriv lgamma - but no deriv wishart_m anyway.
; wishart_m -> int
(def log_gamma_distrib Float ((a : Float) (p : Integer))
    (let ((out (mul 0.28618247146235004 (to_float (mul p (sub@ii p 1)))))) ; 0.25 log pi
      (add out
         (sum (build p (lam (j : Integer)
                 (lgamma (sub@ff a (mul 0.5 (to_float j))))))))))

(def log_wishart_prior Float ((wishart : Tuple Float Integer)
                              (log_Qdiag : Vec Float)
                              (ltri_Q : Vec Float))
    (let (
          (p (size log_Qdiag))
          (wishart_gamma (get$1$2 wishart))
          (wishart_m     (get$2$2 wishart))
          (sum_qs        (sum log_Qdiag))
          (Qdiag         (exp$VecR log_Qdiag))

          (n  (add p (add wishart_m 1)))
          (C  (sub@ff (mul (to_float (mul n p))
                    (sub@ff (log wishart_gamma)
                       (mul 0.5 (log 2.0))))
                 (log_gamma_distrib (mul 0.5 (to_float n)) p)))
          (frobenius (add  (sqnorm Qdiag) (sqnorm ltri_Q)))
          (w2f   (mul 0.5 (mul (mul wishart_gamma wishart_gamma) frobenius)))
          )
        (sub@ff (sub@ff w2f
              (mul (to_float wishart_m)
                  sum_qs))
            C)
    ))

(def gmm_knossos_gmm_objective Float
      ((x : Vec (Vec Float))        ; N x D
       (alphas : Vec Float)         ; K
       (means : Vec (Vec Float))    ; K x D
       (qs : Vec (Vec Float))       ; K x D
       (ls : Vec (Vec Float))       ; K x triD
       (wishart : (Tuple Float Integer)))
 (let ((N (size x))
       (K (size alphas))
       (D (size (index 0 x)))       ; Ugh
       (triD (size (index 0 ls)))   ; Ugh
      )
  (assert (eq triD (gmm_knossos_tri D))
    (let ((CONSTANT (mul (to_float (mul N D)) (neg 0.9189385332046727)) ) ; n * d*-0.5*log(2 * PI)
          (sum_qs   (build K (lam (k12 : Integer) (sum (index k12 qs)))))
          (slse     (sum (build N (lam (i : Integer)
                          (logsumexp (build K (lam (k : Integer)
                            (let ((Q         (gmm_knossos_makeQ (index k qs) (index k ls)))
                                  (mahal_vec (mul$Mat$Vec Q
                                                      (sub$VecR$VecR (index i x) (index k means)))))
                              (sub@ff (add (index k alphas)
                                    ; (index k sum_qs)
                                    (sum (index k qs))
                              )
                                (mul 0.500000  (sqnorm mahal_vec)))
                            ))))
                          ))))
          )
            (add (add CONSTANT
                (sub@ff slse
                  (mul (to_float N) (logsumexp alphas))))
            (sum (build K (lam (k : Integer)
                    (log_wishart_prior wishart (index k qs) (index k ls))))))
    ))))

(def mkfloat Float ((seed  : Integer)
                    (scale : Float))
       (mul ($ranhashdoub seed) scale))

(def mkvec (Vec Float) ((seed  : Integer)
                        (n     : Integer)
                        (scale : Float))
    (build n (lam (j : Integer) (mkfloat (add j seed) scale))))

(def mkvecvec (Vec (Vec Float)) ((seed  : Integer)
                                 (n     : Integer)
                                 (m     : Integer)
                                 (scale : Float))
     (build n (lam (j : Integer) (mkvec (add (mul j m) seed) m scale))))

(def main Integer ()
    (let ((D 64)
          (N 100)
          (K 64)
          (seed 0)
          (scale_unity 1.0)
          (scale_small 0.1)

          (x       (mkvecvec (add seed 0)    N D scale_unity))
          (alphas  (mkvec    (add seed 1000) K   scale_unity))
          (mus     (mkvecvec (add seed 2000) K D scale_unity))
          (qs      (mkvecvec (add seed 3000) K D scale_small))
          (ls      (mkvecvec (add seed 4000) K (gmm_knossos_tri D) scale_unity))
          (wishart (tuple 3.1 7))
        )

      (pr x
          (gmm_knossos_gmm_objective x alphas mus qs ls wishart)
          )))
