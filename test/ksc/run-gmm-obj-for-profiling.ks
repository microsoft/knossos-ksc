; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def gmm_knossos_tri Integer ((n : Integer))
  (div (mul n (sub n 1)) 2))

(gdef fwd [gmm_knossos_tri Integer])
(gdef rev [gmm_knossos_tri Integer])
(gdef suffwdpass [gmm_knossos_tri Integer])
(gdef sufrevpass [gmm_knossos_tri Integer])
(gdef sufrev [gmm_knossos_tri Integer])

; dot
(edef dot Float (Tuple (Vec Float) (Vec Float)))
(edef [D dot] (LM (Tuple (Vec Float) (Vec Float)) Float)
             (Tuple (Vec Float) (Vec Float)))
(edef R$dot (LM Float (Tuple (Vec Float) (Vec Float))) (Tuple (Vec Float) (Vec Float)))
(def [fwd dot] Float ((a_b : (Tuple (Vec Float) (Vec Float))) (da_db : (Tuple (Vec Float) (Vec Float))))
     (let ((a b) a_b)
       (let ((da db) da_db)
         (add (dot a db) (dot da b)))))
(def [rev dot] (Tuple (Vec Float) (Vec Float))
               ((a_b : (Tuple (Vec Float) (Vec Float))) (dr : Float))
     (let ((a b) a_b)
       (tuple (mul dr b) (mul dr a))))

(def [suffwdpass dot] (Tuple Float (Tuple (Vec Float) (Vec Float)))
     ((a : Vec Float) (b : Vec Float))
     (tuple (dot a b) (tuple a b)))

(def [sufrevpass [dot (Tuple (Vec Float) (Vec Float))]]
     (Tuple (Vec Float) (Vec Float))
     ((d_ddot : Float) (a_b : Tuple (Vec Float) (Vec Float)))
     ([rev dot] a_b d_ddot))

(def sqnorm Float ((v : Vec Float))
  (dot v v))

(gdef fwd [sqnorm (Vec Float)])
(gdef rev [sqnorm (Vec Float)])
(gdef suffwdpass [sqnorm (Vec Float)])
(gdef sufrevpass [sqnorm (Vec Float)])
(gdef sufrev [sqnorm (Vec Float)])

(def gmm_knossos_makeQ (Tensor 2 Float) ((q : Vec Float) (l : Vec Float))
 (let (D (size q))
   (assert (eq (size l) (gmm_knossos_tri D))
    (build (tuple D D) (lam (ij : Tuple Integer Integer)
        (let ((i j) ij)
           (if (lt i j)
            0.0
            (if (eq i j)
              (exp (index i q))
              (index (add (sub (gmm_knossos_tri D) (gmm_knossos_tri (sub D j))) (sub (sub i j) 1)) l))
           )
           ))))))

(gdef fwd [gmm_knossos_makeQ (Tuple (Vec Float) (Vec Float))])
(gdef rev [gmm_knossos_makeQ (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [gmm_knossos_makeQ (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [gmm_knossos_makeQ (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [gmm_knossos_makeQ (Tuple (Vec Float) (Vec Float))])

(def logsumexp Float ((v : Vec Float))
  (let (maxv (max v))
    (add maxv
         (log (sum (exp (sub v maxv)))))))

(gdef rev [logsumexp (Vec Float)])
(gdef suffwdpass [logsumexp (Vec Float)])
(gdef sufrevpass [logsumexp (Vec Float)])
(gdef sufrev [logsumexp (Vec Float)])
(gdef fwd [logsumexp (Vec Float)])


; wishart_m -> int
(def log_gamma_distrib Float ((a : Float) (p : Integer))
    (let ((out (mul 0.28618247146235004 (to_float (mul p (sub p 1)))))) ; 0.25 log pi
      (add out
         (sum (build p (lam (j : Integer)
                 (lgamma (sub a (mul 0.5 (to_float j))))))))))

(gdef fwd [log_gamma_distrib (Tuple Float Integer)])
(gdef rev [log_gamma_distrib (Tuple Float Integer)])
(gdef suffwdpass [log_gamma_distrib (Tuple Float Integer)])
(gdef sufrevpass [log_gamma_distrib (Tuple Float Integer)])
(gdef sufrev [log_gamma_distrib (Tuple Float Integer)])

(def log_wishart_prior Float ((wishart : Tuple Float Integer)
                              (log_Qdiag : Vec Float)
                              (ltri_Q : Vec Float))
    (let (
          (p (size log_Qdiag))
          ((wishart_gamma wishart_m)
                         wishart)
          (sum_qs        (sum log_Qdiag))
          (Qdiag         (exp log_Qdiag))

          (n  (add p (add wishart_m 1)))
          (C  (sub (mul (to_float (mul n p))
                    (sub (log wishart_gamma)
                       (mul 0.5 (log 2.0))))
                 (log_gamma_distrib (mul 0.5 (to_float n)) p)))
          (frobenius (add  (sqnorm Qdiag) (sqnorm ltri_Q)))
          (w2f   (mul 0.5 (mul (mul wishart_gamma wishart_gamma) frobenius)))
          )
        (sub (sub w2f
              (mul (to_float wishart_m)
                  sum_qs))
            C)
    ))

(gdef fwd [log_wishart_prior (Tuple (Tuple Float Integer) (Vec Float) (Vec Float))])
(gdef rev [log_wishart_prior (Tuple (Tuple Float Integer) (Vec Float) (Vec Float))])
(gdef suffwdpass [log_wishart_prior (Tuple (Tuple Float Integer) (Vec Float) (Vec Float))])
(gdef sufrevpass [log_wishart_prior (Tuple (Tuple Float Integer) (Vec Float) (Vec Float))])
(gdef sufrev [log_wishart_prior (Tuple (Tuple Float Integer) (Vec Float) (Vec Float))])

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
                                  (mahal_vec (mul Q (sub (index i x) (index k means)))))
                              (sub (add (index k alphas)
                                    ; (index k sum_qs)
                                    (sum (index k qs))
                              )
                                (mul 0.500000  (sqnorm mahal_vec)))
                            ))))
                          ))))
          )
            (add (add CONSTANT
                (sub slse
                  (mul (to_float N) (logsumexp alphas))))
            (sum (build K (lam (k : Integer)
                    (log_wishart_prior wishart (index k qs) (index k ls))))))
    ))))

(gdef fwd [gmm_knossos_gmm_objective
      (Tuple (Vec (Vec Float))
             (Vec Float)
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Tuple Float Integer))])

(gdef rev [gmm_knossos_gmm_objective
      (Tuple (Vec (Vec Float))
             (Vec Float)
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Tuple Float Integer))])
(gdef suffwdpass [gmm_knossos_gmm_objective
      (Tuple (Vec (Vec Float))
             (Vec Float)
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Tuple Float Integer))])
(gdef sufrevpass [gmm_knossos_gmm_objective
      (Tuple (Vec (Vec Float))
             (Vec Float)
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Tuple Float Integer))])
(gdef sufrev [gmm_knossos_gmm_objective
      (Tuple (Vec (Vec Float))
             (Vec Float)
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Tuple Float Integer))])

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

      (print x
          (gmm_knossos_gmm_objective x alphas mus qs ls wishart)
          )))
