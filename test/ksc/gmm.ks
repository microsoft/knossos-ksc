; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def gmm_knossos_tri Integer ((n : Integer))
  (div (mul n (sub n 1)) 2))

(gdef fwd [gmm_knossos_tri Integer])
(gdef rev [gmm_knossos_tri Integer])
(gdef suffwdpass [gmm_knossos_tri Integer])
(gdef sufrevpass [gmm_knossos_tri Integer])
(gdef sufrev [gmm_knossos_tri Integer])

(def subIP (Tuple (Vec (Vec Float)) (Vec Float)) ((a : Vec (Vec Float)) (j : Integer) (b : Vec Float))
  (tuple a
  (build (size b) (lam (i : Integer) (sub (index i (index j a)) (index i b))))))

(def [suffwdpass [subIP (Tuple (Vec (Vec Float)) Integer (Vec Float))]] (Tuple (Tuple (Vec (Vec Float)) (Vec Float)) Integer)
     ((a : Vec (Vec Float)) (j : Integer) (b : Vec Float))
  (tuple (subIP a j b) j))

(edef subIP_helper
      (Vec (Vec Float))
      ((Vec (Vec Float)) (Vec Float) Integer))

(def [sufrevpass [subIP (Tuple (Vec (Vec Float)) Integer (Vec Float))]]
     (Tuple (Vec (Vec Float)) (Tuple) (Vec Float))
     ((t : Tuple (Vec (Vec Float)) (Vec Float)) (j : Integer))
     (let ((t1 t2) t)
     (let (t2r (build (size t2) (lam (i : Integer) (neg (index i t2)))))
     (let (t1r (subIP_helper t1 t2 j))
       (tuple t1r (tuple) t2r)))))

; dot
(edef dot Float ((Vec Float) (Vec Float)))
(edef [D dot] (LM (Tuple (Vec Float) (Vec Float)) Float)
             ((Vec Float) (Vec Float)))
(edef [Dt dot] (Tuple Float (LM (Tuple (Vec Float) (Vec Float)) Float))
              ((Vec Float) (Vec Float)))
(edef R$dot (LM Float (Tuple (Vec Float) (Vec Float))) ((Vec Float) (Vec Float)))
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

(def dot Float ((a : Vec (Vec Float)) (b : Vec (Vec Float)))
  (sum (build (size a) (lam (i : Integer) (dot (index i a) (index i b)))))
  )

(def sqnorm Float ((v : Vec Float))
  (dot v v))

(gdef fwd [sqnorm (Vec Float)])
(gdef rev [sqnorm (Vec Float)])
(gdef suffwdpass [sqnorm (Vec Float)])
(gdef sufrevpass [sqnorm (Vec Float)])
(gdef sufrev [sqnorm (Vec Float)])

(edef setAt (Tensor 2 Float) ((Tuple Integer Integer) (Tensor 2 Float) Float))

(edef [suffwdpass setAt] (Tuple (Tensor 2 Float) (Tuple Integer Integer))
      ((Tuple Integer Integer) (Tensor 2 Float) Float))

(edef [sufrevpass [setAt (Tuple (Tuple Integer Integer)
                                (Tensor 2 Float)
                                Float)]]
                  (Tuple (Tuple (Tuple) (Tuple)) (Tensor 2 Float) Float)
      ((Tensor 2 Float) (Tuple Integer Integer)))

(edef setAt (Tensor 1 Float) (Integer (Tensor 1 Float) Float))

(edef [suffwdpass setAt] (Tuple (Tensor 1 Float) Integer)
      (Integer (Tensor 1 Float) Float))

(edef [sufrevpass [setAt (Tuple Integer
                                (Tensor 1 Float)
                                Float)]]
                  (Tuple (Tuple) (Tensor 1 Float) Float)
      ((Tensor 1 Float) Integer))

(def gmm_knossos_makeQ (Tensor 2 Float) ((q : Vec Float) (l : Vec Float))
  (let ((D (size q))
        (triD (size l)))
  (assert (eq triD (gmm_knossos_tri D))
      (let (r (constVec (tuple D D) 0.0))
      (let ((r1 q1 l1 D1)
          (ifold (mul D D)
           (lam (s_k : (Tuple (Tuple (Tensor 2 Float) (Vec Float) (Vec Float) Integer) Integer))
           (let ((s k) s_k)
           (let ((ti qi li Di) s)
           (let (i (div k Di))
           (let (j (sub k (mul i Di)))   ;; Would prefer (mod k D) but I don't think we have mod
           (let (then (exp (index i qi)))
           (let (entry (if (lt i j)
                          0.0
                       (if (eq i j)
                          then
                       (index (add (sub (gmm_knossos_tri Di) (gmm_knossos_tri (sub Di j))) (sub (sub i j) 1)) li))))
           (let (tupdated (setAt (tuple i j) ti entry))
           (tuple tupdated qi li Di)
           ))))))))
           (tuple r q l D)))
        r1
        )))))

(gdef suffwdpass [gmm_knossos_makeQ (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [gmm_knossos_makeQ (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [gmm_knossos_makeQ (Tuple (Vec Float) (Vec Float))])

(def logsumexp Float ((v : Vec Float))
  (let (maxv (max v))
    (add maxv
         (log (sum (exp (sub v maxv)))))))

(gdef rev [logsumexp (Vec Float)])
(gdef fwd [logsumexp (Vec Float)])
(gdef suffwdpass [logsumexp (Vec Float)])
(gdef sufrevpass [logsumexp (Vec Float)])
(gdef sufrev [logsumexp (Vec Float)])

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
          ((slse K1 x1 alphas1 means1 qs1 ls1)
                          (ifold N
                          (lam (s_i : Tuple (Tuple Float
                                                   Integer
                                                   (Vec (Vec Float))
                                                   (Vec Float)
                                                   (Vec (Vec Float))
                                                   (Vec (Vec Float))
                                                   (Vec (Vec Float)))
                                            Integer)
                          (let ((s i) s_i)
                          (let ((acc Ki xsi alphasi meansi qsi lsi) s)
                          (let (initial_vec (constVec Ki 0.0))
                          (let ((thebuild qsk1 lsk1 xsk11 ik1 meansk1 alphask1) (ifold Ki
                            (lam (ss_k : Tuple (Tuple (Vec Float)
                                                      (Vec (Vec Float))
                                                      (Vec (Vec Float))
                                                      (Vec (Vec Float))
                                                      Integer
                                                      (Vec (Vec Float))
                                                      (Vec Float))
                                               Integer)
                            (let (((ss k) ss_k)
                                  ((veck qsk lsk xsk ik meansk alphask) ss)
                                  (Q         (gmm_knossos_makeQ (index k qsk) (index k lsk)))
                                  ((xsk1 thesub) (subIP xsk ik (index k meansk)))
                                  (mahal_vec (mul Q thesub))
                                  (r (sub (add (index k alphask) (sum (index k qsk)))
                                          (mul 0.500000  (sqnorm mahal_vec))))
                                  (veck1 (setAt k veck r)))
                              (tuple veck1 qsk lsk xsk1 ik meansk alphask)
                            ))
                            (tuple initial_vec qsi lsi xsi i meansi alphasi)
                            ))
                          (let (inc (logsumexp thebuild))
                           (tuple (add acc inc) Ki xsk11 alphask1 meansk1 qsk1 lsk1)
                          ))))))
                           (tuple 0.0 K x alphas means qs ls)
                           )
                    )
          )
            (add (add CONSTANT
                (sub slse
                  (mul (to_float N) (logsumexp alphas1))))
            (sum (build K (lam (k : Integer)
                    (log_wishart_prior wishart (index k qs1) (index k ls1))))))
    ))))

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

(def not_ Bool (p : Bool) (if p false true))

(def main Integer ()
    (let ((D 4)
          (N 5)
          (K 10)
          (seed 0)
          (scale_unity 1.0)
          (scale_small 0.1)


          (x       (mkvecvec (add seed 0)    N D scale_unity))
          (alphas  (mkvec    (add seed 1000) K   scale_unity))
          (mus     (mkvecvec (add seed 2000) K D scale_unity))
          (qs      (mkvecvec (add seed 3000) K D scale_small))
          (ls      (mkvecvec (add seed 4000) K (gmm_knossos_tri D) scale_unity))
          (wishart (tuple 3.1 7))

          (delta 0.0001)

          (dx       (mkvecvec (add seed 5000)  N D delta))
          (dalphas  (mkvec    (add seed 6000)  K   delta))
          (dmus     (mkvecvec (add seed 7000)  K D delta))
          (dqs      (mkvecvec (add seed 8000)  K D delta))
          (dls      (mkvecvec (add seed 9000)  K (gmm_knossos_tri D) delta))
          (dwishart (tuple (mkfloat (add seed 10000) delta) (tuple)))

          (dtheta   (tuple dx dalphas dmus dqs dls dwishart))

          (gmm_at_theta (gmm_knossos_gmm_objective x alphas mus qs ls wishart))
          (gmm_at_theta_plus_dtheta (gmm_knossos_gmm_objective (ts_add x dx) (ts_add alphas dalphas) (ts_add mus dmus) (ts_add qs dqs) (ts_add ls dls) (ts_add wishart dwishart)))

          (gmm_fd (sub gmm_at_theta_plus_dtheta gmm_at_theta))

          (golden_test_gmm_objective
           (let ((tolerance 0.000001)
                 (actual gmm_at_theta)
                 (expected 76.0882))
             (lt (abs (sub actual expected))
                (max (mul (abs expected) tolerance)
                     tolerance))))

          (df (sub gmm_at_theta_plus_dtheta gmm_at_theta))


          (checked_suf ($check (lam (t : Tuple (Vec (Vec Float))
                                           (Vec Float)
                                           (Vec (Vec Float))
                                           (Vec (Vec Float))
                                           (Vec (Vec Float))
                                           (Tuple Float Integer))
                                (gmm_knossos_gmm_objective t))
                           (lam (t : Tuple (Tuple (Vec (Vec Float))
                                                  (Vec Float)
                                                  (Vec (Vec Float))
                                                  (Vec (Vec Float))
                                                  (Vec (Vec Float))
                                                  (Tuple Float Integer))
                                           Float)
                                ([sufrev gmm_knossos_gmm_objective] t))
                    (tuple x  alphas  mus  qs  ls  wishart)
                    (tuple x  alphas  mus  qs  ls  wishart)
                    (tuple dx dalphas dmus dqs dls dwishart)
                    1.0))

          (tolerance 0.0001)
          (everything_works_as_expected_suf     (lt checked_suf tolerance))
        )
      (print x
          (gmm_knossos_makeQ (index 0 qs) (index 0 ls))
          "\n----\n" 
          (mul (gmm_knossos_makeQ (index 0 qs) (index 0 ls)) (index 0 x))
          ; see https://github.com/awf/knossos/issues/281 (D$gmm_knossos_gmm_objective x alphas mus qs ls wishart)

          "gmm_at_theta = " gmm_at_theta
          "\n----\n" 
          "gmm_at_theta_plus_dtheta = " gmm_at_theta_plus_dtheta
          "\n----\n" 
          "gmm_fd = " gmm_fd
          "\n----\n" 

          "dtheta = " dtheta
          "\n----\n" 

          "Checked SUF, should be small: " checked_suf
          "\n----\n" 

          "TESTS FOLLOW"
          "\n----\n" 
          "Golden test GMM objective\n"          golden_test_gmm_objective
          "\n----\n" 
          "Reverse SUF as expected\n"            everything_works_as_expected_suf
          )))
