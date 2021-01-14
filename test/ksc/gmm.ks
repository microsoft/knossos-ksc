; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def gmm_knossos_tri Integer ((n : Integer))
  (div (mul n (sub n 1)) 2))

(gdef fwd [gmm_knossos_tri Integer])
(gdef rev [gmm_knossos_tri Integer])
(gdef suffwdpass [gmm_knossos_tri Integer])
(gdef sufrevpass [gmm_knossos_tri Integer])
(gdef sufrev [gmm_knossos_tri Integer])

(def sub (Tuple (Vec (Vec Float)) (Vec Float)) ((a : Vec (Vec Float)) (j : Integer) (b : Vec Float))
  (tuple a
  (build (size b) (lam (i : Integer) (sub (index i (index j a)) (index i b))))))

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

(edef suffwdpass$setAt (Tuple (Tensor 2 Float) (Tuple Integer Integer))
      ((Tuple Integer Integer) (Tensor 2 Float) Float))

(edef sufrevpass$setAt (Tuple (Tuple (Tuple) (Tuple)) (Tensor 2 Float) Float)
      ((Tensor 2 Float) (Tuple Integer Integer)))

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
                          (let ((acc Ki xi alphasi meansi qsi lsi) s)
                          (let (inc (logsumexp (build Ki (lam (k : Integer)
                            (let ((Q         (gmm_knossos_makeQ (index k qsi) (index k lsi)))
                                  ((xi1 thesub) (sub xi i (index k meansi)))
                                  (mahal_vec (mul Q thesub)))
                              (sub (add (index k alphasi)
                                    ; (index k sum_qs)
                                    (sum (index k qsi))
                              )
                                (mul 0.500000  (sqnorm mahal_vec)))
                            )))))
                           (tuple (add acc inc) Ki xi alphasi meansi qsi lsi)
                          ))))
                           (tuple 0.0 K x alphas means qs ls)
                           )
                    )
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

(gdef shape [rev [gmm_knossos_gmm_objective
      (Tuple (Vec (Vec Float))
             (Vec Float)
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Vec (Vec Float))
             (Tuple Float Integer))]])

(gdef CL [gmm_knossos_gmm_objective
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
          (gmm_at_theta_CL ([CL gmm_knossos_gmm_objective] x alphas mus qs ls wishart))

          (gmm_fd (sub gmm_at_theta_plus_dtheta gmm_at_theta))
          (gmm_fwd ([fwd gmm_knossos_gmm_objective]
                    (tuple x  alphas  mus  qs  ls  wishart)
                    (tuple dx dalphas dmus dqs dls dwishart)))

          (golden_test_gmm_objective
           (let ((tolerance 0.000001)
                 (actual gmm_at_theta)
                 (expected 76.0882))
             (lt (abs (sub actual expected))
                (max (mul (abs expected) tolerance)
                     tolerance))))

          ;; (everything_works_as_expected
          ;;  ; I would like to pull out a function called something
          ;;  ; like `floating_point_numbers_are_close` but I would have
          ;;  ; to implement the derivtives of abs and max for that, and
          ;;  ; I can't be bothered right now.
          ;;  (let ((tolerance 0.001)
          ;;        (actual gmm_fd)
          ;;        (expected gmm_fwd))
          ;;    (lt (abs (sub actual expected))
          ;;       (max (mul (abs expected) tolerance)
          ;;            tolerance))))
          ;; (impossibly_good (eq gmm_fd gmm_fwd))

          ; Check <grad_f, dx> = f(x+dx) - f(x)
          ; with grad_f = f`(x, 1.0)
          (grad_gmm ([rev gmm_knossos_gmm_objective] (tuple x alphas mus qs ls wishart) 1.0))
          (grad_gmm_x          (get$1$6 grad_gmm))
          (grad_gmm_alphas     (get$2$6 grad_gmm))
          (grad_gmm_mus        (get$3$6 grad_gmm))
          (grad_gmm_qs         (get$4$6 grad_gmm))
          (grad_gmm_ls         (get$5$6 grad_gmm))
          (grad_gmm_wishart    (get$6$6 grad_gmm))

          (dot_at_x          (dot grad_gmm_x dx))
          (dot_at_alphas     (dot grad_gmm_alphas dalphas))
          (dot_at_mus        (dot grad_gmm_mus dmus))
          (dot_at_qs         (dot grad_gmm_qs dqs))
          (dot_at_ls         (dot grad_gmm_ls dls))
          (dot_at_wishart    (mul (get$1$2 grad_gmm_wishart) (get$1$2 dwishart)))

          (grad_gmm_dot_dtheta (add      dot_at_x
                                (add     dot_at_alphas
                                 (add    dot_at_mus
                                  (add   dot_at_qs
                                   (add  dot_at_ls
                                       dot_at_wishart))))))

          (df (sub gmm_at_theta_plus_dtheta gmm_at_theta))
          (rev_ok (tuple grad_gmm_dot_dtheta " ==?== " df))


          (checked ($check (lam (t : Tuple (Vec (Vec Float))
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
                                ([rev gmm_knossos_gmm_objective] t))
                    (tuple x  alphas  mus  qs  ls  wishart)
                    (tuple x  alphas  mus  qs  ls  wishart)
                    (tuple dx dalphas dmus dqs dls dwishart)
                    1.0))

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

          (rev_gmm_at_theta_shape ([shape [rev gmm_knossos_gmm_objective]] (tuple x alphas mus qs ls wishart) 1.0))

          (tolerance 0.0001)
          (everything_works_as_expected_reverse (lt checked tolerance))
          (everything_works_as_expected_suf     (lt checked_suf tolerance))
        )
      (print x
          (gmm_knossos_makeQ (index 0 qs) (index 0 ls))

          "\n----\n" 
          (mul (gmm_knossos_makeQ (index 0 qs) (index 0 ls)) (index 0 x))
          ; see https://github.com/awf/knossos/issues/281 (D$gmm_knossos_gmm_objective x alphas mus qs ls wishart)

          "gmm_at_theta = " gmm_at_theta
          "\n----\n" 
          "gmm_at_theta_CL = " gmm_at_theta_CL
          "\n----\n"
          "gmm_at_theta_shape = " rev_gmm_at_theta_shape
          "\n----\n"
          "gmm_at_theta_plus_dtheta = " gmm_at_theta_plus_dtheta
          "\n----\n" 
;          "gmm_fwd = " gmm_fwd
          "\n----\n" 
          "gmm_fd = " gmm_fd
          "\n----\n" 

          "grad_gmm = " grad_gmm
          "\n----\n" 
          "dtheta = " dtheta
          "\n----\n" 
          "rev_ok = " rev_ok
          "\n----\n" 

          "Checked, should be small: " checked
          "\n----\n" 
          "Checked SUF, should be small: " checked_suf
          "\n----\n" 

          "TESTS FOLLOW"
          "\n----\n" 
          "Golden test GMM objective\n"          golden_test_gmm_objective
          "\n----\n" 
          "Reverse mode as expected\n"           everything_works_as_expected_reverse
          "\n----\n" 
          "Reverse SUF as expected\n"            everything_works_as_expected_suf
          "\n----\n" 
          "Forward mode as expected\n"           everything_works_as_expected
          "\n----\n" 
          "Not impossibly good\n"                (not_ impossibly_good)
          )))
