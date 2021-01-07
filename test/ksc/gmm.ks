; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def gmm_knossos_tri Integer ((n : Integer))
  (div (mul n (sub n 1)) 2))

(def exp$VecR (Vec Float) ((v : Vec Float))
  (build (size v) (lam (i : Integer) (exp (index i v)))))

(def mul$R$VecR (Vec Float) ((r : Float) (a : Vec Float))
    (build (size a) (lam (i : Integer) (mul r (index i a)))))

(def sub$VecR$VecR (Vec Float) ((a : Vec Float) (b : Vec Float))
  (build (size a) (lam (i : Integer) (sub (index i a) (index i b)))))

; dot
(edef dot Float ((Vec Float) (Vec Float)))
(edef D$dot (LM (Tuple (Vec Float) (Vec Float)) Float)
             ((Vec Float) (Vec Float)))
(edef Dt$dot (Tuple Float (LM (Tuple (Vec Float) (Vec Float)) Float))
              ((Vec Float) (Vec Float)))
(edef R$dot (LM Float (Tuple (Vec Float) (Vec Float))) ((Vec Float) (Vec Float)))
(def fwd$dot Float ((a_b : (Tuple (Vec Float) (Vec Float))) (da_db : (Tuple (Vec Float) (Vec Float))))
     (let ((a  (get$1$2 a_b))
           (b  (get$2$2 a_b))
           (da (get$1$2 da_db))
           (db (get$2$2 da_db)))
    (add (dot a db) (dot da b))))
(def rev$dot (Tuple (Vec Float) (Vec Float))
               ((a_b : (Tuple (Vec Float) (Vec Float))) (dr : Float))
     (let ((a  (get$1$2 a_b))
           (b  (get$2$2 a_b)))
    (tuple (mul$R$VecR dr b) (mul$R$VecR dr a))))

(def dot Float ((a : Vec (Vec Float)) (b : Vec (Vec Float)))
  (sum (build (size a) (lam (i : Integer) (dot (index i a) (index i b)))))
  )

(def sqnorm Float ((v : Vec Float))
  (dot v v))

(def gmm_knossos_makeQ (Tensor 2 Float) ((q : Vec Float) (l : Vec Float))
  (let ((D (size q))
        (triD (size l)))
  (assert (eq triD (gmm_knossos_tri D))
    (build (tuple D D) (lam (ij : (Tuple Integer Integer))
        (let ((i j) ij)
           (if (lt i j)
            0.0
            (if (eq i j)
              (exp (index i q))
              (index (add (sub (gmm_knossos_tri D) (gmm_knossos_tri (sub D j))) (sub (sub i j) 1)) l))
           )
           ))))))

(def logsumexp Float ((v : Vec Float))
    (log (sum (exp$VecR v))))

; wishart_m -> int
(def log_gamma_distrib Float ((a : Float) (p : Integer))
    (let ((out (mul 0.28618247146235004 (to_float (mul p (sub p 1)))))) ; 0.25 log pi
      (add out
         (sum (build p (lam (j : Integer)
                 (lgamma (sub a (mul 0.5 (to_float j))))))))))

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
                                  (mahal_vec (mul Q (sub$VecR$VecR (index i x) (index k means)))))
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
          (gmm_fwd (fwd$gmm_knossos_gmm_objective
                    (tuple x  alphas  mus  qs  ls  wishart)
                    (tuple dx dalphas dmus dqs dls dwishart)))

          (golden_test_gmm_objective
           (let ((tolerance 0.000001)
                 (actual gmm_at_theta)
                 (expected 76.0882))
             (lt (abs (sub actual expected))
                (max (mul (abs expected) tolerance)
                     tolerance))))

          (everything_works_as_expected
           ; I would like to pull out a function called something
           ; like `floating_point_numbers_are_close` but I would have
           ; to implement the derivtives of abs and max for that, and
           ; I can't be bothered right now.
           (let ((tolerance 0.001)
                 (actual gmm_fd)
                 (expected gmm_fwd))
             (lt (abs (sub actual expected))
                (max (mul (abs expected) tolerance)
                     tolerance))))
          (impossibly_good (eq gmm_fd gmm_fwd))

          ; Check <grad_f, dx> = f(x+dx) - f(x)
          ; with grad_f = f`(x, 1.0)
          (grad_gmm (rev$gmm_knossos_gmm_objective (tuple x alphas mus qs ls wishart) 1.0))
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
                                (rev$gmm_knossos_gmm_objective t))
                    (tuple x  alphas  mus  qs  ls  wishart)
                    (tuple x  alphas  mus  qs  ls  wishart)
                    (tuple dx dalphas dmus dqs dls dwishart)
                    1.0))

          (tolerance 0.0001)
          (everything_works_as_expected_reverse (lt checked tolerance))
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
          "gmm_fwd = " gmm_fwd
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
          "TESTS FOLLOW"
          "\n----\n" 
          "Golden test GMM objective\n"          golden_test_gmm_objective
          "\n----\n" 
          "Reverse mode as expected\n"           everything_works_as_expected_reverse
          "\n----\n" 
          "Forward mode as expected\n"           everything_works_as_expected
          "\n----\n" 
          "Not impossibly good\n"                (not_ impossibly_good)
          )))
