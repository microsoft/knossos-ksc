(def gmm_knossos_tri ((n : Integer))
  (/ (* n (- n 1)) 2))

(def exp$VecR ((v : Vec Float))
  (build (size v) (lam (i : Integer) (exp (index i v)))))

(def mul$R$VecR ((r : Float) (a : Vec Float))
    (build (size a) (lam (i : Integer) (* r (index i a)))))

(def mul$R$VecVecR ((r : Float) (a : Vec Vec Float))
    (build (size a) (lam (i : Integer) (mul$R$VecR r (index i a)))))

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
       (alphas : Vec Float)
       (means : Vec Vec Float)
       (qs : Vec Vec Float)
       (ls : Vec Vec Float)
       (wishart_gamma : Float)
       (wishart_m : Float))
  (let (n (size x))
  (let (d (size (index 0 x)))
  (let (K (size alphas))
        (+ (- (sum (build n (lam (i : Integer)
              (logsumexp (build K (lam (k : Integer)
                (let ((Q (gmm_knossos_makeQ (index k qs) (index k ls)))
                      (mahal_vec (mul$Mat$Vec Q
                                          (sub$VecR$VecR (index i x) (index k means)))))
                  (- (+ (index k alphas) (sum (index k qs)))
                    (* wishart_gamma 1.0) -- should be (* 0.500000  (sqnorm mahal_vec))
                    ))))))))
            (* (to_float n) (logsumexp alphas)))
         (* 0.5 (sum (build K (lam (k : Integer)
                                            (+ (sqnorm (exp$VecR (index k qs)))
                                                (sqnorm (index k ls))))))))))))

(def mkvec ((n : Integer))
    (build n (lam (j : Integer) ($rand 1.0))))

(def zerov ((x : Vec Float))
  (mul$R$VecR 0.0 x))

(def zerovv ((x : Vec Vec Float))
  (mul$R$VecVecR 0.0 x))

(def mkdeltav ((n : Integer)
               (i : Integer)
               (val : Float))
    (build n (lam (ii : Integer)
                (if (== i ii)
                    val
                    0.0))))

(def mkdeltavv ((x : Vec Vec Float)
                (i : Integer)
                (j : Integer)
                (val : Float))
    (build (size x) (lam (ii : Integer)
        (build (size (index ii x)) (lam (jj : Integer)
            (if (== i ii)
                (if (== j jj)
                    val
                    0.0)
                0.0))))))

(def main ()
  (let ((d 4)
        (x (build 18 (lam (i : Integer) (mkvec d)))))
    (let ((alphas  (build 10 (lam (i : Integer) ($rand 1.0))))
          (mus     (build 10 (lam (i : Integer) (mkvec d))))
          (qs      (build 10 (lam (i : Integer) (mkvec d))))
          (ls      (build 10 (lam (i : Integer) (mkvec (gmm_knossos_tri d)))))
          (gamma 3.5)

          (delta 0.001)
          (del (mkdeltavv qs 2 1 delta))
        )
      (pr x
          (gmm_knossos_makeQ (index 0 qs) (index 0 ls))
          (mul$Mat$Vec (gmm_knossos_makeQ (index 0 qs) (index 0 ls)) (index 0 x))
          (gmm_knossos_gmm_objective x alphas mus qs ls gamma 1.2)
          del
          (D$gmm_knossos_gmm_objective x alphas mus qs ls gamma 1.2)

          (gmm_knossos_gmm_objective x alphas mus qs ls gamma 1.2 )
          (gmm_knossos_gmm_objective x alphas mus (+ qs del) ls gamma 1.2)
          (fwd$gmm_knossos_gmm_objective 
                x          alphas          mus           qs      ls            gamma    1.2
                (zerovv x) (zerov alphas)  (zerovv mus)  del     (zerovv ls)   0.0      0.0)
          (- (gmm_knossos_gmm_objective x alphas mus (+ qs del) ls gamma 1.2)
             (gmm_knossos_gmm_objective x alphas mus qs ls gamma 1.2))

          (rev$gmm_knossos_gmm_objective 
            x alphas mus qs ls gamma 1.2 1.0)
          ))))

