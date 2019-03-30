(def gmm_knossos_tri Integer ((n : Integer))
  (/ (* n (- n 1)) 2))

(def exp$VecR (Vec Float) ((v : Vec Float))
  (build (size v) (lam (i : Integer) (exp (index i v)))))

(def mul$R$VecR (Vec Float) ((r : Float) (a : Vec Float))
    (build (size a) (lam (i : Integer) (* r (index i a)))))

(def mul$R$VecVecR (Vec (Vec Float)) ((r : Float) (a : Vec (Vec Float)))
    (build (size a) (lam (i : Integer) (mul$R$VecR r (index i a)))))

(def mul$VecR$VecR (Vec Float) ((a : Vec Float) (b : Vec Float))
  (assert (== (size a) (size b))
    (build (size a) (lam (i : Integer) (* (index i a) (index i b))))))

(def sub$VecR$VecR (Vec Float) ((a : Vec Float) (b : Vec Float))
  (assert (== (size a) (size b))
    (build (size a) (lam (i : Integer) (- (index i a) (index i b))))))

-- dotv
(edef dotv Float ((Vec Float) (Vec Float)))
(edef D$dotv (LM (Tuple (Vec Float) (Vec Float)) Float) 
            ((Vec Float) (Vec Float)))
(edef R$dotv (LM Float (Tuple (Vec Float) (Vec Float))) ((Vec Float) (Vec Float)))
(def fwd$dotv Float ((a : Vec Float) (b : Vec Float) (da : Vec Float) (db : Vec Float))
    (+ (dotv a db) (dotv da b)))
(def rev$dotv (Tuple (Vec Float) (Vec Float))
               ((a : Vec Float) (b : Vec Float) (dr : Float))
    (tuple (mul$R$VecR dr b) (mul$R$VecR dr a)))

(def dotvv Float ((a : Vec (Vec Float)) (b : Vec (Vec Float)))
  (sum (build (size a) (lam (i : Integer) (dotv (index i a) (index i b)))))
  )

(def sqnorm Float ((v : Vec Float))
  (dotv v v))

-- mul Mat Vec
(edef mul$Mat$Vec (Vec Float) ((Vec (Vec Float)) (Vec Float)))

(edef D$mul$Mat$Vec (LM (Tuple (Vec (Vec Float)) (Vec Float)) (Vec Float)) 
          ((Vec (Vec Float)) (Vec Float)))

(edef R$mul$Mat$Vec (LM (Vec Float) (Tuple (Vec (Vec Float)) (Vec Float))) 
          ((Vec (Vec Float)) (Vec Float)))

(def fwd$mul$Mat$Vec (Vec Float) 
          ((M : Vec (Vec Float)) (v : Vec Float) (dM : Vec (Vec Float)) (dv : Vec Float))
    (+ (mul$Mat$Vec dM v) (mul$Mat$Vec M dv))) 

(edef rev$mul$Mat$Vec (Tuple (Vec (Vec Float)) (Vec Float))
          ((Vec (Vec Float)) (Vec Float) (Vec Float)))

(def gmm_knossos_makeQ (Vec (Vec Float)) ((q : Vec Float) (l : Vec Float))
    (let (D
      (size q))
    (build D (lam (i : Integer)
        (build D (lam (j : Integer)
           (if (< i j)
            0.0
            (if (== i j)
              (exp (index i q))
              (index (+ (gmm_knossos_tri (- i 1)) j) l))
           )
           ))))))

(def logsumexp Float ((v : Vec Float))
    (log (sum (exp$VecR v))))


(def gmm_knossos_gmm_objective Float
      ((x : Vec (Vec Float))
       (alphas : Vec Float)
       (means : Vec (Vec Float))
       (qs : Vec (Vec Float))
       (ls : Vec (Vec Float))
       (wishart_gamma : Float))
  (let (N (size x))
  (let (D (size (index 0 x)))
  (let (K (size alphas))
        (+ (- (sum (build N (lam (i : Integer)
              (logsumexp (build K (lam (k : Integer)
                (let ((Q         (gmm_knossos_makeQ (index k qs) (index k ls)))
                      (mahal_vec (mul$Mat$Vec Q
                                          (sub$VecR$VecR (index i x) (index k means)))))
                  (- (+ (index k alphas) (sum (index k qs)))
                    (* 0.500000  (sqnorm mahal_vec))
                    ))))))))
            (* (to_float N) (logsumexp alphas)))
         (* 0.5 (sum (build K (lam (k : Integer)
                                            (+ (sqnorm (exp$VecR (index k qs)))
                                                (sqnorm (index k ls))))))))))))

(def mkvec (Vec Float) ((N : Integer) (scale : Float))
    (build N (lam (j : Integer) ($rand scale))))

(def zerov (Vec Float) ((x : Vec Float))
  (mul$R$VecR 0.0 x))

(def zerovv (Vec (Vec Float)) ((x : Vec (Vec Float)))
  (mul$R$VecVecR 0.0 x))

(def mkdeltav (Vec Float)
              ((n : Integer)
               (i : Integer)
               (val : Float))
    (build n (lam (ii : Integer)
                (if (== i ii)
                    val
                    0.0))))

(def mkdeltavv (Vec (Vec Float))
               ((x : Vec (Vec Float))
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

(def main Integer ()
    (let ((D 64)
          (N 100)
          (K 64)

          (x       (build N  (lam (i : Integer) (mkvec D 1.0))))
          (alphas  (build K (lam (i : Integer) ($rand 1.0))))
          (mus     (build K (lam (i : Integer) (mkvec D 1.0))))
          (qs      (build K (lam (i : Integer) (mkvec D 0.1))))
          (ls      (build K (lam (i : Integer) (mkvec (gmm_knossos_tri D) 1.0))))
          (gamma   1.5)
        )

      (pr x
          (gmm_knossos_gmm_objective x alphas mus qs ls gamma)
          )))
