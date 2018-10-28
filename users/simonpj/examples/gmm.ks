(def gmm_knossos_tri ((const n))
  (/ (* n (- n 1)) 2))

(def gmm_knossos_makeQ (q l)
    (let (d
      (size q))
    (build d (lam i
        (build d (lam j
          (if (< i j)
            0.000000
            (if (== i j)
              (exp (index q i))
              (index l (add$int$int (gmm_knossos_tri (- i 1)) j))))))))))

(def exp$Vec (v)
    (build (size v) (lam i (exp (index v i)))))

(def mul$Vec$Vec (v1 v2)
  (assert (== (size v1) (size v2))
    (build (size v1) (lam i (* (index v1 i) (index v2 i))))))

(def linalg_vectorSum (v1 v2)
  (assert (== (size v1) (size v2))
    (build (size v1) (lam i (+ (index v1 i) (index v2 i))))))

(def linalg_vectorSub (v1 v2)
  (assert (== (size v1) (size v2))
    (build (size v1) (lam i (- (index v1 i) (index v2 i))))))

(def mul$Vec$R (v r)
  (build (size v) (lam i (* (index v i) r))))

-- Mat is Vec of columns
(def linalg_matrixVectorMult (m v)
  (let (cols (size m))
    (let (rows (size (index m 0)))
      (assert (== cols (size v))
        (sum (build (cols) (lam i (mul$Vec$R (index m i) (index v i)))))))))

(def gmm_knossos_logsumexp (v)
    (log (sum (exp$Vec v))))

(def sqnorm (v)
    (sum (mul$Vec v v)))

(def gmm_knossos_gmm_objective (x alphas means qs ls wishart_gamma wishart_m)
  (let (n
    (size x))
  (let (d
      (size (index x 0)))
    (let (K
        (size alphas))
      (+ (- (linalg_vectorSum (build n (lam  i
          (gmm_knossos_logsumexp (build K (lam  k
            (let (mahal_vec
              (linalg_matrixVectorMult (gmm_knossos_makeQ (index qs k) (index ls k)) (linalg_vectorSub (index x i) (index means k))))
            (- (+ (index alphas k) (linalg_vectorSum (index qs k))) (* 0.500000 (linalg_sqnorm mahal_vec)))))))))) (* (double (cardToInt n)) (gmm_knossos_logsumexp alphas))) (* 0.500000 (linalg_vectorSum (build K (lam  k
          (+ (linalg_sqnorm (linalg_vectorMap (lam  value
            (exp value)) (index qs k))) (linalg_sqnorm (index ls k))))))))))))

(def ks_main ()
  (let (x (build 10 (lam i (build 3 (lam j 2.0)))))
    (let (alphas (build 10 (lam i 7.0)))
      (gmm_knossos_gmm_objective x alphas x x x 1.3 1.2))))
