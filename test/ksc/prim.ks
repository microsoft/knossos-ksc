(def index_VecFloat Float ((i : Integer) (v : (Vec Float)))
    (index i v))

(gdef fwd [index_VecFloat (Tuple Integer (Vec Float))])
(gdef rev [index_VecFloat (Tuple Integer (Vec Float))])
(gdef suffwdpass [index_VecFloat (Tuple Integer (Vec Float))])
(gdef sufrevpass [index_VecFloat (Tuple Integer (Vec Float))])
(gdef sufrev [index_VecFloat (Tuple Integer (Vec Float))])

(def index_VecVecFloat (Vec Float) ((i : Integer) (v : (Vec (Vec Float))))
    (index i v))

(gdef fwd [index_VecVecFloat (Tuple Integer (Vec (Vec Float)))])
(gdef rev [index_VecVecFloat (Tuple Integer (Vec (Vec Float)))])
(gdef suffwdpass [index_VecVecFloat (Tuple Integer (Vec (Vec Float)))])
(gdef sufrevpass [index_VecVecFloat (Tuple Integer (Vec (Vec Float)))])
(gdef sufrev [index_VecVecFloat (Tuple Integer (Vec (Vec Float)))])

(def constVec_Float (Vec Float) ((n : Integer) (v : Float))
    (constVec n v))

(gdef fwd [constVec_Float (Tuple Integer Float)])
(gdef rev [constVec_Float (Tuple Integer Float)])
(gdef suffwdpass [constVec_Float (Tuple Integer Float)])
(gdef sufrevpass [constVec_Float (Tuple Integer Float)])
(gdef sufrev [constVec_Float (Tuple Integer Float)])

(def constVec_VecFloat (Vec (Vec Float)) ((n : Integer) (v : (Vec Float)))
    (constVec n v))

(gdef fwd [constVec_VecFloat (Tuple Integer (Vec Float))])
(gdef rev [constVec_VecFloat (Tuple Integer (Vec Float))])
(gdef suffwdpass [constVec_VecFloat (Tuple Integer (Vec Float))])
(gdef sufrevpass [constVec_VecFloat (Tuple Integer (Vec Float))])
(gdef sufrev [constVec_VecFloat (Tuple Integer (Vec Float))])

(def constVec_Tensor2Float (Tensor 2 Float) ((n : (Tuple Integer Integer)) (v : Float))
    (constVec n v))

(gdef fwd [constVec_Tensor2Float (Tuple (Tuple Integer Integer) Float)])
(gdef rev [constVec_Tensor2Float (Tuple (Tuple Integer Integer) Float)])
(gdef suffwdpass [constVec_Tensor2Float (Tuple (Tuple Integer Integer) Float)])
(gdef sufrevpass [constVec_Tensor2Float (Tuple (Tuple Integer Integer) Float)])
(gdef sufrev [constVec_Tensor2Float (Tuple (Tuple Integer Integer) Float)])


(def build_constVec_Float (Vec Float) ((n : Integer) (v : Float))
    (build n (lam (i : Integer) v)))

(gdef fwd [build_constVec_Float (Tuple Integer Float)])
(gdef rev [build_constVec_Float (Tuple Integer Float)])
(gdef suffwdpass [build_constVec_Float (Tuple Integer Float)])
(gdef sufrevpass [build_constVec_Float (Tuple Integer Float)])
(gdef sufrev [build_constVec_Float (Tuple Integer Float)])

(def build_constVec_VecFloat (Vec (Vec Float)) ((n : Integer) (v : (Vec Float)))
    (build n (lam (i : Integer) v)))
    
(gdef fwd [build_constVec_VecFloat (Tuple Integer (Vec Float))])
(gdef rev [build_constVec_VecFloat (Tuple Integer (Vec Float))])
(gdef suffwdpass [build_constVec_VecFloat (Tuple Integer (Vec Float))])
(gdef sufrevpass [build_constVec_VecFloat (Tuple Integer (Vec Float))])
(gdef sufrev [build_constVec_VecFloat (Tuple Integer (Vec Float))])

(def deltaVec_Float (Vec Float) ((n : Integer) (i : Integer) (v : Float))
    (deltaVec n i v))

(gdef fwd [deltaVec_Float (Tuple Integer Integer Float)])
(gdef rev [deltaVec_Float (Tuple Integer Integer Float)])
(gdef suffwdpass [deltaVec_Float (Tuple Integer Integer Float)])
(gdef sufrevpass [deltaVec_Float (Tuple Integer Integer Float)])
(gdef sufrev [deltaVec_Float (Tuple Integer Integer Float)])

(def deltaVec_VecFloat (Vec (Vec Float)) ((n : Integer) (i : Integer) (v : (Vec Float)))
    (deltaVec n i v))

(gdef fwd [deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])
(gdef rev [deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])
(gdef suffwdpass [deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])
(gdef sufrevpass [deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])
(gdef sufrev [deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])

(def deltaVec_Tensor2Float (Tensor 2 Float) ((n : (Tuple Integer Integer)) (i : (Tuple Integer Integer)) (v : Float))
    (deltaVec n i v))

(gdef fwd [deltaVec_Tensor2Float (Tuple (Tuple Integer Integer) (Tuple Integer Integer) Float)])
(gdef rev [deltaVec_Tensor2Float (Tuple (Tuple Integer Integer) (Tuple Integer Integer) Float)])
(gdef suffwdpass [deltaVec_Tensor2Float (Tuple (Tuple Integer Integer) (Tuple Integer Integer) Float)])
(gdef sufrevpass [deltaVec_Tensor2Float (Tuple (Tuple Integer Integer) (Tuple Integer Integer) Float)])
(gdef sufrev [deltaVec_Tensor2Float (Tuple (Tuple Integer Integer) (Tuple Integer Integer) Float)])

(def build_deltaVec_Float (Vec Float) ((n : Integer) (i : Integer) (v : Float))
    (build n (lam (k : Integer) (if (eq i k) v 0.0))))

(gdef fwd [build_deltaVec_Float (Tuple Integer Integer Float)])
(gdef rev [build_deltaVec_Float (Tuple Integer Integer Float)])
(gdef suffwdpass [build_deltaVec_Float (Tuple Integer Integer Float)])
(gdef sufrevpass [build_deltaVec_Float (Tuple Integer Integer Float)])
(gdef sufrev [build_deltaVec_Float (Tuple Integer Integer Float)])

(def build_deltaVec_VecFloat (Vec (Vec Float)) ((n : Integer) (i : Integer) (v : (Vec Float)))
    (let (zerov (build (size v) (lam (l : Integer) 0.0)))
        (build n (lam (k : Integer) (if (eq i k) v zerov)))))

(gdef fwd [build_deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])
(gdef rev [build_deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])
(gdef suffwdpass [build_deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])
(gdef sufrevpass [build_deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])
(gdef sufrev [build_deltaVec_VecFloat (Tuple Integer Integer (Vec Float))])

(def sum_VecFloat Float (v : (Vec Float))
   (sum v))

(gdef fwd [sum_VecFloat (Vec Float)])
(gdef rev [sum_VecFloat (Vec Float)])
(gdef suffwdpass [sum_VecFloat (Vec Float)])
(gdef sufrevpass [sum_VecFloat (Vec Float)])
(gdef sufrev [sum_VecFloat (Vec Float)])

(def sum_Tensor2Float Float (v : (Tensor 2 Float))
   (sum v))

(gdef fwd [sum_Tensor2Float (Tensor 2 Float)])
(gdef rev [sum_Tensor2Float (Tensor 2 Float)])
(gdef suffwdpass [sum_Tensor2Float (Tensor 2 Float)])
(gdef sufrevpass [sum_Tensor2Float (Tensor 2 Float)])
(gdef sufrev [sum_Tensor2Float (Tensor 2 Float)])

(def eq_Float (Bool) ((a : Float) (b : Float))
    (eq a b))

(gdef fwd [eq_Float (Tuple Float Float)])
(gdef rev [eq_Float (Tuple Float Float)])
(gdef suffwdpass [eq_Float (Tuple Float Float)])
(gdef sufrevpass [eq_Float (Tuple Float Float)])
(gdef sufrev [eq_Float (Tuple Float Float)])

(def eq_VecFloat (Bool) ((a : (Vec Float)) (b : (Vec Float)))
    (eq a b))

(gdef fwd [eq_VecFloat (Tuple (Vec Float) (Vec Float))])
(gdef rev [eq_VecFloat (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [eq_VecFloat (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [eq_VecFloat (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [eq_VecFloat (Tuple (Vec Float) (Vec Float))])

(def ne_Float (Bool) ((a : Float) (b : Float))
    (ne a b))

(gdef fwd [ne_Float (Tuple Float Float)])
(gdef rev [ne_Float (Tuple Float Float)])
(gdef suffwdpass [ne_Float (Tuple Float Float)])
(gdef sufrevpass [ne_Float (Tuple Float Float)])
(gdef sufrev [ne_Float (Tuple Float Float)])

(def ne_VecFloat (Bool) ((a : (Vec Float)) (b : (Vec Float)))
    (ne a b))

(gdef fwd [ne_VecFloat (Tuple (Vec Float) (Vec Float))])
(gdef rev [ne_VecFloat (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [ne_VecFloat (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [ne_VecFloat (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [ne_VecFloat (Tuple (Vec Float) (Vec Float))])

(def mkfloat Float ((seed  : Integer)
                    (scale : Float))
       (mul ($ranhashdoub seed) scale))

(gdef fwd [mkfloat (Tuple Integer Float)])
(gdef rev [mkfloat (Tuple Integer Float)])
(gdef suffwdpass [mkfloat (Tuple Integer Float)])
(gdef sufrevpass [mkfloat (Tuple Integer Float)])
(gdef sufrev [mkfloat (Tuple Integer Float)])

(def mkvec (Vec Float) ((seed  : Integer)
                        (n     : Integer)
                        (scale : Float))
    (build n (lam (j : Integer) (mkfloat (add j seed) scale))))

(gdef fwd [mkvec (Tuple Integer Integer Float)])
(gdef rev [mkvec (Tuple Integer Integer Float)])
(gdef suffwdpass [mkvec (Tuple Integer Integer Float)])
(gdef sufrevpass [mkvec (Tuple Integer Integer Float)])
(gdef sufrev [mkvec (Tuple Integer Integer Float)])

(def testfunc Float ((v1 : Float) (v2 : Float))
    (let (mv42 (mkvec 42 5 1.3))
    (let (mv43 (mkvec 43 5 v2))
    (let (deltaVec1 (ts_add mv42 (deltaVec_Float 5 3 v1)))
    (let (v3 (mul v2 (ts_dot deltaVec1 mv43)))
    (let (constVec2 (ts_scale v1 (constVec_Float 6 v3)))
    (let (fv (index 3 deltaVec1))
    (let (vf (Vec_init (cos fv) 4.4))
    (let (sinv2 (Vec_init (sin v2)))
    (add (sumbuild (size deltaVec1) (lam (i : Integer)
             (mul (index i deltaVec1) (cos (to_float i)))))
         (mul (mul (sum vf) (index 0 sinv2))
            (sumbuild (size constVec2) (lam (i : Integer)
                (mul (index i constVec2) (cos (add 0.5 (to_float i)))))))))))))))))

(gdef fwd [testfunc (Tuple Float Float)])
(gdef rev [testfunc (Tuple Float Float)])
; FIXME: SUF/BOG-AD missing some prim functions
; (gdef suffwdpass [testfunc (Tuple Float Float)])
; (gdef sufrevpass [testfunc (Tuple Float Float)])
; (gdef sufrev [testfunc (Tuple Float Float)])

(def main Integer ()
     (let (seed 20000)
     (let (delta 0.0001)
     (let (tolerance 0.0001)

     (let (x1  (mkfloat (add seed 0)       1.0))
     (let (x2  (mkfloat (add seed 1)       1.0))
     (let (x   (tuple x1 x2))
     (let (dx1 (mkfloat (add seed 2)      delta))
     (let (dx2 (mkfloat (add seed 3)      delta))
     (let (dx  (tuple dx1 dx2))
     (let (df  (mkfloat (add seed 4)       1.0))

     (let (test_x (testfunc x))
     (let (test_x_plus_dx (testfunc (ts_add x dx)))
     (let (test_fd (sub test_x_plus_dx test_x))

     (let (test_fwd ([fwd testfunc] x dx))

     (let (fwd_ok
              (lt (abs (sub test_fwd test_fd))
                  (mul (add (abs test_fwd) (abs test_fd)) tolerance)))

     (let (checked ($check  (lam (t : Tuple Float Float) (testfunc t))
                            (lam (t : Tuple (Tuple Float Float) Float) ([rev testfunc] t))
                            x
                            x
                            dx
                            df))
     (let (rev_ok (lt (abs checked) tolerance))

     (print
        "x = " x "\n"
        "dx = " dx "\n"
        "testfunc(x) = " test_x "\n"
        "testfunc(x + dx) = " test_x_plus_dx "\n"
        "fwd = " test_fwd "\n"
        "fd = " test_fd "\n"
        "fwd - fd = " (sub test_fwd test_fd) "\n"
        "rev = " ([rev testfunc] x df) "\n"
        "checked (should be small) = " checked "\n"

        "TESTS FOLLOW"
        "\n----\n"
        "[fwd testfunc]\n"
        fwd_ok
        "\n----\n"
        "[rev testfunc]\n"
        rev_ok
        "\n"
        )))))))))))))))))))
