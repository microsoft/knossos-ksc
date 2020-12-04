(def index_VecFloat Float ((i : Integer) (v : (Vec Float)))
    (index i v))

(def index_VecVecFloat (Vec Float) ((i : Integer) (v : (Vec (Vec Float))))
    (index i v))

(def constVec_Float (Vec Float) ((n : Integer) (v : Float))
    (constVec n v))

(def constVec_VecFloat (Vec (Vec Float)) ((n : Integer) (v : (Vec Float)))
    (constVec n v))

(def build_constVec_Float (Vec Float) ((n : Integer) (v : Float))
    (build n (lam (i : Integer) v)))

(def build_constVec_VecFloat (Vec (Vec Float)) ((n : Integer) (v : (Vec Float)))
    (build n (lam (i : Integer) v)))
    
(def deltaVec_Float (Vec Float) ((n : Integer) (i : Integer) (v : Float))
    (deltaVec n i v))

(def deltaVec_VecFloat (Vec (Vec Float)) ((n : Integer) (i : Integer) (v : (Vec Float)))
    (deltaVec n i v))

(def build_deltaVec_Float (Vec Float) ((n : Integer) (i : Integer) (v : Float))
    (build n (lam (k : Integer) (if (eq i k) v 0.0))))

(def build_deltaVec_VecFloat (Vec (Vec Float)) ((n : Integer) (i : Integer) (v : (Vec Float)))
    (build n (lam (k : Integer) (if (eq i k) v (build (size v) (lam (l : Integer) 0.0))))))

(def mkfloat Float ((seed  : Integer)
                    (scale : Float))
       (mul ($ranhashdoub seed) scale))

(def mkvec (Vec Float) ((seed  : Integer)
                        (n     : Integer)
                        (scale : Float))
    (build n (lam (j : Integer) (mkfloat (add j seed) scale))))

(def testfunc Float ((v1 : Float) (v2 : Float))
    (let (deltaVec1 (deltaVec_Float 5 3 v1))
    (let (constVec2 (constVec_Float 6 v2))
    (add (sumbuild (size deltaVec1) (lam (i : Integer)
             (mul (index i deltaVec1) (cos (to_float i)))))
         (sumbuild (size constVec2) (lam (i : Integer)
             (mul (index i constVec2) (cos (add 0.5 (to_float i))))))))))

(def main Integer ()
     (let (seed 20000)
     (let (delta 0.0001)

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

     (let (test_fwd (fwd$testfunc x dx))

     (let (fwd_ok
              (let (tolerance 0.00001)
              (lt (abs (sub test_fwd test_fd))
                  (mul (add (abs test_fwd) (abs test_fd)) tolerance))))

     (let (checked ($check  (lam (t : Tuple Float Float) (testfunc t))
                            (lam (t : Tuple (Tuple Float Float) Float) (rev$testfunc t))
                            x
                            x
                            dx
                            df))
     (let (rev_ok (lt (abs checked) 0.00001))

     (print
        "x = " x "\n"
        "dx = " dx "\n"
        "testfunc(x) = " test_x "\n"
        "testfunc(x + dx) = " test_x_plus_dx "\n"
        "fwd = " test_fwd "\n"
        "fd = " test_fd "\n"
        "fwd - fd = " (sub test_fwd test_fd) "\n"
        "rev = " (rev$testfunc x df) "\n"
        "checked (should be small) = " checked "\n"

        "TESTS FOLLOW"
        "\n----\n"
        "fwd$deltaVec\n"
        fwd_ok
        "\n----\n"
        "rev$deltaVec\n"
        rev_ok
        "\n"
        ))))))))))))))))))
