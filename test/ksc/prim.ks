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

