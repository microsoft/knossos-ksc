(def naked_vector_size Integer
     (x : Vec Float)
     (size x))

(def randfloatvec (Vec Float) ((seed : Integer) (n : Integer))
     (build n (lam (i : Integer) 0.0)))

(def vec2mat (Vec (Vec Float)) ((input : (Vec Float)) (m : Integer) (n : Integer))
     (build m (lam (mi : Integer) (build n (lam (ni : Integer) 0.0)))))

(def randfloatmat (Vec (Vec Float)) ((seed : Integer) (m : Integer) (n : Integer))
  (let ((flat (randfloatvec seed (mul m n))))
    (vec2mat flat m n)))

(def size_binders Float (x : Vec Float)
     (sumbuild (size x) (lam (j : Integer) 3.0)))

(def explicitly_bound Float ((a : Integer) (b : Integer) (x : Vec Float)) 0.0)
