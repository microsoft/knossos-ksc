(def naked_vector_size Integer
     (x : (Vec D Float))
     D)

(def randfloatvec (Vec n Float) ((seed : Integer) (n : Integer))
     (build n (lam (i : Integer) 0.0)))

(def vec2mat (Vec m (Vec n Float)) ((input : (Vec mn Float)) (m : Integer) (n : Integer))
     (build m (lam (mi : Integer) (build n (lam (ni : Integer) 0.0)))))

(def randfloatmat (Vec m (Vec n Float)) ((seed : Integer) (m : Integer) (n : Integer))
  (let ((flat (randfloatvec seed (mul m n))))
    (vec2mat flat m n)))

(def size_binders Float (x : Vec a Float)
     (sumbuild a (lam (j : Integer) 3.0)))
