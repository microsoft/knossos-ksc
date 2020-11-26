

(def mkvec (Vec Float) ((n : Integer) (start : Float))
    (build n (lam (i : Integer) (add start (to_float i)))))

(def add_k (Vec Float) ((v : (Vec Float)) (k : Float))
    (build (size v) (lam (i : Integer) (add (index i v) k))))

(def add_ones (Vec Float) (v : (Vec Float))
    (add_k v 1.0))

(def add_ones_vec_vec (Vec (Vec Float)) (v : (Vec (Vec Float)))
    (build (size v) (lam (i : Integer) (add_ones (index i v)))))

(def dps$add_k (Vec Float) ((dest : Allocator) (v_k : (Tuple (Vec Float) Float)))
    (let ((v k) v_k)
        (dps$build dest (tuple (size v)
                               (lam (dest_i : (Tuple Allocator Integer))
                                   (add (index (get$2$2 dest_i) v) k))))))

(def dps$add_ones (Vec Float) ((dest : Allocator) (v : (Vec Float)))
    (dps$add_k dest (tuple v 1.0)))

(def dps$add_ones_vec_vec (Vec (Vec Float)) ((dest : Allocator) (v : (Vec (Vec Float))))
    (dps$build dest (tuple (size v)
                           (lam (dest_i : (Tuple Allocator Integer)) 
                               (let ((dest_inner i) dest_i)
                                  (dps$add_ones dest_inner (index i v)))))))

(def main Integer ()
    (let (testvec (mkvec 10 2.0))
    (let (testvecvec (build 5 (lam (i : Integer) (mkvec (add 5 i) 10.0))))
      (print
          "\n----\n" 
          "TESTS FOLLOW"

          "\n----\n"
          "DPS build (Vec Float)\n"
          (eq (add_ones testvec)
              ($allocateAndEmplace 80 (lam (dest : Allocator) (dps$add_ones dest testvec))))

          "\n----\n"
          "DPS build (Vec (Vec Float))\n"
          (eq (add_ones_vec_vec testvecvec)
              ($allocateAndEmplace 600 (lam (dest : Allocator) (dps$add_ones_vec_vec dest testvecvec))))
      ))))


