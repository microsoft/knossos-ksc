(def sumExample (Tuple (Vec n Float) Float) (v : Vec n Float)
     (forRange n (tuple v 0.0) (lam (i_v_acc : Tuple Integer (Tuple (Vec n Float) Float))
         (untuple (i v_acc) i_v_acc
         (untuple (v acc) v_acc
         (untuple (vi v_) (indexL i v)
         (tuple v_ (add acc vi))))))))
