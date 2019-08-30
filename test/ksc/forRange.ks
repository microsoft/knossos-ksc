(def sumExample (Tuple (Vec n Float) Float) (v : Vec n Float)
     (forRange n (tuple v 0.0) (lam (i_v_acc : Tuple Integer (Tuple (Vec n Float) Float))
         (untuple (i v_acc) i_v_acc
         (untuple (v acc) v_acc
         (untuple (vi v_) (indexL i v)
         (tuple v_ (add acc vi))))))))

(def prodExample (Tuple (Vec n Float) Float) (v : Vec n Float)
     (forRange n (tuple v 1.0) (lam (i_v_acc : Tuple Integer (Tuple (Vec n Float) Float))
         (untuple (i v_acc) i_v_acc
         (untuple (v acc) v_acc
         (untuple (vi v_) (indexL i v)
         (tuple v_ (mul acc vi))))))))

(def main Integer ()
     (let ((v (build 10 (lam (i : Integer) (to_float (add i 1)))))
           (zero (build 10 (lam (i : Integer) 0.0)))
           (zero2 (build 10 (lam (i : Integer) 1e-38))))
       (print "sumExample v" (sumExample v) "\n"
              "revl$sumExample v (tuple zero 1.0) "
              (revl$sumExample v (tuple zero 1.0))
              "\n"
              "prodExample v" (prodExample v) "\n"
              "revl$prodExample v (tuple zero 1.0) "
              (revl$prodExample v (tuple zero2 1.0))
              "\n"
              )))
