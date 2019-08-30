(def sumExample (Tuple (Vec n Float) Float) (v : Vec n Float)
     (forRange n (tuple v 0.0) (lam (i_v_acc : Tuple Integer (Tuple (Vec n Float) Float))
         (untuple (i v_acc) i_v_acc
         (untuple (v acc) v_acc
         (untuple (vi v_) (indexL i v)
         (tuple v_ (add acc vi))))))))

(def main Integer ()
     (let ((v (build 10 (lam (i : Integer) (to_float i))))
           (zero (build 10 (lam (i : Integer) 0.0))))
       (print "sumExample v" (sumExample v) "\n"
              "revl$sumExample v (tuple zero 1.0) "
              (revl$sumExample v (tuple zero 1.0))
              "\n"
              )))
