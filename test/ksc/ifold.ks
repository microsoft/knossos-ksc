(def prod (Tuple Float (Vec Float)) (v : Vec Float)
     (ifold (size v)
            (lam (acc_v_i : (Tuple (Tuple Float (Vec Float)) Integer))
                 (let ((acc_v i) acc_v_i)
                 (let ((acc v) acc_v)
                 (tuple (mul (index i v) acc) v))))
            (tuple 1.0 v)))

(gdef suffwdpass [prod (Vec Float)])
(gdef sufrevpass [prod (Vec Float)])

(def pow_ Float ((x : Float) (n : Integer))
     (let ((xn v) (prod (build n (lam (i : Integer) x)))) xn))

(gdef suffwdpass [pow_ (Tuple Float Integer)])
(gdef sufrevpass [pow_ (Tuple Float Integer)])
(gdef sufrev [pow_ (Tuple Float Integer)])

(def main Integer ()
  (let (onetofour (build 4 (lam (i : Integer) (to_float (add i 1)))))
     (print
      (prod onetofour)
      "\n"
      ([sufrev pow_] (tuple 2.0 5) 1.0)
      "\n"
      80
      "\n"
      )))
