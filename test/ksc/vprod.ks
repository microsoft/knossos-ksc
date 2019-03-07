(def vprod Float ( (i : Integer) (v : Vec Float) )
       (if (== i 0) 0.0 (* (index i v) (vprod (+ i 1) v))))
