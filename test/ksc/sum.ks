(def vsum Float ( (i : Integer) (v : Vec Float) )
       (if (== i 0) 0.0 (+ (index i v) (vsum (+ i 1) v))))

