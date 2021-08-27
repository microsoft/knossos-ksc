
(def cumsum (Vec Float) (xs : (Vec Float))
    (get$1$2
    (fold (lam (acc_x : (Tuple (Tuple (Vec Float) #| evolving result vector |#
                                      Float)      #| sum of evolving result vector |#
                               Float))
               (let (((acc x) acc_x)
                     ((res sumres) acc)
                     (n (size res))
                     (newsumres (add sumres x)))
                (tuple (build (add n 1) (lam (i : Integer)
                        (if (eq i n)
                            newsumres
                            (index i res))))
                       newsumres)))
          (tuple (build 0 (lam (i : Integer) 0.0)) 0.0)
          xs)))

(def [shape cumsum] Integer (xs : (Vec Float))
    (shape xs))

(def cumsum_aux (Tuple (Vec Float) Float)
        ((env : (Tuple))
         (acc : (Tuple (Vec Float) #| evolving result vector |#
                       Float       #| sum of evolving result vector |#))
         (x : Float))
            (let (((res sumres) acc)
                  (n (size res))
                  (newsumres (add sumres x)))
             (tuple (build (add n 1) (lam (i : Integer)
                        (if (eq i n)
                            newsumres
                            (index i res))))
                    newsumres)))

(def assumed_shape_cumsum_aux (Tuple (Vec (Tuple)) (Tuple))
        ((env : (Tuple))
         (acc : (Tuple (Vec Float)
                       Float))
         (x : Float))
    (tuple (constVec (add 1 (size (get$1$2 acc))) (tuple)) (tuple)))

#|
(def cumsum_liftlambda (Vec Float) (xs : (Vec Float))
    (get$1$2
    (fold cumsum_aux
          (tuple)
          (tuple (build 0 (lam (i : Integer) 0.0)) 0.0)
          xs)))
|#

(def cumsum_fixed (Vec Float) (xs : (Vec Float))
    (get$1$3
    (fold (lam (acc_x : (Tuple (Tuple (Vec Float) #| evolving result vector |#
                                      Float       #| sum of evolving result vector |#
                                      Integer)    #| index |#
                               Float))
               (let (((acc x) acc_x)
                     ((res sumres i) acc)
                     (n (size res))
                     (newsumres (add sumres x)))
                (tuple (ts_add res (deltaVec (size res) i newsumres))
                       newsumres
                       (add i 1))))
          (tuple (build (size xs) (lam (i : Integer) 0.0)) 0.0 0)
          xs)))

(def main Integer ()
    (let ((in (build 10 (lam (i : Integer)
                       (to_float (add 1 i)))))
          (expected (build 10 (lam (i : Integer)
                       (to_float (div (mul (add i 1) (add i 2)) 2))))))
      (print
        "\n----\n"
        "TESTS FOLLOW"

          "\n----\n"
          "Cumulative sum with growing accumulator\n"
          (eq expected (cumsum in))

          "\n----\n"
          "Cumulative sum with fixed-size accumulator\n"
          (eq expected (cumsum_fixed in))

    )))
