

(def mkvec (Vec Float) ((n : Integer) (start : Float))
    (build n (lam (i : Integer) (add start (to_float i)))))

(def zerovec (Vec Float) (n : Integer)
    (build n (lam (i : Integer) 0.0)))
(def zerovecvec (Vec (Vec Float)) ((m : Integer) (n : Integer))
    (build m (lam (i : Integer) (zerovec n))))

(def test1 (Tuple (Vec Float) Integer (Vec Float)) (dummy : Integer)
    (tuple (mkvec 10 0.0) 25 (mkvec 5 50.0)))

(def test2_impl (Vec Float) (ignored : (Vec Float))
    (mkvec 10 0.0))
(def test2 (Vec Float) (dummy : Integer)
    (test2_impl (mkvec 2 100.0)))

(def test3_impl (Tuple (Vec Float) Integer (Vec Float)) (ignored : (Vec Float))
    (tuple (mkvec 10 0.0) 40 (mkvec 5 60.0)))
(def test3 (Tuple (Vec Float) Integer (Vec Float)) (dummy : Integer)
    (test3_impl (mkvec 2 100.0)))

(def test4_impl (Vec (Tuple (Vec Float) Integer (Vec Float))) (ignored : (Vec Float))
    (build 6 (lam (i : Integer) (tuple (mkvec 10 0.0) (add 40 i) (mkvec 5 (add 60.0 (to_float i)))))))
(def test4 (Vec (Tuple (Vec Float) Integer (Vec Float))) (dummy : Integer)
    (test4_impl (mkvec 2 100.0)))

(def test5_swap (Tuple (Vec Float) (Vec Float)) ((a : (Vec Float)) (b : (Vec Float)))
    (tuple b a))
(def test5 (Tuple (Vec Float) (Vec Float)) (dummy : Integer)
    (test5_swap (mkvec 10 0.0) (mkvec 8 50.0)))

(def test6_helper (Vec (Vec Float)) (a : (Vec (Vec Float)))
     (let (b (build 4 (lam (j : Integer) (mkvec 2 42.0))))
     (build 4 (lam (i : Integer)
        (if (eq i 1)
            (index 2 a)
            (if (eq i 2)
                (index 2 b)
                (mkvec 8 0.0)))))))
(def test6 (Vec (Vec Float)) (dummy : Integer)
    (test6_helper (build 3 (lam (i : Integer) (mkvec 3 12.0)))))

(def test7 (Vec (Vec Float)) ((a : (Vec (Vec Float))) (dummy : Integer))
    (build 3 (lam (i : Integer)
        (if (eq i 1)
            (index 1 a)
            (if (eq i 2)
                (index 3 a)
                (mkvec 3 10.0))))))

(def test8 (Vec (Vec Float)) ((a : (Vec (Vec Float))) (dummy : Integer))
    (build 4 (lam (i : Integer)
        (if (eq i 1)
            (index 1 a)
            (if (eq i 2)
                (index 3 a)
                (mkvec 3 10.0))))))

(def test9 (Vec (Vec Float)) (dummy : Integer)
    (build 10 (lam (i : Integer)
        (if (eq i 4)
            (zerovec 8)
            (mkvec 4 (mul (to_float i) 10.0))))))

(def test10 (Vec (Vec (Vec Float))) (dummy : Integer)
    (build 10 (lam (i : Integer)
        (if (eq i 4)
            (zerovecvec 3 6)
            (build i (lam (j : Integer)
                (mkvec 4 (mul (to_float (add i j)) 10.0))))))))

(def main Integer ()
      (print
          "\n----\n" 
          "TESTS FOLLOW"

          "\n----\n"
          "Trivial copydown\n"
          (eq (test1 0) ($copydown (test1 1)))

          "\n----\n"
          "Monotonic copydown of vec\n"
          (eq (test2 0) ($copydown (test2 1)))

          "\n----\n"
          "Monotonic copydown of tuple\n"
          (eq (test3 0) ($copydown (test3 1)))

          "\n----\n"
          "Monotonic copydown of vec of tuple\n"
          (eq (test4 0) ($copydown (test4 1)))

          "\n----\n"
          "Copydown requiring swap of tuple elements\n"
          (eq (test5 0) ($copydown (test5 1)))

          "\n----\n"
          "Copydown vec elements allocated before build\n"
          (eq (test6 0) ($copydown (test6 1)))

          "\n----\n"
          "Copydown vec elements allocated before copydown destination\n"
          (let (a (build 5 (lam (i : Integer) (mkvec 3 0.0))))
            (eq (test7 a 0) ($copydown (test7 a 1))))

          "\n----\n"
          "Copydown must not make a temporary copy in a place which still overlaps the destination\n"
          (let (a (build 5 (lam (i : Integer) (mkvec 3 0.0))))
            (eq (test8 a 0) ($copydown (test8 a 1))))

          "\n----\n"
          "Copydown of zero vec\n"
          (eq (test9 0) ($copydown (test9 0)))

          "\n----\n"
          "Copydown of zero vec-of-vec\n"
          (eq (test10 0) ($copydown (test10 0)))
      ))


