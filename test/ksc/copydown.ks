

(def mkvec (Vec Float) ((n : Integer) (start : Float))
    (build n (lam (i : Integer) (add start (to_float i)))))

(gdef fwd [mkvec (Tuple Integer Float)])
(gdef rev [mkvec (Tuple Integer Float)])
(gdef suffwdpass [mkvec (Tuple Integer Float)])
(gdef sufrevpass [mkvec (Tuple Integer Float)])
(gdef sufrev [mkvec (Tuple Integer Float)])

(def zerovec (Vec Float) (n : Integer)
    (build n (lam (i : Integer) 0.0)))

(gdef fwd [zerovec Integer])
(gdef rev [zerovec Integer])
(gdef suffwdpass [zerovec Integer])
(gdef sufrevpass [zerovec Integer])
(gdef sufrev [zerovec Integer])

(def zerovecvec (Vec (Vec Float)) ((m : Integer) (n : Integer))
    (build m (lam (i : Integer) (zerovec n))))

(gdef fwd [zerovecvec (Tuple Integer Integer)])
(gdef rev [zerovecvec (Tuple Integer Integer)])
(gdef suffwdpass [zerovecvec (Tuple Integer Integer)])
(gdef sufrevpass [zerovecvec (Tuple Integer Integer)])
(gdef sufrev [zerovecvec (Tuple Integer Integer)])

(def test1 (Tuple (Vec Float) Integer (Vec Float)) (dummy : Integer)
    (tuple (mkvec 10 0.0) 25 (mkvec 5 50.0)))

(gdef fwd [test1 Integer])
(gdef rev [test1 Integer])
(gdef suffwdpass [test1 Integer])
(gdef sufrevpass [test1 Integer])
(gdef sufrev [test1 Integer])

(def test2_impl (Vec Float) (ignored : (Vec Float))
    (mkvec 10 0.0))

(gdef fwd [test2_impl (Vec Float)])
(gdef rev [test2_impl (Vec Float)])
(gdef suffwdpass [test2_impl (Vec Float)])
(gdef sufrevpass [test2_impl (Vec Float)])
(gdef sufrev [test2_impl (Vec Float)])

(def test2 (Vec Float) (dummy : Integer)
    (test2_impl (mkvec 2 100.0)))

(gdef fwd [test2 Integer])
(gdef rev [test2 Integer])
(gdef suffwdpass [test2 Integer])
(gdef sufrevpass [test2 Integer])
(gdef sufrev [test2 Integer])

(def test3_impl (Tuple (Vec Float) Integer (Vec Float)) (ignored : (Vec Float))
    (tuple (mkvec 10 0.0) 40 (mkvec 5 60.0)))

(gdef fwd [test3_impl (Vec Float)])
(gdef rev [test3_impl (Vec Float)])
(gdef suffwdpass [test3_impl (Vec Float)])
(gdef sufrevpass [test3_impl (Vec Float)])
(gdef sufrev [test3_impl (Vec Float)])

(def test3 (Tuple (Vec Float) Integer (Vec Float)) (dummy : Integer)
    (test3_impl (mkvec 2 100.0)))

(gdef fwd [test3 Integer])
(gdef rev [test3 Integer])
(gdef suffwdpass [test3 Integer])
(gdef sufrevpass [test3 Integer])
(gdef sufrev [test3 Integer])

(def test4_impl (Vec (Tuple (Vec Float) Integer (Vec Float))) (ignored : (Vec Float))
    (build 6 (lam (i : Integer) (tuple (mkvec 10 0.0) (add 40 i) (mkvec 5 (add 60.0 (to_float i)))))))

(gdef fwd [test4_impl (Vec Float)])
(gdef rev [test4_impl (Vec Float)])
(gdef suffwdpass [test4_impl (Vec Float)])
(gdef sufrevpass [test4_impl (Vec Float)])
(gdef sufrev [test4_impl (Vec Float)])

(def test4 (Vec (Tuple (Vec Float) Integer (Vec Float))) (dummy : Integer)
    (test4_impl (mkvec 2 100.0)))

(gdef fwd [test4 Integer])
(gdef rev [test4 Integer])
(gdef suffwdpass [test4 Integer])
(gdef sufrevpass [test4 Integer])
(gdef sufrev [test4 Integer])

(def test5_swap (Tuple (Vec Float) (Vec Float)) ((a : (Vec Float)) (b : (Vec Float)))
    (tuple b a))

(gdef fwd [test5_swap (Tuple (Vec Float) (Vec Float))])
(gdef rev [test5_swap (Tuple (Vec Float) (Vec Float))])
(gdef suffwdpass [test5_swap (Tuple (Vec Float) (Vec Float))])
(gdef sufrevpass [test5_swap (Tuple (Vec Float) (Vec Float))])
(gdef sufrev [test5_swap (Tuple (Vec Float) (Vec Float))])

(def test5 (Tuple (Vec Float) (Vec Float)) (dummy : Integer)
    (test5_swap (mkvec 10 0.0) (mkvec 8 50.0)))

(gdef fwd [test5 Integer])
(gdef rev [test5 Integer])
(gdef suffwdpass [test5 Integer])
(gdef sufrevpass [test5 Integer])
(gdef sufrev [test5 Integer])

(def test6_helper (Vec (Vec Float)) (a : (Vec (Vec Float)))
     (let (b (build 4 (lam (j : Integer) (mkvec 2 42.0))))
     (build 4 (lam (i : Integer)
        (if (eq i 1)
            (index 2 a)
            (if (eq i 2)
                (index 2 b)
                (mkvec 8 0.0)))))))

(gdef fwd [test6_helper (Vec (Vec Float))])
(gdef rev [test6_helper (Vec (Vec Float))])
(gdef suffwdpass [test6_helper (Vec (Vec Float))])
(gdef sufrevpass [test6_helper (Vec (Vec Float))])
(gdef sufrev [test6_helper (Vec (Vec Float))])

(def test6 (Vec (Vec Float)) (dummy : Integer)
    (test6_helper (build 3 (lam (i : Integer) (mkvec 3 12.0)))))

(gdef fwd [test6 Integer])
(gdef rev [test6 Integer])
(gdef suffwdpass [test6 Integer])
(gdef sufrevpass [test6 Integer])
(gdef sufrev [test6 Integer])

(def test7 (Vec (Vec Float)) ((a : (Vec (Vec Float))) (dummy : Integer))
    (build 3 (lam (i : Integer)
        (if (eq i 1)
            (index 1 a)
            (if (eq i 2)
                (index 3 a)
                (mkvec 3 10.0))))))

(gdef fwd [test7 (Tuple (Vec (Vec Float)) Integer)])
(gdef rev [test7 (Tuple (Vec (Vec Float)) Integer)])
(gdef suffwdpass [test7 (Tuple (Vec (Vec Float)) Integer)])
(gdef sufrevpass [test7 (Tuple (Vec (Vec Float)) Integer)])
(gdef sufrev [test7 (Tuple (Vec (Vec Float)) Integer)])

(def test8 (Vec (Vec Float)) ((a : (Vec (Vec Float))) (dummy : Integer))
    (build 4 (lam (i : Integer)
        (if (eq i 1)
            (index 1 a)
            (if (eq i 2)
                (index 3 a)
                (mkvec 3 10.0))))))

(gdef fwd [test8 (Tuple (Vec (Vec Float)) Integer)])
(gdef rev [test8 (Tuple (Vec (Vec Float)) Integer)])
(gdef suffwdpass [test8 (Tuple (Vec (Vec Float)) Integer)])
(gdef sufrevpass [test8 (Tuple (Vec (Vec Float)) Integer)])
(gdef sufrev [test8 (Tuple (Vec (Vec Float)) Integer)])

(def test9 (Vec (Vec Float)) (dummy : Integer)
    (build 10 (lam (i : Integer)
        (if (eq i 4)
            (zerovec 8)
            (mkvec 4 (mul (to_float i) 10.0))))))

(gdef fwd [test9 Integer])
(gdef rev [test9 Integer])
(gdef suffwdpass [test9 Integer])
(gdef sufrevpass [test9 Integer])
(gdef sufrev [test9 Integer])

(def test10 (Vec (Vec (Vec Float))) (dummy : Integer)
    (build 10 (lam (i : Integer)
        (if (eq i 4)
            (zerovecvec 3 6)
            (build i (lam (j : Integer)
                (mkvec 4 (mul (to_float (add i j)) 10.0))))))))

(gdef fwd [test10 Integer])
(gdef rev [test10 Integer])
(gdef suffwdpass [test10 Integer])
(gdef sufrevpass [test10 Integer])
(gdef sufrev [test10 Integer])

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


