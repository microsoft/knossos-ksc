# fmt: off
cost_sequence = [
    102267214109.0,
    102267223999.0,
    102269202999.00002,
    102566201999.00002,
    132365101999.0,
    102475101999.00002,
    102465101999.00002,
    132465101999.0,
    162165001999.0,
    162565001999.0,
    162965001999.0,
    163965001999.0,
    163955001999.0,
    134065001999.0,
    134465001999.0,
    104575001998.99998,
    104565001999.00002,
    104565001999.00002,
    104644201999.00002,
    94744201999.00002,
    84745201999.00002,
    84745201999.00002,
    84745993999.00003,
    84646993999.00003,
    84646003999.00003,
    74647003999.00002,
    74647003999.00002,
    74647003999.00002,
    74647011919.00002,
    74647011998.20001,
    74646021998.20001,
    74646011999.20001
]

annotations = [
    (0, 102267214109.0, """
(def
 rev$conv1d (Tuple (Vec k (Vec l (Vec kn Float)))
                   (Vec l (Vec n Float)))
 ((var0 : (Tuple (Vec k (Vec l (Vec kn Float)))
   (Vec l (Vec n Float))
   (Vec k (Vec n Float)))))
 (let
  ((kernels (get$1$3 var0))
  (image (get$2$3 var0))
  (d$r (get$3$3 var0)))
    (sumbuild k (lam (ki : Integer)
      (let (a_6 (index ki d$r))
        (sumbuild n (lam (ni : Integer)
          (let (a_8 (index ni a_6))
            (let (a_7 (build kn (lam (var1 : Integer) a_8)))
              (sumbuild kn (lam (kni : Integer)
                (let (a_10 (index kni a_7))
                  (let (a_11 (build l (lam (sum$i : Integer) a_10)))
                    (sumbuild l (lam (li : Integer)
                      (let (noi (sub (add ni (div kn 2)) kni))
                        (let (outside_image (or (gt 0 noi) (gte noi n)))
                          (add (if outside_image
                                   (tuple (constVec k (constVec l (constVec kn 0.0)))
                                          (constVec l (constVec n 0.0)))
                                   (tuple (constVec k (constVec l (constVec kn 0.0)))
                                          (deltaVec l li
                                            (deltaVec n noi
                                              (mul (index kni (index li (index ki kernels)))
                                                   (index li a_11))))))
                               (tuple (deltaVec k ki
                                        (deltaVec l li
                                          (deltaVec kn kni
                                            (mul (if outside_image
                                                     0.0
                                                     (index noi (index li image)))
                                                 (index li a_11)))))
                                      (constVec l (constVec n 0.0)))))))))))))))))))
"""),
    (11, 163955001999.0, """
(def
 rev$conv1d (Tuple (Vec k (Vec l (Vec kn Float)))
                   (Vec l (Vec n Float)))
 (var0 : (Vec k (Vec l (Vec kn Float)))
  (Vec l (Vec n Float))
  (Vec k (Vec n Float)))
 (let
  ((kernels (get$1$3 var0))
  (image (get$2$3 var0))
  (d$r (get$3$3 var0)))
    (sumbuild k (lam (ki : Integer)
      (sumbuild n (lam (ni : Integer)
        (sumbuild kn (lam (kni : Integer)
          (sumbuild l (lam (li : Integer)
            (let (noi (sub (add ni (div kn 2)) kni))
              (let (outside_image (or (gt 0 (sub (add ni (div kn 2)) kni))
                                      (gte (sub (add ni (div kn 2)) kni) n)))
                (add (if (or (gt 0 (sub (add ni (div kn 2)) kni))
                             (gte (sub (add ni (div kn 2)) kni) n))
                         (tuple (constVec k (constVec l (constVec kn 0.0)))
                                (constVec l (constVec n 0.0)))
                         (tuple (constVec k (constVec l (constVec kn 0.0)))
                                (deltaVec l li
                                  (deltaVec n noi
                                    (mul (index kni (index li (index ki kernels)))
                                         (index li (build l (lam (sum$i : Integer)
                                                     (index ni (index ki d$r))))))))))
                     (tuple (deltaVec k ki
                              (deltaVec l li
                                (deltaVec kn kni
                                  (mul (if outside_image
                                           0.0
                                           (index noi (index li image)))
                                       (index li (build l (lam (var0 : Integer)
                                                   (index ni (index ki d$r)))))))))
                            (constVec l (constVec n 0.0))))))))))))))))
"""),
    (31, 74646011999.20001, """
(def
 rev$conv1d (Tuple (Vec k (Vec l (Vec kn Float)))
                   (Vec l (Vec n Float)))
 (var0 : (Vec k (Vec l (Vec kn Float)))
                        (Vec l (Vec n Float))
                        (Vec k (Vec n Float)))
 (let
  ((kernels (get$1$3 var0))
  (image (get$2$3 var0))
  (d$r (get$3$3 var0)))
 (add
   (sumbuild k (lam (var6 : Integer)
     (sumbuild n (lam (var5 : Integer)
       (sumbuild kn (lam (var7 : Integer)
         (sumbuild l (lam (var8 : Integer)
           (if (or (gt 0 (sub (add var5 (div kn 2)) var7))
                   (gte (sub (add var5 (div kn 2)) var7) n))
               (tuple (constVec k (constVec l (constVec kn 0.0)))
                      (constVec l (constVec n 0.0)))
                   (tuple (constVec k (constVec l (constVec kn 0.0)))
                      (deltaVec l var8
                        (deltaVec n (sub (add var5 (div kn 2)) var7)
                          (mul (index var7 (index var8 (index var6 kernels)))
                            (let (sum$i var8)
                              (index var5 (index var6 d$r))))))))))))))))
  (tuple (build k (lam (var4 : Integer)
           (sumbuild n (lam (var3 : Integer)
             (build l (lam (var1 : Integer)
                (build kn (lam (var2 : Integer)
                  (mul (if (or (gt 0 (sub (add var3 (div kn 2)) var2))
                               (gte (sub (add var3 (div kn 2)) var2) n))
                           0.0
                           (index (sub (add var3 (div kn 2)) var2)
                             (index var1 image)))
                       (let (var0 var1)
                         (index var3 (index var4 d$r))))))))))))
         (constVec l (constVec n 0.0)))))
""")
]
