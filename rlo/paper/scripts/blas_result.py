# fmt: off
cost_sequence = [
    7160405.099999997,
    70001160305.00006,
    700150305.0000002,
    7130302.899999997,
    508080202.8000002,
    12070202.799999997,
    7100202.799999997,
    70201080102.70004,
    702070102.7000002,
    7070102.699999997
]

annotations = [
    (0, 7160408, """
(def
 gemm (Vec n (Vec l Float))
 ((var0 : (Tuple Float
                 (Vec n (Vec m Float))
                 (Vec m (Vec l Float))
                 Float
                 (Vec n (Vec l Float)))))
 (let (beta (get$4$5 var0))
 (let (mat_b (get$3$5 var0))
 (let (mat_a (get$2$5 var0))
 (let (alpha (get$1$5 var0))
 (let (mat_c (get$5$5 var0))
   (let (mat_x (build n (lam (var4 : Integer)
                 (build l (lam (k : Integer)
                   (sumbuild m (lam (var5 : Integer)
                     (mul (index var4 (index var5 mat_a))
                          (index var5 (index k mat_b))))))))))
   (let (mat_x_6 (build n (lam (var2 : Integer)
                   (build m (lam (var3 : Integer)
                     (mul alpha (index var2 (index var3 mat_x))))))))
   (let (mat_y (build n (lam (var6 : Integer)
                 (build m (lam (var1 : Integer)
                   (mul beta (index var6 (index var1 mat_c))))))))
           (build n (lam (i : Integer)
             (build m (lam (j : Integer)
               (add (index i (index j mat_x_6))
                    (index i (index j mat_y)))))))))))))))
"""),
    (9, 7070100, """
(def
 gemm (Vec n (Vec l Float))
 ((var0 : (Tuple Float
                 (Vec n (Vec m Float))
                 (Vec m (Vec l Float))
                 Float
                 (Vec n (Vec l Float)))))
 (let (beta (get$4$5 var0))
 (let (mat_b (get$3$5 var0))
 (let (mat_a (get$2$5 var0))
 (let (alpha (get$1$5 var0))
 (let (mat_c (get$5$5 var0))
     (build n (lam (i : Integer)
       (build l (lam (j : Integer)
         (add
           (mul alpha
             (sumbuild m (lam (var5 : Integer)
               (mul
                 (index i (index var5 mat_a))
                 (index var5 (index j mat_b))))))
           (mul beta (index j (index i mat_c))))))))))))))
""")
]
