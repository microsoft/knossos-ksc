(def f ((x : Vec Float) (y : Vec Float)) 
    (if (< 2 3) (index 1 x) 7.0)
)

(def mkvec (n : Integer)
    (build n (lam (j : Integer) (to_float j))))

(def sqnorm (v : Vec Float)
  (sum (build (size v) (lam (i : Integer) (let (vi (index i v)) (* vi vi)))))) 

{-
(def g1 (gamma : Float)
    (let (ls     (build 10 (lam (i : Integer) (mkvec 3 gamma))))
         (sqnorm (index 0 ls))))
-}

(def g (gamma : Float)
    (let (v     (* gamma (mkvec 3)))
         (sqnorm v)))

(def main ()
    (let (v1 (build 4 (lam (i : Integer) 3.0)))
        (pr 1
            (D$f v1 v1)
            --(D$g 1.1)
            (fwd$g 1.1 0.001)
            (- (g 1.101) (g 1.1))
            )))
