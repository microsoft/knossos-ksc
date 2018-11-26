
(def f ((x : Vec Float) (y : Vec Float)) 
    (if (< 2 3) (index 1 x) 7.0)
)

(def main ()
    (let (v1 (build 4 (lam (i : Integer) 3.0)))
        (pr 1
            (D$f v1 v1)
            )))
