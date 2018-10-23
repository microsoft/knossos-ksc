(def tri ((const n))
  (/ (* n (- n 1)) 2))

(def makeQ (qs ls)
    (let (d (length qs))
      (build d (lam i
        (build d (lam j
          (if (< i j)
            0
            (if (== i j)
              (exp (get qs i))
              (index ls (+ (tri (- i 1)) j))))))))))
