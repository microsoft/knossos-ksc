(def tri (n)
  (/ (* n (- n 1)) 2))

(def Dtri (n)
  (lmZero))

(def makeQ (q l)
    (let (d (length q))
      (build d (lam i
        (build d (lam j
          (if (< i j)
            0
            (if (== i j)
              (exp (get q i))
              (index l (+ (tri (- i 1)) j))))))))))
