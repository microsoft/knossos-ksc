; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def vprod Float ( (i : Integer) (v : Vec n Float) )
       (if (eq i n) 
           1.0 
           (mul (index i v) (vprod (add i 1) v))))

(def aprod Float ( (i : Integer) (v : Vec n Float) (acc : Float) )
       (if (eq i n) 
           acc
         (let (acc (mul acc (index i v)))
            (aprod (add i 1) v acc))))

(def fprod Float (v : Vec n Float)
     (fold (lam (acc_x : Tuple Float Float)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  (mul acc x)))
           1.0
           v))

(edef $BENCH Float ((Lam (Tuple) Float)))
(def vchomp Float (_ : (Tuple (Tuple) (Vec n Float))) 1.0)
(def achomp Float (_ : (Tuple (Tuple) (Vec n Float) Float)) 1.0)
(def fchomp Float (_ : (Vec n Float)) 1.0)

;; run now takes about 40 sec.
;; Run with
;;   *Main> :! obj\\test\\ksc\\vprod.exe
;; to see progress bars
(def main Integer ()
  (let ((N 6)
        (ns (build N (lam (i : Integer) (mul (add i 1) 100)))))
  (print "# Julia code:\n"
      "n=" ns "\n"
      "vp="
      (build N (lam (n : Integer) 
                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (vprod 0 v))))))
      "\n"

      "ap="
      (build N (lam (n : Integer) 
                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (aprod 0 v 1.0))))))
      "\n"

      "fp="
       (build N (lam (n : Integer) 0.0
;                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
;                      ($BENCH (lam (_ : (Tuple)) (fprod v))))))
                     ))
      "\n# C++ currently seems to optimize this to a constant at -O3"
      "\n"

      "rvp="
      (build N (lam (n : Integer) 
                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (vchomp (rev$vprod 0 v 1.0)))))))          
      "\n"

      "rap="
      (build N (lam (n : Integer) 
                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (achomp (rev$aprod 0 v 1.0 1.0)))))))

      "\n"

      "rfp="
      (build N (lam (n : Integer)
                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (fchomp (rev$fprod v 1.0)))))))

      "\n"
      "plot(n,rap,n,rvp,n,rfp)\n"
  )
  )
)
