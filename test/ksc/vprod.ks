; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def vprod Float ( (i : Integer) (v : Vec n Float) )
       (if (== i n) 
           1.0 
           (* (index i v) (vprod (+ i 1) v))))

(def aprod Float ( (i : Integer) (v : Vec n Float) (acc : Float) )
       (if (== i n) 
           acc
         (let (acc (* acc (index i v)))
            (aprod (+ i 1) v acc))))

(edef $BENCH Float ((Lam (Tuple) Float)))
(def vchomp Float (_ : (Tuple (Tuple) (Vec n Float))) 1.0)
(def achomp Float (_ : (Tuple (Tuple) (Vec n Float) Float)) 1.0)

;; run now takes about 40 sec.
;; Run with
;;   *Main> :! obj\\test\\ksc\\vprod.exe
;; to see progress bars
(def main Integer ()
  (pr "VPROD"
      (build 4 (lam (n : Integer) 
                  (let (v (build (* (+ n 1) 100) (lam (i : Integer) (+ ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (vprod 0 v))))))

      "APROD"
      (build 4 (lam (n : Integer) 
                  (let (v (build (* (+ n 1) 100) (lam (i : Integer) (+ ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (aprod 0 v 1.0))))))

      "rev$vprod"
      (build 4 (lam (n : Integer) 
                  (let (v (build (* (+ n 1) 100) (lam (i : Integer) (+ ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (vchomp (rev$vprod 0 v 1.0)))))))
          
      "rev$aprod"
      (build 4 (lam (n : Integer) 
                  (let (v (build (* (+ n 1) 100) (lam (i : Integer) (+ ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (achomp (rev$aprod 0 v 1.0 1.0)))))))

  )
)
