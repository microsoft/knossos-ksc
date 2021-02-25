; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def vprod Float ( (i : Integer) (v : Vec Float) )
       (if (eq i (size v))
           1.0
           (mul (index i v) (vprod (add i 1) v))))

(gdef fwd [vprod (Tuple Integer (Vec Float))])
(gdef rev [vprod (Tuple Integer (Vec Float))])
; SUF/BOG-AD doesn't support recursion
;; (gdef suffwdpass [vprod (Tuple Integer (Vec Float))])
;; (gdef sufrevpass [vprod (Tuple Integer (Vec Float))])
;; (gdef sufrev [vprod (Tuple Integer (Vec Float))])

(def aprod Float ( (i : Integer) (v : Vec Float) (acc : Float) )
       (if (eq i (size v))
           acc
         (let (acc (mul acc (index i v)))
            (aprod (add i 1) v acc))))

(gdef fwd [aprod (Tuple Integer (Vec Float) Float)])
(gdef rev [aprod (Tuple Integer (Vec Float) Float)])
; SUF/BOG-AD doesn't support recursion
;; (gdef suffwdpass [aprod (Tuple Integer (Vec Float) Float)])
;; (gdef sufrevpass [aprod (Tuple Integer (Vec Float) Float)])
;; (gdef sufrev [aprod (Tuple Integer (Vec Float) Float)])

(def fprod Float (v : Vec Float)
     (fold (lam (acc_x : Tuple Float Float)
                (let ((acc x) acc_x)
                  (mul acc x)))
           1.0
           v))

(gdef fwd [fprod (Vec Float)])
(gdef rev [fprod (Vec Float)])
; SUF/BOG-AD doesn't support fold
;; (gdef suffwdpass [fprod (Vec Float)])
;; (gdef sufrevpass [fprod (Vec Float)])
;; (gdef sufrev [fprod (Vec Float)])

(edef indexL (Tuple (Vec Float) Float) ((Tuple Integer (Vec Float))))

(edef [suffwdpass indexL] (Tuple (Tuple (Vec Float) Float) Integer)
                          ((Tuple Integer (Vec Float))))

(edef [sufrevpass [indexL (Tuple Integer (Vec Float))]]
      (Tuple (Tuple) (Vec Float))
      ((Tuple (Tuple (Vec Float) Float) Integer)))

(def ifprod Float (v : Vec Float)
     (let ((v_ r) (ifold (size v)
                         (lam (v_acc_i : (Tuple (Tuple (Vec Float) Float) Integer))
                              (let ((v_acc i) v_acc_i)
                              (let ((v acc) v_acc)
                              (let ((v_ vi) (indexL i v))
                                (tuple v_ (mul acc vi))))))
                         (tuple v 1.0)))
       r))

(gdef sufrevpass [ifprod (Vec Float)])
(gdef suffwdpass [ifprod (Vec Float)])
(gdef sufrev [ifprod (Vec Float)])

(edef $BENCH Float ((Lam (Tuple) Float)))
(def vchomp Float (_ : (Tuple (Tuple) (Vec Float))) 1.0)
(gdef fwd [vchomp (Tuple (Tuple) (Vec Float))])
(gdef rev [vchomp (Tuple (Tuple) (Vec Float))])
(gdef suffwdpass [vchomp (Tuple (Tuple) (Vec Float))])
(gdef sufrevpass [vchomp (Tuple (Tuple) (Vec Float))])
(gdef sufrev [vchomp (Tuple (Tuple) (Vec Float))])
(def achomp Float (_ : (Tuple (Tuple) (Vec Float) Float)) 1.0)
(gdef fwd [achomp (Tuple (Tuple) (Vec Float) Float)])
(gdef rev [achomp (Tuple (Tuple) (Vec Float) Float)])
(gdef suffwdpass [achomp (Tuple (Tuple) (Vec Float) Float)])
(gdef sufrevpass [achomp (Tuple (Tuple) (Vec Float) Float)])
(gdef sufrev [achomp (Tuple (Tuple) (Vec Float) Float)])
(def fchomp Float (_ : (Vec Float)) 1.0)
(gdef fwd [fchomp (Vec Float)])
(gdef rev [fchomp (Vec Float)])
(gdef suffwdpass [fchomp (Vec Float)])
(gdef sufrevpass [fchomp (Vec Float)])
(gdef sufrev [fchomp (Vec Float)])

;; run now takes about 40 sec.
;; Run with
;;   *Main> :! obj\\test\\ksc\\vprod.exe
;; to see progress bars
(def main Integer ()
  (let ((N 6)
        (ns (build N (lam (i : Integer) (mul (add i 1) 100)))))
  (print "# Julia code:\n"
      "using Plots\n"
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

      "ifp="
      (build N (lam (n : Integer) 0.0
;                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
;                      ($BENCH (lam (_ : (Tuple)) (ifprod v))))
                    ))
      "\n# C++ currently seems to optimize this to a constant at -O3"
      "\n"

      "rvp="
      (build N (lam (n : Integer)
                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (vchomp ([rev vprod] (tuple 0 v) 1.0)))))))
      "\n"

      "rap="
      (build N (lam (n : Integer) 0.0
;                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
;                      ($BENCH (lam (_ : (Tuple)) (achomp ([rev aprod] (tuple 0 v 1.0) 1.0)))))
                  ))

      "\n# rev$aprod currently seems not to terminate in reasonable time"
      "\n"

      "rfp="
      (build N (lam (n : Integer)
                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (fchomp ([rev fprod] v 1.0)))))))

      "\n"

      "rifp="
      (build N (lam (n : Integer)
                  (let (v (build (index n ns) (lam (i : Integer) (add ($ranhashdoub i) 0.5))))
                      ($BENCH (lam (_ : (Tuple)) (fchomp ([sufrev ifprod] v 1.0)))))))

      "\n"
      "plot(n,rvp, xscale=:log10, yscale=:log10, title=\"Reverse mode of prod\", xlabel=\"Vector length\", ylabel=\"Run time (us)\", labels=\"vprod\")\n"
      "plot!(n,rfp, labels=\"fprod\")\n"
      "plot!(n,rifp, labels=\"ifprod\")\n"
  )
  )
)
