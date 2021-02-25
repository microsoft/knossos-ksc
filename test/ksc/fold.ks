(def f Float (t : Tuple Float (Tuple Float Float))
     0.0)

(gdef fwd [f (Tuple Float (Tuple Float Float))])
(gdef rev [f (Tuple Float (Tuple Float Float))])
(gdef suffwdpass [f (Tuple Float (Tuple Float Float))])
(gdef sufrevpass [f (Tuple Float (Tuple Float Float))])
(gdef sufrev [f (Tuple Float (Tuple Float Float))])

(def fold_f Float ((env : Float) (i : Integer) (v : Vec Float) (acc : Float))
     (if
         (eq i (size v))
         acc
       (fold_f env (add i 1) v (f (tuple env (tuple acc (index i v)))))))

(gdef fwd [fold_f (Tuple Float Integer (Vec Float) Float)])
(gdef rev [fold_f (Tuple Float Integer (Vec Float) Float)])
; SUF/BOG-AD doesn't support recursion
;; (gdef suffwdpass [fold_f (Tuple Float Integer (Vec Float) Float)])
;; (gdef sufrevpass [fold_f (Tuple Float Integer (Vec Float) Float)])
;; (gdef sufrev [fold_f (Tuple Float Integer (Vec Float) Float)])

(def prod Float ((i : Integer) (v : Vec Float) (acc : Float))
     (if
         (eq i (size v))
         acc
       (prod (add i 1) v (mul acc (index i v)))))

(gdef fwd [prod (Tuple Integer (Vec Float) Float)])
(gdef rev [prod (Tuple Integer (Vec Float) Float)])
; SUF/BOG-AD doesn't support recursion
;; (gdef suffwdpass [prod (Tuple Integer (Vec Float) Float)])
;; (gdef sufrevpass [prod (Tuple Integer (Vec Float) Float)])
;; (gdef sufrev [prod (Tuple Integer (Vec Float) Float)])

; This ends up calculating prod(v) * pow(closure, size(v))
(def prod_fold Float ((v : Vec Float) (closure : Float))
     (fold (lam (acc_x : Tuple Float Float)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  (mul (mul acc x) closure)))
           1.0
           v))

(gdef fwd [prod_fold (Tuple (Vec Float) Float)])
(gdef rev [prod_fold (Tuple (Vec Float) Float)])
; SUF/BOG-AD doesn't support fold
;; (gdef suffwdpass [prod_fold (Tuple (Vec Float) Float)])
;; (gdef sufrevpass [prod_fold (Tuple (Vec Float) Float)])
;; (gdef sufrev [prod_fold (Tuple (Vec Float) Float)])

;; Check that it works with an environment type that isn't its own
;; tangent type
(def prod_fold_integer_env Float ((v : Vec Float) (ignored : Integer))
     (fold (lam (acc_x : Tuple Float Float)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  (mul acc x)))
           1.0
           v))

; SUF/BOG-AD doesn't support fold
(gdef fwd [prod_fold_integer_env (Tuple (Vec Float) Integer)])
(gdef rev [prod_fold_integer_env (Tuple (Vec Float) Integer)])
;; (gdef suffwdpass [prod_fold_integer_env (Tuple (Vec Float) Integer)])
;; (gdef sufrevpass [prod_fold_integer_env (Tuple (Vec Float) Integer)])
;; (gdef sufrev [prod_fold_integer_env (Tuple (Vec Float) Integer)])

;; Check that it works with an accumulator type that isn't its own
;; tangent type
(def prod_fold_integer_acc Integer (v : Vec Float)
     (fold (lam (acc_x : Tuple Integer Float)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  acc))
           1
           v))

(gdef fwd [prod_fold_integer_acc (Vec Float)])
(gdef rev [prod_fold_integer_acc (Vec Float)])
; SUF/BOG-AD doesn't support fold
;; (gdef suffwdpass [prod_fold_integer_acc (Vec Float)])
;; (gdef sufrevpass [prod_fold_integer_acc (Vec Float)])
;; (gdef sufrev [prod_fold_integer_acc (Vec Float)])

;; Check that it works with a vector element type that isn't its own
;; tangent type
(def prod_fold_integer_v Float (v : Vec Integer)
     (fold (lam (acc_x : Tuple Float Integer)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  acc))
           1.0
           v))

(gdef fwd [prod_fold_integer_v (Vec Integer)])
(gdef rev [prod_fold_integer_v (Vec Integer)])
; SUF/BOG-AD doesn't support fold
;; (gdef suffwdpass [prod_fold_integer_v (Vec Integer)])
;; (gdef sufrevpass [prod_fold_integer_v (Vec Integer)])
;; (gdef sufrev [prod_fold_integer_v (Vec Integer)])

(def mkfloat Float ((seed  : Integer)
                    (scale : Float))
       (mul ($ranhashdoub seed) scale))

(gdef fwd [mkfloat (Tuple Integer Float)])
(gdef rev [mkfloat (Tuple Integer Float)])
(gdef suffwdpass [mkfloat (Tuple Integer Float)])
(gdef sufrevpass [mkfloat (Tuple Integer Float)])
(gdef sufrev [mkfloat (Tuple Integer Float)])

(def mkvec (Vec Float) ((seed  : Integer)
                        (n     : Integer)
                        (scale : Float))
    (build n (lam (j : Integer) (mkfloat (add j seed) scale))))

(gdef fwd [mkvec (Tuple Integer Integer Float)])
(gdef rev [mkvec (Tuple Integer Integer Float)])
(gdef suffwdpass [mkvec (Tuple Integer Integer Float)])
(gdef sufrevpass [mkvec (Tuple Integer Integer Float)])
(gdef sufrev [mkvec (Tuple Integer Integer Float)])

(def main Integer ()
     (let ((seed 20000)
           (delta 0.0001)
           (v  (mkvec   (add seed 0)    10 1.0))
           (c  (mkfloat (add seed 1000)    1.0))
           (dv (mkvec   (add seed 2000) 10 delta))
           (dc (mkfloat (add seed 3000)    delta))
           (checked ($check (lam (t : Tuple (Vec Float) Float) (prod_fold t))
                            (lam (t : Tuple (Tuple (Vec Float) Float) Float) ([rev prod_fold] t))
                            (tuple v  c)
                            (tuple v  c)
                            (tuple dv dc)
                            1.0))
           (rev_ok (lt (abs checked) 0.001))
           (fold_x   (prod_fold v c))
           (fold_xpd (prod_fold (ts_add v dv) (add c dc)))
           (fold_fwd ([fwd prod_fold] (tuple v c) (tuple dv dc)))
           (fold_fd  (sub fold_xpd fold_x))
           (everything_works_as_expected
            (let ((tolerance 0.001)
                  (actual fold_fd)
                  (expected fold_fwd))
              (lt (abs (sub actual expected))
                      (mul (add (abs expected) (abs actual))
                         tolerance)))))
       (print
        "v = " v "\n"
        "c = " c "\n"
        "dv = " dv "\n"
        "dc = " dc "\n"
        "fold(x) = " fold_x "\n"
        "fold(x + dx) = " fold_xpd "\n"
        "fwd fold = " fold_fwd "\n"
        "fd fold = " fold_fd "\n"
        "fwd - fd = " (sub fold_fwd fold_fd) "\n"
        "rev fold = " ([rev prod_fold] (tuple v c) 1.0) "\n"
        "checked (should be small) = " checked "\n" 
        "TESTS FOLLOW"
        "\n----\n"
        "fwd OK\n"
        everything_works_as_expected
        "\n----\n"
        "rev OK\n"
        rev_ok "\n"
        )))
