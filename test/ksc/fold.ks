(def f Float (t : Tuple Float (Tuple Float Float))
     0.0)

(def fold_f Float ((env : Float) (i : Integer) (v : Vec Float) (acc : Float))
     (if
         (eq i (size v))
         acc
       (fold_f env (add i 1) v (f (tuple env (tuple acc (index i v)))))))

(def prod Float ((i : Integer) (v : Vec Float) (acc : Float))
     (if
         (eq i (size v))
         acc
       (prod (add i 1) v (mul acc (index i v)))))

; This ends up calculating prod(v) * pow(closure, size(v))
(def prod_fold Float ((v : Vec Float) (closure : Float))
     (fold (lam (acc_x : Tuple Float Float)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  (mul (mul acc x) closure)))
           1.0
           v))

;; Check that it works with an environment type that isn't its own
;; tangent type
(def prod_fold_integer_env Float ((v : Vec Float) (ignored : Integer))
     (fold (lam (acc_x : Tuple Float Float)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  (mul acc x)))
           1.0
           v))

;; Check that it works with an accumulator type that isn't its own
;; tangent type
(def prod_fold_integer_acc Integer (v : Vec Float)
     (fold (lam (acc_x : Tuple Integer Float)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  acc))
           1
           v))

;; Check that it works with a vector element type that isn't its own
;; tangent type
(def prod_fold_integer_v Float (v : Vec Integer)
     (fold (lam (acc_x : Tuple Float Integer)
                (let ((acc (get$1$2 acc_x))
                      (x   (get$2$2 acc_x)))
                  acc))
           1.0
           v))

(def mkfloat Float ((seed  : Integer)
                    (scale : Float))
       (mul ($ranhashdoub seed) scale))

(def mkvec (Vec Float) ((seed  : Integer)
                        (n     : Integer)
                        (scale : Float))
    (build n (lam (j : Integer) (mkfloat (add j seed) scale))))

(def main Integer ()
     (let ((seed 20000)
           (delta 0.0001)
           (v  (mkvec   (add seed 0)    10 1.0))
           (c  (mkfloat (add seed 1000)    1.0))
           (dv (mkvec   (add seed 2000) 10 delta))
           (dc (mkfloat (add seed 3000)    delta))
           (checked ($check (lam (t : Tuple (Vec Float) Float) (prod_fold t))
                            (lam (t : Tuple (Tuple (Vec Float) Float) Float) (rev$prod_fold t))
                            (tuple v  c)
                            (tuple v  c)
                            (tuple dv dc)
                            1.0))
           (rev_ok (lt (abs checked) 0.001))
           (fold_x   (prod_fold v c))
           (fold_xpd (prod_fold (ts_add v dv) (add c dc)))
           (fold_fwd (fwd$prod_fold (tuple v c) (tuple dv dc)))
           (fold_fd  (sub fold_xpd fold_x))
           (everything_works_as_expected
            (let ((tolerance 0.001)
                  (actual fold_fd)
                  (expected fold_fwd))
              (lt (abs (sub actual expected))
                      (mul (add (abs expected) (abs actual))
                         tolerance)))))
       (print
        "v"
        v
        "c"
        c
        "dv"
        dv
        "dc"
        dc
        "fold(x)"
        fold_x
        "fold(x + dx)"
        fold_xpd
        "fwd fold"
        fold_fwd
        "fd fold"
        fold_fd
        "fwd - fd"
        (sub fold_fwd fold_fd)
        "rev fold"
        (rev$prod_fold (tuple v c) 1.0)
        "checked (should be small)"
        checked "\n"
        "TESTS FOLLOW"
        "\n----\n"
        "fwd OK\n"
        everything_works_as_expected
        "\n----\n"
        "rev OK\n"
        rev_ok "\n"
        )))
