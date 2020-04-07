; RUN: ksc-mlir LLVM %s 2> /dev/null

; Usage of some vector maths ought to be interesting

; Element-wise multiply
(def vmul (Vec Float) ((u : Vec Float) (v : Vec Float))
  (build (size u) (lam (i : Integer) (mul (index i u) (index i v)))))

; Standard dot product
(def dot Float ((u : Vec Float) (v : Vec Float))
  (sum (vmul u v)))

; Square norm
(def sqnorm Float ((v : Vec Float))
  (dot v v))

; Linear rectifier
(def max Float ((x : Float) (y : Float)) (if (gt x y) x y))
(def relu (Vec Float) (image : Vec Float)
  (build (size image) (lam (i : Integer) (max (index i image) 0.0))))

; An example from gmm.ks, modified to 1D vecs:
(def gmm_knossos_tri Integer (n : Integer)
  (div (mul n (sub n 1)) 2))

(def gmm_knossos_makeQ (Vec Float) ((q : Vec Float) (j : Integer))
  (let (D (size q))
    (build D (lam (i : Integer)
       (if (lt i j)
        0.0
        (if (eq i j)
          (exp (index i q))
          (index (add (sub (gmm_knossos_tri D) (gmm_knossos_tri (sub D j))) (sub (sub i j) 1)) q))
       )))))
