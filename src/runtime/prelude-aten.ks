;; Definitions for aten functions
;; edefs will go in prelude-aten.cpp

(def aten::lt Bool ((a : Float) (b : Float))
    (lt a b))

(def aten::lt Bool ((a : Integer) (b : Float))
    (lt (to_float a) b))

(def aten::mul Float ((a : Float) (b : Float))
    (mul a b))

(def aten::mul (Tensor 1 Float) ((a : Tensor 1 Float) (b : Float))
    (ts_scale b a))

(def aten::mul (Tensor 2 Float) ((a : Tensor 2 Float) (b : Float))
    (ts_scale b a))

(def aten::add Float ((a : Float) (b : Float))
    (add a b))

(def aten::neg Float (a : Float)
    (neg a))

(def aten::sin Float (a : Float)
    (sin a))

(def aten::Float Float (a : Integer)
    (to_float a))
(def aten::Float Float (a : Float)
    a)

; (edef aten::pow Float (Float Integer))

; (def D$aten::pow (LM Float Float) ((a : Float) (b : Integer))
;     (LMScale (aten::pow a (sub b 1))))

(def Tensor_init (Tensor 2 Float) ((a : Vec (Vec Float)))
    (let (m (size a))
    (let (n (size (index 0 a)))
    (build (tuple m n) (lam (ij : Tuple Integer Integer)
        (let ((i j) ij)
            (index j (index i a))))))))

(def VecVec_init (Vec (Vec Float)) (a : Tensor 2 Float)
    (let ((m n) (size a))
    (build m (lam (i : Integer)
        (build n (lam (j : Integer)
            (index (tuple i j) a)))))))

(def xrev$Tensor_init (Vec (Vec Float)) ((a : Vec (Vec Float)) (dr : Tensor 2 Float))
    (VecVec_init dr))
   
(def aten::tensor (Tensor 2 Float) ((a : Vec (Vec Float)) (x1 : Float) (x2 : Float) (x3 : Float) )
    (Tensor_init a))

(def aten::tensor (Tensor 1 Float) ((a : Vec Float) (x1 : Float) (x2 : Float) (x3 : Float) )
    a)

; mul Mat Vec
(edef aten::matmul (Tensor 1 Float) ((Tensor 2 Float) (Tensor 1 Float)))
(def shape$aten::matmul (Tensor 1 (Tuple)) ((m : (Tensor 2 Float)) (v : (Tensor 1 Float)))
          (constVec (size v) (tuple)))

(edef D$aten::matmul (LM (Tuple (Tensor 2 Float) (Tensor  1 Float)) (Tensor  1 Float))
          ((Tensor 2 Float) (Tensor  1 Float)))
(edef Dt$aten::matmul (Tuple (Tensor  1 Float) (LM (Tuple (Tensor 2 Float) (Tensor  1 Float)) (Tensor  1 Float)))
          ((Tensor 2 Float) (Tensor  1 Float)))

(edef R$aten::matmul (LM (Tensor  1 Float) (Tuple (Tensor 2 Float) (Tensor  1 Float)))
          ((Tensor 2 Float) (Tensor  1 Float)))

(def fwd$aten::matmul (Tensor 1 Float)
          ((M_v : (Tuple (Tensor 2 Float) (Tensor  1 Float))) (dM_dv : (Tuple (Tensor 2 Float) (Tensor  1 Float))))
     (let ((M v) M_v)
     (let ((dM dv) dM_dv)
        (ts_add (aten::matmul dM v) (aten::matmul M dv)))))

(edef rev$aten::matmul (Tuple (Tensor 2 Float) (Tensor  1 Float))
          ((Tuple (Tensor 2 Float) (Tensor  1 Float)) (Tensor  1 Float)))


(def aten::dot Float ((a : Tensor 1 Float) (b : Tensor 1 Float))
    (ts_dot a b))

(def aten::size (Tuple Integer Integer) (a : Tensor 2 Float)
    (size a))

(def aten::len Integer (a : Tuple Integer Integer)
    2)