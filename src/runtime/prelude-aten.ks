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

(def aten::sin Float (a : Float)
    (sin a))

(def aten::Float Float (a : Integer)
    (to_float a))

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

(def aten::size (Tuple Integer Integer) (a : Tensor 2 Float)
    (size a))

(def aten::len Integer (a : Tuple Integer Integer)
    2)
