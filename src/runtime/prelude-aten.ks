;; Definitions for aten functions
;; edefs will go in prelude-aten.cpp


(def transpose (Tensor 2 Float) (x : Tensor 2 Float)
    (let ((M N) (size x))
    (build (tuple N M) (lam (ij : Tuple Integer Integer)
        (let ((i j) ij)
            (index (tuple j i) x))))))


(def aten::lt Bool ((a : Float) (b : Float))
    (lt a b))

(def aten::lt Bool ((a : Integer) (b : Float))
    (lt (to_float a) b))


;; mul
(def aten::mul Float ((a : Float) (b : Float))
    (mul a b))

(def aten::mul (Tensor 1 Float) ((a : Tensor 1 Float) (b : Float))
    (ts_scale b a))

(def aten::mul (Tensor 2 Float) ((a : Tensor 2 Float) (b : Float))
    (ts_scale b a))

(def aten::mul (Tensor 2 Float) ((a : Float) (b : Tensor 2 Float))
    (ts_scale a b))

(def aten::mul (Tensor 2 Float) ((a : Tensor 2 Float) (b : Tensor 2 Float))
    (build (size a) (lam (inds : Tuple Integer Integer)
        (mul (index inds a) (index inds b)))))

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

(def aten::Bool Bool (a : Bool)
    a)
(def aten::Bool Bool (a : Float)
    (not (eq a 0.0)))

(def aten::sin (Tensor 2 Float) (a : Tensor 2 Float)
    (build (size a) (lam (ij : Tuple Integer Integer)
        (sin (index ij a)))))

(def aten::tanh (Tensor 2 Float) (a : Tensor 2 Float)
    (build (size a) (lam (ij : Tuple Integer Integer)
        (tanh (index ij a)))))

;; a^n
(edef aten::pow (Tensor 2 Float) ((Tensor 2 Float) Integer))
(def shape$aten::pow (Tensor 2 (Tuple)) ((a : Tensor 2 Float) (n : Integer))
    (shape a))
(edef D$aten::pow (LM (Tuple (Tensor 2 Float) Integer) (Tensor 2 Float)) ((Tensor 2 Float) Integer))

;; n*a^(n - 1) * dr
(def rev$aten::pow (Tuple (Tensor 2 Float) (Tuple)) ((a_n : Tuple (Tensor 2 Float) Integer) (dr : Tensor 2 Float))
    (let ((a n) a_n)
    (let (nanm1 (ts_scale (to_float n) (aten::pow a (sub n 1))))
    (tuple (aten::mul nanm1 dr)
       (tuple)))))

(def fwd$aten::pow (Tensor 2 Float) ((a_n : Tuple (Tensor 2 Float) Integer) (da_n : (Tuple (Tensor 2 Float) (Tuple))))
    (let ((a n) a_n)
    (let ((da dn) da_n)
    (let (nanm1 (ts_scale (to_float n) (aten::pow a (sub n 1))))
    (aten::mul nanm1 da)))))

(def aten::prod Float (a : Tuple Float Float)
    (mul (get$1$2 a) (get$2$2 a)))

(def aten::prod Integer (a : Tuple Integer Integer)
    (mul (get$1$2 a) (get$2$2 a)))

(def aten::sum Float (a : Tensor 2 Float)
    (sumbuild (size a) (lam (ij : Tuple Integer Integer)
            (index ij a))))

(def aten::mean Float ((a : Tensor 2 Float) (opt_dtype : Float))
    (div (aten::sum a) (aten::Float (aten::prod (size a)))))

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


(edef aten::matmul (Tensor 2 Float) ((Tensor 2 Float) (Tensor 2 Float)))
(def shape$aten::matmul (Tensor 2 (Tuple)) ((A : (Tensor 2 Float)) (B : (Tensor 2 Float)))
    (let ((M N) (size A))
    (let ((N1 P) (size B))
    (assert (eq N N1)
        (constVec (tuple M P) (tuple))))))

(edef D$aten::matmul (LM (Tuple (Tensor 2 Float) (Tensor  2 Float)) 
                         (Tensor  2 Float))
          ((Tensor 2 Float) (Tensor  2 Float)))
(def fwd$aten::matmul (Tensor 2 Float)
          ((A_B : (Tuple (Tensor 2 Float) (Tensor 2 Float))) 
           (dA_dB : (Tuple (Tensor 2 Float) (Tensor 2 Float))))
     (let ((A B) A_B)
     (let ((dA dB) dA_dB)
        (ts_add (aten::matmul dA B) (aten::matmul A dB)))))

; dR = A*dB + dA*B
; [dA, dB] = [dR * B^T, A^T * dR]
(def rev$aten::matmul (Tuple (Tensor 2 Float) (Tensor  2 Float))
          ((AB : Tuple (Tensor 2 Float) (Tensor 2 Float)) (dR : Tensor  2 Float))
    (let ((A B) AB)
        (tuple (aten::matmul dR (transpose B)) (aten::matmul (transpose A) dR))))

; (def addA1bt (Tensor 2 Float) ((A : Tensor 2 Float) (b : Tensor 1 Float))
;     (let ((M N) (size A))
;     (assert (eq N (size b))
;         (build (tuple M N) (lam (ij : Tuple Integer Integer)
;             (let ((i j) ij)
;                 (add (index (tuple i j) A) (index j b))))))))
                
(edef addA1bt (Tensor 2 Float) ((Tensor 2 Float) (Tensor 1 Float)))
(def shape$addA1bt (Tensor 2 (Tuple)) ((a : Tensor 2 Float) (b : Tensor 1 Float))
    (shape a))
(edef D$addA1bt (LM (Tuple (Tensor 2 Float) (Tensor 1 Float)) (Tensor 2 Float)) ((Tensor 2 Float) (Tensor 1 Float)))
(def fwd$addA1bt (Tensor 2 Float) ((arg : Tuple (Tensor 2 Float) (Tensor 1 Float)) (darg : Tuple (Tensor 2 Float) (Tensor 1 Float)))
    (let ((dA db) darg)
     (addA1bt dA db)))
(edef rev$addA1bt (Tuple (Tensor 2 Float) (Tensor 1 Float)) ((Tuple (Tensor 2 Float) (Tensor 1 Float)) (Tensor 2 Float)))
(def shape$rev$addA1bt (Tuple (Tensor 2 (Tuple)) (Tensor 1 (Tuple))) ((a : Tuple (Tensor 2 Float) (Tensor 1 Float)) (dret : Tensor 2 Float))
    (shape a))

; Applies a linear transformation to the incoming data: :math:`y = X A^T + b`.

; This operator supports :ref:`TensorFloat32<tf32_on_ampere>`.

; Shape:

;     - x: Input: :math:`(N, *, F)`
;          N is the batch size, `*` means any number of additional dimensions
;     - A: Weight: :math:`(O, F)`
;     - b: Bias: :math:`(O)`
;     - Output: :math:`(N, *, O)`

(def linear (Tensor 2 Float) 
    ((X : Tensor 2 Float) (A : Tensor 2 Float) (b : Tensor 1 Float))
      (addA1bt (aten::matmul X (transpose A)) b))
    

(def aten::dot Float ((a : Tensor 1 Float) (b : Tensor 1 Float))
    (ts_dot a b))

(def aten::size (Tuple Integer Integer) (a : Tensor 2 Float)
    (size a))

(def aten::len Integer (a : Tuple Integer Integer)
    2)

;; cat, transpose
(edef aten::__getitem__ (Tensor 2 Float) ((Tensor 1 (Tensor 2 Float)) Integer))
(edef D$aten::__getitem__ (LM (Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float)) ((Tensor 1 (Tensor 2 Float)) Integer))
; (def rev$aten::__getitem__ (Tuple (Tensor 1 (Tensor 2 Float)) Integer) ((t : (Tuple (Tensor 1 (Tensor 2 Float)) Integer)) 
;                                                                          (dret : (Tensor 2 Float)))
;    )

(edef aten::cat (Tensor 2 Float) ((Tensor 1 (Tensor 2 Float)) Integer))
(edef shape$aten::cat (Tensor 2 (Tuple)) ((Tensor 1 (Tensor 2 Float)) Integer))
(edef D$aten::cat (LM (Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float)) ((Tensor 1 (Tensor 2 Float)) Integer))
(def fwd$aten::cat (Tensor 2 Float) ((as_i : Tuple (Tensor 1 (Tensor 2 Float)) Integer) (da : Tuple (Tensor 1 (Tensor 2 Float)) (Tuple)))
    (let ((as i) as_i)
    (let ((das _) da)
      (aten::cat das i))))
(edef rev$aten::cat (Tuple (Tensor 1 (Tensor 2 Float)) (Tuple)) ((Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float)))
(edef shape$rev$aten::cat (Tuple (Tensor 1 (Tensor 2 (Tuple))) (Tuple)) ((Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float)))


; Splits a tensor into a specific number of chunks. Each chunk is a view of the input tensor.
; Last chunk will be smaller if the tensor size along the given dimension dim is not divisible by chunks.
; Parameters
; input (Tensor) – the tensor to split
; chunks (int) – number of chunks to return
; dim (int) – dimension along which to split the tensor
(edef aten::chunk (Vec (Tensor 2 Float)) ((Tensor 2 Float) Integer Integer))
(edef D$aten::chunk (LM (Tuple (Tensor 2 Float) Integer Integer) (Vec (Tensor 2 Float))) ((Tensor 2 Float) Integer Integer))
; (def rev$aten::chunk (Tuple (Tensor 2 Float) Integer Integer) ((t : (Tuple (Tensor 2 Float) Integer Integer)) (dret : (Vec (Tensor 2 Float))))

(edef aten::sigmoid (Tensor 2 Float) ((Tensor 2 Float)))
(edef D$aten::sigmoid (LM (Tensor 2 Float) (Tensor 2 Float)) ((Tensor 2 Float)))
; (def rev$aten::sigmoid (Tuple (Tensor 2 Float)) ((t : (Tuple (Tensor 2 Float))) (dret : (Tensor 2 Float)))
;    )

; https://pytorch.org/docs/stable/_modules/torch/nn/functional.html#elu
;     :math:`\text{ELU}(x) = \max(0,x) + \min(0, \alpha * (\exp(x) - 1))`.
(edef elu (Tensor 2 Float) ((Tensor 2 Float) Float Bool))
(edef D$elu (LM (Tuple (Tensor 2 Float) Float Bool) (Tensor 2 Float)) ((Tensor 2 Float) Float Bool))
; (def rev$elu (Tuple (Tensor 2 Float) Float Integer) ((t : (Tuple (Tensor 2 Float) Float Bool)) (dret : (Tensor 2 Float)))
;    )

; https://pytorch.org/docs/stable/generated/torch.add.html#torch.add
(def aten::add (Tensor 2 Float) ((a : Tensor 2 Float) (b : Tensor 2 Float) (alpha : Integer))
    (ts_add a (ts_scale (to_float alpha) b)))
