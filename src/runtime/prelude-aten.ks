;; Definitions for aten functions
;; edefs will go in prelude-aten.cpp


(def transpose (Tensor 2 Float) (x : Tensor 2 Float)
    (let ((M N) (size x))
    (build (tuple N M) (lam (ij : Tuple Integer Integer)
        (let ((i j) ij)
            (index (tuple j i) x))))))

(def aten::item Float (x : Float)
    x)


(def aten::lt Bool ((a : Float) (b : Float))
    (lt a b))
(gdef fwd [aten::lt (Tuple Float Float)])
(gdef rev [aten::lt (Tuple Float Float)])

(def aten::lt Bool ((a : Integer) (b : Float))
    (lt (to_float a) b))
(gdef fwd [aten::lt (Tuple Integer Float)])
(gdef rev [aten::lt (Tuple Integer Float)])

;; mul
(def aten::mul Float ((a : Float) (b : Float))
    (mul a b))
(gdef fwd [aten::mul (Tuple Float Float)])
(gdef rev [aten::mul (Tuple Float Float)])

(def aten::mul Float ((a : Float) (b : Integer))
    (mul a (to_float b)))

(def aten::mul (Tensor 1 Float) ((a : Tensor 1 Float) (b : Float))
    (ts_scale b a))
(gdef fwd [aten::mul (Tuple (Tensor 1 Float) Float)])
(gdef rev [aten::mul (Tuple (Tensor 1 Float) Float)])

(def aten::mul (Tensor 2 Float) ((a : Tensor 2 Float) (b : Float))
    (ts_scale b a))
(gdef fwd [aten::mul (Tuple (Tensor 2 Float) Float)])
(gdef rev [aten::mul (Tuple (Tensor 2 Float) Float)])

(def aten::mul (Tensor 2 Float) ((a : Float) (b : Tensor 2 Float))
    (ts_scale a b))

(def aten::mul (Tensor 1 Float) ((a : Tensor 1 Float) (b : Tensor 1 Float))
    (build (size a) (lam (inds : Integer )
        (mul (index inds a) (index inds b)))))

(def aten::mul (Tensor 2 Float) ((a : Tensor 2 Float) (b : Tensor 2 Float))
    (build (size a) (lam (inds : Tuple Integer Integer)
        (mul (index inds a) (index inds b)))))
(gdef fwd [aten::mul (Tuple (Tensor 2 Float) (Tensor 2 Float))])
(gdef rev [aten::mul (Tuple (Tensor 2 Float) (Tensor 2 Float))])

;; add
(def aten::add Float ((a : Float) (b : Float))
    (add a b))
(gdef fwd [aten::add (Tuple Float Float)])
(gdef rev [aten::add (Tuple Float Float)])

(def aten::add Integer ((a : Integer) (b : Integer))
    (add a b))

;; div
(def aten::div Integer ((a : Integer) (b : Integer))
    (div  a b))

(def aten::div Float ((a : Float) (b : Float))
    (div  a b))

;; neg
(def aten::neg Float (a : Float)
    (neg a))
(gdef fwd [aten::neg Float])
(gdef rev [aten::neg Float])

(def aten::sin Float (a : Float)
    (sin a))
(gdef fwd [aten::sin Float])
(gdef rev [aten::sin Float])

(def aten::Float Float (a : Integer)
    (to_float a))
(gdef fwd [aten::Float Integer])
(gdef rev [aten::Float Integer])

(def aten::Float Float (a : Float)
    a)
(gdef fwd [aten::Float Float])
(gdef rev [aten::Float Float])

(def aten::Bool Bool (a : Bool)
    a)
(gdef fwd [aten::Bool Bool])
(gdef rev [aten::Bool Bool])

(def aten::Bool Bool (a : Float)
    (not (eq a 0.0)))
(gdef fwd [aten::Bool Float])
(gdef rev [aten::Bool Float])

(def aten::sin (Tensor 2 Float) (a : Tensor 2 Float)
    (build (size a) (lam (ij : Tuple Integer Integer)
        (sin (index ij a)))))
(gdef fwd [aten::sin (Tensor 2 Float)])
(gdef rev [aten::sin (Tensor 2 Float)])

(def aten::sin (Tensor 1 Float) (a : Tensor 1 Float)
    (build (size a) (lam (ij : Integer)
        (sin (index ij a)))))

(def aten::tanh (Tensor 2 Float) (a : Tensor 2 Float)
    (build (size a) (lam (ij : Tuple Integer Integer)
        (tanh (index ij a)))))

;; a^n
(def aten::pow Float ((a : Float) (n : Integer))
    (pow a n))

(def aten::prod Float (a : Tuple Float Float)
    (mul (get$1$2 a) (get$2$2 a)))
(gdef fwd [aten::prod (Tuple Float Float)])
(gdef rev [aten::prod (Tuple Float Float)])

(def aten::prod Integer (a : Tuple Integer Integer)
    (mul (get$1$2 a) (get$2$2 a)))
(gdef fwd [aten::prod (Tuple Integer Integer)])
(gdef rev [aten::prod (Tuple Integer Integer)])

(def aten::sum Float (a : Tensor 2 Float)
    (sumbuild (size a) (lam (ij : Tuple Integer Integer)
            (index ij a))))
(gdef rev [aten::sum (Tensor 2 Float)])
(gdef fwd [aten::sum (Tensor 2 Float)])

(def aten::sum Float (a : Tensor 1 Float)
    (sumbuild (size a) (lam (ij : Integer)
            (index ij a))))

(def aten::mean Float ((a : Tensor 2 Float) (opt_dtype : (Tuple)))
    (div (aten::sum a) (aten::Float (aten::prod (size a)))))
(gdef fwd [aten::mean (Tuple (Tensor 2 Float) (Tuple))])
(gdef rev [aten::mean (Tuple (Tensor 2 Float) (Tuple))])

(def aten::mean Float ((a : Tensor 1 Float) (opt_dtype : (Tuple)))
    (div (aten::sum a) (aten::Float (size a))))

(def Tensor_init (Tensor 2 Float) ((a : Vec (Vec Float)))
    (let (m (size a))
    (let (n (size (index 0 a)))
    (build (tuple m n) (lam (ij : Tuple Integer Integer)
        (let ((i j) ij)
            (index j (index i a))))))))
(gdef fwd [Tensor_init (Vec (Vec Float))])
(gdef rev [Tensor_init (Vec (Vec Float))])

(def VecVec_init (Vec (Vec Float)) (a : Tensor 2 Float)
    (let ((m n) (size a))
    (build m (lam (i : Integer)
        (build n (lam (j : Integer)
            (index (tuple i j) a)))))))
(gdef fwd [VecVec_init (Tensor 2 Float)])
(gdef rev [VecVec_init (Tensor 2 Float)])

; (def [rev Tensor_init] (Vec (Vec Float)) ((a : Vec (Vec Float)) (dr : Tensor 2 Float))
;     (VecVec_init dr))
   
(def aten::tensor (Tensor 2 Float) ((a : Vec (Vec Float)) (x1 : Float) (x2 : Float) (x3 : Float) )
    (Tensor_init a))
(gdef fwd [aten::tensor (Tuple (Vec (Vec Float)) Float Float Float)])
(gdef rev [aten::tensor (Tuple (Vec (Vec Float)) Float Float Float)])

(def aten::tensor (Tensor 1 Float) ((a : Vec Float) (x1 : Float) (x2 : Float) (x3 : Float) )
    a)
(gdef fwd [aten::tensor (Tuple (Vec Float) Float Float Float)])
(gdef rev [aten::tensor (Tuple (Vec Float) Float Float Float)])

(def aten::tensor (Tensor 1 Float) ((a : Vec Float) (x1 : (Tuple)) (x2 : (Tuple)) (x3 : Bool) )
    a)

; mul Mat Vec
(edef aten::matmul (Tensor 1 Float) ((Tensor 2 Float) (Tensor 1 Float)))
(def [shape aten::matmul] (Tensor 1 (Tuple)) ((m : (Tensor 2 Float)) (v : (Tensor 1 Float)))
          (constVec (size v) (tuple)))

(edef [D aten::matmul] (LM (Tuple (Tensor 2 Float) (Tensor  1 Float)) (Tensor  1 Float))
          ((Tensor 2 Float) (Tensor  1 Float)))
(edef [Dt aten::matmul] (Tuple (Tensor  1 Float) (LM (Tuple (Tensor 2 Float) (Tensor  1 Float)) (Tensor  1 Float)))
          ((Tensor 2 Float) (Tensor  1 Float)))

(def [fwd aten::matmul] (Tensor 1 Float)
          ((M_v : (Tuple (Tensor 2 Float) (Tensor  1 Float))) (dM_dv : (Tuple (Tensor 2 Float) (Tensor  1 Float))))
     (let ((M v) M_v)
     (let ((dM dv) dM_dv)
        (ts_add (aten::matmul dM v) (aten::matmul M dv)))))

(edef [rev aten::matmul] (Tuple (Tensor 2 Float) (Tensor  1 Float))
          ((Tuple (Tensor 2 Float) (Tensor  1 Float)) (Tensor  1 Float)))


(edef aten::matmul (Tensor 2 Float) ((Tensor 2 Float) (Tensor 2 Float)))
(def [shape aten::matmul] (Tensor 2 (Tuple)) ((A : (Tensor 2 Float)) (B : (Tensor 2 Float)))
    (let ((M N) (size A))
    (let ((N1 P) (size B))
    (assert (eq N N1)
        (constVec (tuple M P) (tuple))))))

(edef [D aten::matmul] (LM (Tuple (Tensor 2 Float) (Tensor  2 Float)) 
                         (Tensor  2 Float))
          ((Tensor 2 Float) (Tensor  2 Float)))
(edef [Dt aten::matmul] (Tuple (Tensor  2 Float) (LM (Tuple (Tensor 2 Float) (Tensor  2 Float)) (Tensor  2 Float)))
          ((Tensor 2 Float) (Tensor  2 Float)))
(def [fwd aten::matmul] (Tensor 2 Float)
          ((A_B : (Tuple (Tensor 2 Float) (Tensor 2 Float))) 
           (dA_dB : (Tuple (Tensor 2 Float) (Tensor 2 Float))))
     (let ((A B) A_B)
     (let ((dA dB) dA_dB)
        (ts_add (aten::matmul dA B) (aten::matmul A dB)))))

; dR = A*dB + dA*B
; [dA, dB] = [dR * B^T, A^T * dR]
(def [rev aten::matmul] (Tuple (Tensor 2 Float) (Tensor  2 Float))
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
(def [shape addA1bt] (Tensor 2 (Tuple)) ((a : Tensor 2 Float) (b : Tensor 1 Float))
    (shape a))
(edef [D addA1bt] (LM (Tuple (Tensor 2 Float) (Tensor 1 Float)) (Tensor 2 Float)) ((Tensor 2 Float) (Tensor 1 Float)))
(edef [Dt addA1bt] (Tuple (Tensor 2 Float) (LM (Tuple (Tensor 2 Float) (Tensor 1 Float)) (Tensor 2 Float))) ((Tensor 2 Float) (Tensor 1 Float)))
(def [fwd addA1bt] (Tensor 2 Float) ((arg : Tuple (Tensor 2 Float) (Tensor 1 Float)) (darg : Tuple (Tensor 2 Float) (Tensor 1 Float)))
    (let ((dA db) darg)
     (addA1bt dA db)))
(edef [rev addA1bt] (Tuple (Tensor 2 Float) (Tensor 1 Float)) ((Tuple (Tensor 2 Float) (Tensor 1 Float)) (Tensor 2 Float)))
(def [shape [rev addA1bt]] (Tuple (Tensor 2 (Tuple)) (Tensor 1 (Tuple))) ((a : Tuple (Tensor 2 Float) (Tensor 1 Float)) (dret : Tensor 2 Float))
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
(gdef fwd [aten::dot (Tuple (Tensor 1 Float) (Tensor 1 Float))])
(gdef rev [aten::dot (Tuple (Tensor 1 Float) (Tensor 1 Float))])

(def aten::size (Tuple Integer Integer) (a : Tensor 2 Float)
    (size a))
(gdef fwd [aten::size (Tensor 2 Float)])
(gdef rev [aten::size (Tensor 2 Float)])

(def aten::len Integer (a : Tuple Integer Integer)
    2)
(gdef fwd [aten::len (Tuple Integer Integer)])
(gdef rev [aten::len (Tuple Integer Integer)])


(def aten::len Integer (a : Tensor 1 Float)
    (size a))
(gdef fwd [aten::len (Tensor 1 Float)])
(gdef rev [aten::len (Tensor 1 Float)])

(edef prim::min Integer ((Tensor 1 Integer)))
(def [fwd [prim::min (Tensor 1 Integer)]] (Tuple) ((a : Tensor 1 Integer) (da : Tensor 1 (Tuple)))
    (tuple))
(def [rev [prim::min (Tensor 1 Integer)]] (Tensor 1 (Tuple)) ((a : Tensor 1 Integer) (dret : (Tuple)))
    (constVec (size a) (tuple)))

(def aten::select Float ((a : Tensor 1 Float) (dim : Integer) (i : Integer))
    (assert (eq dim 0)
        (index i a)))

;; cat, transpose
(edef aten::__getitem__ (Tensor 2 Float) ((Tensor 1 (Tensor 2 Float)) Integer))
(edef [D aten::__getitem__] (LM (Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float)) ((Tensor 1 (Tensor 2 Float)) Integer))
; (def [rev aten::__getitem__] (Tuple (Tensor 1 (Tensor 2 Float)) Integer) ((t : (Tuple (Tensor 1 (Tensor 2 Float)) Integer)) 
;                                                                          (dret : (Tensor 2 Float)))
;    )

(edef aten::cat (Tensor 2 Float) ((Tensor 1 (Tensor 2 Float)) Integer))
(edef [shape aten::cat] (Tensor 2 (Tuple)) ((Tensor 1 (Tensor 2 Float)) Integer))
(edef [D aten::cat] (LM (Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float)) ((Tensor 1 (Tensor 2 Float)) Integer))
(edef [Dt aten::cat] (Tuple (Tensor 2 Float) (LM (Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float))) ((Tensor 1 (Tensor 2 Float)) Integer))
(def [fwd aten::cat] (Tensor 2 Float) ((as_i : Tuple (Tensor 1 (Tensor 2 Float)) Integer) (da : Tuple (Tensor 1 (Tensor 2 Float)) (Tuple)))
    (let ((as i) as_i)
    (let ((das _) da)
      (aten::cat das i))))
(edef [rev aten::cat] (Tuple (Tensor 1 (Tensor 2 Float)) (Tuple)) ((Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float)))
(edef [shape [rev aten::cat]] (Tuple (Tensor 1 (Tensor 2 (Tuple))) (Tuple)) ((Tuple (Tensor 1 (Tensor 2 Float)) Integer) (Tensor 2 Float)))


; Splits a tensor into a specific number of chunks. Each chunk is a view of the input tensor.
; Last chunk will be smaller if the tensor size along the given dimension dim is not divisible by chunks.
; Parameters
; input (Tensor) – the tensor to split
; chunks (int) – number of chunks to return
; dim (int) – dimension along which to split the tensor
(edef aten::chunk (Vec (Tensor 2 Float)) ((Tensor 2 Float) Integer Integer))
(edef [D aten::chunk] (LM (Tuple (Tensor 2 Float) Integer Integer) (Vec (Tensor 2 Float))) ((Tensor 2 Float) Integer Integer))
; (def [rev aten::chunk] (Tuple (Tensor 2 Float) Integer Integer) ((t : (Tuple (Tensor 2 Float) Integer Integer)) (dret : (Vec (Tensor 2 Float))))

(edef aten::sigmoid (Tensor 2 Float) ((Tensor 2 Float)))
(edef [D aten::sigmoid] (LM (Tensor 2 Float) (Tensor 2 Float)) ((Tensor 2 Float)))
; (def [rev aten::sigmoid] (Tuple (Tensor 2 Float)) ((t : (Tuple (Tensor 2 Float))) (dret : (Tensor 2 Float)))
;    )

; https://pytorch.org/docs/stable/_modules/torch/nn/functional.html#elu
;     :math:`\text{ELU}(x) = \max(0,x) + \min(0, \alpha * (\exp(x) - 1))`.
(edef elu (Tensor 2 Float) ((Tensor 2 Float) Float Bool))
(edef [D elu] (LM (Tuple (Tensor 2 Float) Float Bool) (Tensor 2 Float)) ((Tensor 2 Float) Float Bool))
; (def [rev elu] (Tuple (Tensor 2 Float) Float Integer) ((t : (Tuple (Tensor 2 Float) Float Bool)) (dret : (Tensor 2 Float)))
;    )

; https://pytorch.org/docs/stable/generated/torch.add.html#torch.add
(def aten::add (Tensor 2 Float) ((a : Tensor 2 Float) (b : Tensor 2 Float) (alpha : Integer))
    (ts_add a (ts_scale (to_float alpha) b)))
