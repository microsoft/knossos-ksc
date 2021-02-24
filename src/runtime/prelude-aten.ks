;; Definitions for aten functions
;; edefs will go in prelude-aten.cpp

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

(def aten::mul (Tensor 1 Float) ((a : Tensor 1 Float) (b : Float))
    (ts_scale b a))
(gdef fwd [aten::mul (Tuple (Tensor 1 Float) Float)])
(gdef rev [aten::mul (Tuple (Tensor 1 Float) Float)])

(def aten::mul (Tensor 2 Float) ((a : Tensor 2 Float) (b : Float))
    (ts_scale b a))
(gdef fwd [aten::mul (Tuple (Tensor 2 Float) Float)])
(gdef rev [aten::mul (Tuple (Tensor 2 Float) Float)])

(def aten::mul (Tensor 2 Float) ((a : Tensor 2 Float) (b : Tensor 2 Float))
    (build (size a) (lam (inds : Tuple Integer Integer)
        (mul (index inds a) (index inds b)))))
(gdef fwd [aten::mul (Tuple (Tensor 2 Float) (Tensor 2 Float))])
(gdef rev [aten::mul (Tuple (Tensor 2 Float) (Tensor 2 Float))])

(def aten::add Float ((a : Float) (b : Float))
    (add a b))
(gdef fwd [aten::add (Tuple Float Float)])
(gdef rev [aten::add (Tuple Float Float)])

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

;; a^n
(edef aten::pow (Tensor 2 Float) ((Tensor 2 Float) Integer))
(def [shape aten::pow] (Tensor 2 (Tuple)) ((a : Tensor 2 Float) (n : Integer))
    (shape a))
(edef [D aten::pow] (LM (Tuple (Tensor 2 Float) Integer) (Tensor 2 Float)) ((Tensor 2 Float) Integer))
(edef [Dt aten::pow] (Tuple (Tensor 2 Float) (LM (Tuple (Tensor 2 Float) Integer) (Tensor 2 Float))) ((Tensor 2 Float) Integer))

;; n*a^(n - 1) * dr
(def [rev aten::pow] (Tuple (Tensor 2 Float) (Tuple)) ((a_n : Tuple (Tensor 2 Float) Integer) (dr : Tensor 2 Float))
    (let ((a n) a_n)
    (let (nanm1 (ts_scale (to_float n) (aten::pow a (sub n 1))))
    (tuple (aten::mul nanm1 dr)
       (tuple)))))

(def [fwd aten::pow] (Tensor 2 Float) ((a_n : Tuple (Tensor 2 Float) Integer) (da_n : (Tuple (Tensor 2 Float) (Tuple))))
    (let ((a n) a_n)
    (let ((da dn) da_n)
    (let (nanm1 (ts_scale (to_float n) (aten::pow a (sub n 1))))
    (aten::mul nanm1 da)))))

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

(def aten::mean Float ((a : Tensor 2 Float) (opt_dtype : Float))
    (div (aten::sum a) (aten::Float (aten::prod (size a)))))
(gdef fwd [aten::mean (Tuple (Tensor 2 Float) Float)])
(gdef rev [aten::mean (Tuple (Tensor 2 Float) Float)])

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

(def xrev$Tensor_init (Vec (Vec Float)) ((a : Vec (Vec Float)) (dr : Tensor 2 Float))
    (VecVec_init dr))
   
(def aten::tensor (Tensor 2 Float) ((a : Vec (Vec Float)) (x1 : Float) (x2 : Float) (x3 : Float) )
    (Tensor_init a))
(gdef fwd [aten::tensor (Tuple (Vec (Vec Float)) Float Float Float)])
(gdef rev [aten::tensor (Tuple (Vec (Vec Float)) Float Float Float)])

(def aten::tensor (Tensor 1 Float) ((a : Vec Float) (x1 : Float) (x2 : Float) (x3 : Float) )
    a)
(gdef fwd [aten::tensor (Tuple (Vec Float) Float Float Float)])
(gdef rev [aten::tensor (Tuple (Vec Float) Float Float Float)])

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

