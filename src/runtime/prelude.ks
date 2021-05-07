
;;; Type conversions

;; to_float
(edef to_float Float (Integer))
(edef [D to_float] (LM Integer Float) (Integer))
(def [fwd to_float] Float ((x : Integer) (dx : (Tuple))) 0.0)
(def [rev to_float] (Tuple) ((x : Integer) (d_dto_float : Float)) (tuple))
(edef [Dt to_float] (Tuple Float (LM Integer Float)) (Integer))
(def [suffwdpass to_float] (Tuple Float (Tuple)) (x : Integer)
     (tuple (to_float x) (tuple)))
(def [sufrevpass [to_float Integer]] (Tuple) ((d_dto_float : Float) (bog : Tuple)) (tuple))

;; not :: Bool -> Bool
(def not Bool (p : Bool) (if p false true))
(gdef fwd [not Bool])
(gdef rev [not Bool])
(gdef suffwdpass [not Bool])
(gdef sufrevpass [not Bool])
(gdef sufrev [not Bool])

;; neg :: Number -> Number
;; neg x = -x
(edef neg Float (Float))
(edef [D neg] (LM Float Float) (Float))
(edef [Dt neg] (Tuple Float (LM Float Float)) (Float))
(def [fwd neg] Float ((x : Float) (dx : Float))
     (neg dx))
(def [rev neg] Float ((x : Float) (d_dneg : Float))
     (neg d_dneg))

(def [suffwdpass neg] (Tuple Float (Tuple)) (t : Float)
     (tuple (neg t) (tuple)))
(def [sufrevpass [neg Float]] Float ((d_dneg : Float) (bog : Tuple))
     (neg d_dneg))

(edef neg Integer (Integer))
(edef [D neg] (LM Integer Integer) (Integer))
(edef [Dt neg] (Tuple Integer (LM Integer Integer)) (Integer))
(def [fwd neg] (Tuple) ((x : Integer) (dx : (Tuple)))
     (tuple))
(def [rev neg] (Tuple) ((x : Integer) (d_dneg : (Tuple)))
     (tuple))

(def [suffwdpass neg] (Tuple Integer (Tuple)) (t : Integer)
     (tuple (neg t) (tuple)))
(def [sufrevpass [neg Integer]] (Tuple) (t : Tuple (Tuple) (Tuple))
     (tuple))

;; add :: Number x Number -> Number
;; add (x, y) = x + y
(edef add Float (Tuple Float Float))
(edef [D add] (LM (Tuple Float Float) Float) (Tuple Float Float))
(edef [Dt add] (Tuple Float (LM (Tuple Float Float) Float)) (Tuple Float Float))
(def
 [fwd add] Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let ((dx1 dx2) dxt)
  (add dx1 dx2)))
(def
 [rev add] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  (d_dadd drt)
  (tuple d_dadd d_dadd)))

(def [suffwdpass add] (Tuple Float (Tuple)) (x_y : Tuple Float Float)
     (tuple (add x_y) (tuple)))
(def [sufrevpass [add (Tuple Float Float)]] (Tuple Float Float) ((d_dadd : Float) (bog : Tuple))
     (tuple d_dadd d_dadd))

(edef add Integer (Tuple Integer Integer))
(edef [D add] (LM (Tuple Integer Integer) Integer) (Tuple Integer Integer))
(edef [Dt add] (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Tuple Integer Integer))
(def
 [fwd add] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 [rev add] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(def [suffwdpass add] (Tuple Integer (Tuple)) (x_y : Tuple Integer Integer)
     (tuple (add x_y) (tuple)))
(def [sufrevpass [add (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple)) ((d_dadd : Tuple) (bog : Tuple))
     (tuple (tuple) (tuple)))

;; sub :: Number x Number -> Number
;; sub (x, y) = x - y
(edef sub Float (Tuple Float Float))
(edef [D sub] (LM (Tuple Float Float) Float) (Tuple Float Float))
(edef [Dt sub] (Tuple Float (LM (Tuple Float Float) Float)) (Tuple Float Float))
(def
 [fwd sub] Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((dx1 dx2) dxt)
  (sub dx1 dx2)))
(def
 [rev sub] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  (d_dsub drt)
  (tuple d_dsub (neg d_dsub))))

(def [suffwdpass sub] (Tuple Float (Tuple)) (x_y : Tuple Float Float)
     (tuple (sub x_y) (tuple)))
(def [sufrevpass [sub (Tuple Float Float)]] (Tuple Float Float) ((d_dsub : Float) (bog : Tuple))
     (tuple d_dsub (neg d_dsub)))

(edef sub Integer (Tuple Integer Integer))
(edef [D sub] (LM (Tuple Integer Integer) Integer) (Tuple Integer Integer))
(edef [Dt sub] (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Tuple Integer Integer))
(def
 [fwd sub] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 [rev sub] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(def [suffwdpass sub] (Tuple Integer (Tuple)) (x_y : Tuple Integer Integer)
     (tuple (sub x_y) (tuple)))
(def [sufrevpass [sub (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple)) ((d_dsub : (Tuple)) (bog : Tuple))
     (tuple (tuple) (tuple)))

(def sub (Tensor 1 Float) ((a : (Tensor 1 Float)) (b : (Tensor 1 Float)))
  (build (size a) (lam (i : Integer) (sub (index i a) (index i b)))))

(gdef fwd [sub (Tuple (Tensor 1 Float) (Tensor 1 Float))])
(gdef rev [sub (Tuple (Tensor 1 Float) (Tensor 1 Float))])
(gdef suffwdpass [sub (Tuple (Tensor 1 Float) (Tensor 1 Float))])
(gdef sufrevpass [sub (Tuple (Tensor 1 Float) (Tensor 1 Float))])

(def sub (Tensor 1 Float) ((a : (Tensor 1 Float)) (b : Float))
  (build (size a) (lam (i : Integer) (sub (index i a) b))))

(gdef fwd [sub (Tuple (Tensor 1 Float) Float)])
(gdef rev [sub (Tuple (Tensor 1 Float) Float)])
(gdef suffwdpass [sub (Tuple (Tensor 1 Float) Float)])
(gdef sufrevpass [sub (Tuple (Tensor 1 Float) Float)])

;; mul :: Number x Number -> Number
;; mul (x, y) = x * y
(edef mul Float (Tuple Float Float))
(edef [D mul] (LM (Tuple Float Float) Float) (Tuple Float Float))
(edef [Dt mul] (Tuple Float (LM (Tuple Float Float) Float)) (Tuple Float Float))
(def [fwd mul] Float ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
      (let ((x1 x2) xt)
      (let ((dx1 dx2) dxt)
        (add (mul x2 dx1) (mul x1 dx2)))))

(def
 [rev mul] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let ((x1 x2) xt)
 (let (d_dmul drt)
  (tuple (mul d_dmul x2) (mul d_dmul x1)))))

(def [suffwdpass mul] (Tuple Float (Tuple Float Float)) (x_y : Tuple Float Float)
     (tuple (mul x_y) x_y))
(def [sufrevpass [mul (Tuple Float Float)]] (Tuple Float Float) ((d_dmul : Float) (x_y : Tuple Float Float))
     (let ((x y) x_y)
       (tuple (mul y d_dmul) (mul x d_dmul))))

(edef mul Integer (Tuple Integer Integer))
(edef [D mul] (LM (Tuple Integer Integer) Integer) (Tuple Integer Integer))
(edef [Dt mul] (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Tuple Integer Integer))
(def
 [fwd mul] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 [rev mul] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(def [suffwdpass mul] (Tuple Integer (Tuple)) (x_y : Tuple Integer Integer)
     (tuple (mul x_y) (tuple)))
(def [sufrevpass [mul (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple)) ((d_dmul : Tuple) (bog : Tuple))
     (tuple (tuple) (tuple)))


;; mul Scalar Vec
(def mymul (Tensor 1 Float) ((r : Float) (a : Tensor 1 Float))
    (build (size a) (lam (i : Integer) (mul r (index i a)))))

(def mul Float ((a : Float) (b : Integer))
  (mul a (to_float b)))

;; mul Scalar Vec
(def mul (Tensor 1 Float) ((r : Float) (a : Tensor 1 Float))
    (build (size a) (lam (i : Integer) (mul r (index i a)))))

;; mul Mat Vec
(edef mul (Vec Float) (Tuple (Tensor 2 Float) (Vec Float)))
(def [shape mul] (Vec (Tuple)) ((m : (Tensor 2 Float)) (v : (Vec Float)))
          (constVec (get$1$2 (size m)) (tuple)))

(edef [D mul] (LM (Tuple (Tensor 2 Float) (Vec Float)) (Vec Float))
          (Tuple (Tensor 2 Float) (Vec Float)))
(edef [Dt mul] (Tuple (Vec Float) (LM (Tuple (Tensor 2 Float) (Vec Float)) (Vec Float)))
          (Tuple (Tensor 2 Float) (Vec Float)))

(def [fwd mul] (Vec Float)
          ((M_v : (Tuple (Tensor 2 Float) (Vec Float))) (dM_dv : (Tuple (Tensor 2 Float) (Vec Float))))
     (let ((M v) M_v)
     (let ((dM dv) dM_dv)
        (ts_add (mul dM v) (mul M dv)))))

(edef [rev mul] (Tuple (Tensor 2 Float) (Vec Float))
          (Tuple (Tuple (Tensor 2 Float) (Vec Float)) (Vec Float)))

(def [suffwdpass mul] (Tuple (Vec Float) (Tuple (Tensor 2 Float) (Vec Float)))
     (t : Tuple (Tensor 2 Float) (Vec Float))
     (tuple (mul t) t))
(def [sufrevpass [mul (Tuple (Tensor 2 Float) (Vec Float))]]
     (Tuple (Tensor 2 Float) (Vec Float))
     ((d_dr : Vec Float) (bog : Tuple (Tensor 2 Float) (Vec Float)))
     ([rev mul] bog d_dr))

;; div :: Number x Number -> Number
;; div (x, y) = x / y
(edef div Float (Tuple Float Float))
(edef [D div] (LM (Tuple Float Float) Float) (Tuple Float Float))
(edef [Dt div] (Tuple Float (LM (Tuple Float Float) Float)) (Tuple Float Float))
(def
 [fwd div] Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let ((x1 x2) xt)
 (let ((dx1 dx2) dxt)
  (div (sub (mul x2 dx1)
                  (mul x1 dx2))
          (mul x2 x2)))))
(def
 [rev div] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let ((x1 x2) xt)
 (let (d_ddiv drt)
  (tuple (div d_ddiv x2)
         (neg (div (mul x1 d_ddiv)
                         (mul x2 x2)))))))

(def [suffwdpass div] (Tuple Float (Tuple Float Float)) (x1_x2 : Tuple Float Float)
     (tuple (div x1_x2) x1_x2))
(def [sufrevpass [div (Tuple Float Float)]] (Tuple Float Float) ((d_ddiv : Float) (bog_xt : Tuple Float Float))
     (let ((x1 x2) bog_xt)
       (tuple (div d_ddiv x2)
              (neg (div (mul x1 d_ddiv)
                        (mul x2 x2))))))

(edef div Integer (Tuple Integer Integer))
(edef [D div] (LM (Tuple Integer Integer) Integer) (Tuple Integer Integer))
(edef [Dt div] (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Tuple Integer Integer))
(def
 [fwd div] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 [rev div] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(def [suffwdpass div] (Tuple Integer (Tuple)) (x_y : Tuple Integer Integer)
     (tuple (div x_y) (tuple)))
(def [sufrevpass [div (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple)) ((d_ddiv : (Tuple)) (xt : (Tuple)))
     (tuple (tuple) (tuple)))

;; pow :: Number ^ Number -> Number
;; pow (x, y) = x ^ y
(edef pow Float (Tuple Float Integer))
(edef [D pow] (LM (Tuple Float Integer) Float) (Tuple Float Integer))
(edef [Dt pow] (Tuple Float (LM (Tuple Float Integer) Float)) (Tuple Float Integer))
(def
 [fwd pow] Float
 ((xt : (Tuple Float Integer)) (dxt : (Tuple Float (Tuple))))
 (let ((x n) xt)
 (let ((dx dn) dxt)
  (mul dx (pow x (sub n 1))))))

(def
 [rev pow] (Tuple Float (Tuple))
 ((xt : (Tuple Float Integer)) (dret : Float))
 (let ((x n) xt)
 (let (dx (mul dret (pow x (sub n 1))))
   (tuple dx (tuple)))))

; fwd (x,n) = x * x^(n-1), BOG: n * x^(n-1)
(def [suffwdpass pow] (Tuple Float Float)
 (xt : (Tuple Float Integer))
 (let ((x n) xt)
 (if (eq n 0)
      (tuple 1.0 0.0)
   (let (xnm1 (pow x (sub n 1)))
      (tuple (mul x xnm1) (mul (to_float n) xnm1))))))
; rev (bog, dpow) = dpow * bog  = dpow * (n * x^(n-1))
(def [sufrevpass [pow (Tuple Float Integer)]] Float ((dpow : Float) (bog : Float))
     (mul dpow bog))

; TODO: MOVEEQ 'eq' is primitive in Haskell at the moment
; ;; eq :: Number x Number -> Bool
; ;; eq (x, y) = x == y
; (edef eq Bool (Tuple Float Float))
; (edef [D eq] (LM (Tuple Float Float) Bool) (Tuple Float Float))
; (edef [Dt eq] (Tuple Bool (LM (Tuple Float Float) Bool)) (Tuple Float Float))
; (def
;  [fwd eq] (Tuple)
;  ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
;   (tuple))
; (def
;  [rev eq] (Tuple Float Float)
;  ((xt : (Tuple Float Float)) (drt : (Tuple)))
;   (tuple 0.0 0.0))

; (edef eq Bool (Tuple Integer Integer))
; (edef [D eq] (LM (Tuple Integer Integer) Bool) (Tuple Integer Integer))
; (edef [Dt eq] (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Tuple Integer Integer))
; (def
;  [fwd eq] (Tuple)
;  ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
;   (tuple))
; (def
;  [rev eq] (Tuple (Tuple) (Tuple))
;  ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
;   (tuple (tuple) (tuple)))

;; gt :: Number x Number -> Bool
;; gt (x, y) = x > y
(edef gt Bool (Tuple Float Float))
(edef [D gt] (LM (Tuple Float Float) Bool) (Tuple Float Float))
(edef [Dt gt] (Tuple Bool (LM (Tuple Float Float) Bool)) (Tuple Float Float))
(def
 [fwd gt] (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 [rev gt] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(def [suffwdpass gt] (Tuple Bool (Tuple)) (x_y : Tuple Float Float)
     (tuple (gt x_y) (tuple)))
(def [sufrevpass [gt (Tuple Float Float)]] (Tuple Float Float) ((d_dgt : Tuple) (bog : Tuple))
     (tuple 0.0 0.0))

(edef gt Bool (Tuple Integer Integer))
(edef [D gt] (LM (Tuple Integer Integer) Bool) (Tuple Integer Integer))
(edef [Dt gt] (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Tuple Integer Integer))
(def
 [fwd gt] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 [rev gt] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(def [suffwdpass gt] (Tuple Bool (Tuple)) (x_y : Tuple Integer Integer)
     (tuple (gt x_y) (tuple)))
(def [sufrevpass [gt (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple)) ((d_dgt : Tuple) (bog : Tuple))
     (tuple (tuple) (tuple)))

;; lt :: Number x Number -> Bool
;; lt (x, y) = x < y
(edef lt Bool (Tuple Float Float))
(edef [D lt] (LM (Tuple Float Float) Bool) (Tuple Float Float))
(edef [Dt lt] (Tuple Bool (LM (Tuple Float Float) Bool)) (Tuple Float Float))
(def
 [fwd lt] (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 [rev lt] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))
(def [suffwdpass lt] (Tuple Bool (Tuple)) (x_y : Tuple Float Float)
     (tuple (gt x_y) (tuple)))
(def [sufrevpass [lt (Tuple Float Float)]] (Tuple Float Float) ((d_dlt : Tuple) (bog : Tuple))
     (tuple 0.0 0.0))


(edef lt Bool (Tuple Integer Integer))
(edef [D lt] (LM (Tuple Integer Integer) Bool) (Tuple Integer Integer))
(edef [Dt lt] (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Tuple Integer Integer))
(def
 [fwd lt] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 [rev lt] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(def [suffwdpass lt] (Tuple Bool (Tuple)) (t : Tuple Integer Integer)
     (tuple (lt t) (tuple)))
(def [sufrevpass [lt (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple)) (t : Tuple (Tuple) (Tuple))
     (tuple (tuple) (tuple)))

;; lte :: Number x Number -> Bool
;; lte (x, y) = x <= y
(edef lte Bool (Tuple Float Float))
(edef [D lte] (LM (Tuple Float Float) Bool) (Tuple Float Float))
(edef [Dt lte] (Tuple Bool (LM (Tuple Float Float) Bool)) (Tuple Float Float))
(def
 [fwd lte] (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 [rev lte] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef lte Bool (Tuple Integer Integer))
(edef [D lte] (LM (Tuple Integer Integer) Bool) (Tuple Integer Integer))
(edef [Dt lte] (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Tuple Integer Integer))
(def
 [fwd lte] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 [rev lte] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; gte :: Number x Number -> Bool
;; gte (x, y) = x >= y
(edef gte Bool (Tuple Float Float))
(edef [D gte] (LM (Tuple Float Float) Bool) (Tuple Float Float))
(edef [Dt gte] (Tuple Bool (LM (Tuple Float Float) Bool)) (Tuple Float Float))
(def
 [fwd gte] (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 [rev gte] (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(def [suffwdpass [gte (Tuple Float Float)]] (Tuple Bool (Tuple)) (x_y : Tuple Float Float)
     (tuple (gte x_y) (tuple)))
(def [sufrevpass [gte (Tuple Float Float)]] (Tuple Float Float) ((d_dgte : Tuple) (bog : Tuple))
     (tuple 0.0 0.0))

(edef gte Bool (Tuple Integer Integer))
(edef [D gte] (LM (Tuple Integer Integer) Bool) (Tuple Integer Integer))
(edef [Dt gte] (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Tuple Integer Integer))
(def
 [fwd gte] (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 [rev gte] (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(def [suffwdpass gte] (Tuple Bool (Tuple)) (x_y : Tuple Integer Integer)
     (tuple (gte x_y) (tuple)))
(def [sufrevpass [gte (Tuple Integer Integer)]] (Tuple (Tuple) (Tuple)) ((d_dgte : Tuple) (bog : Tuple))
     (tuple (tuple) (tuple)))

(edef log Float (Float))
(edef [D log] (LM Float Float) (Float))
(def [fwd log] Float ((x : Float) (dx : Float)) (div dx x))
(def [rev log] Float ((x : Float) (d_dlog : Float)) (div d_dlog x))
(edef [Dt log] (Tuple Float (LM Float Float)) (Float))

(def [suffwdpass log] (Tuple Float Float) (x : Float)
     (tuple (log x) x))
(def [sufrevpass [log Float]] Float ((d_dlog : Float) (x : Float))
     ([rev log] x d_dlog))

(edef exp Float (Float))
(edef [D exp] (LM Float Float) (Float))
(def [fwd exp] Float ((x : Float) (dx : Float)) (mul (exp x) dx))
(def [rev exp] Float ((x : Float) (d_dexp : Float)) (mul (exp x) d_dexp))
(edef [Dt exp] (Tuple Float (LM Float Float)) (Float))

(def [suffwdpass exp] (Tuple Float Float) (x : Float)
     (let (exp_x (exp x))
       (tuple exp_x exp_x)))
(def [sufrevpass [exp Float]] Float ((d_dexp : Float) (exp_x : Float))
     (mul exp_x d_dexp))

(def exp (Tensor 1 Float) ((v : Tensor 1 Float))
  (build (size v) (lam (i : Integer) (exp (index i v)))))
(gdef fwd [exp (Tensor 1 Float)])
(gdef rev [exp (Tensor 1 Float)])
(gdef suffwdpass [exp (Tensor 1 Float)])
(gdef sufrevpass [exp (Tensor 1 Float)])

(edef sin Float (Float))
(edef cos Float (Float))

(edef [D sin] (LM Float Float) (Float))
(def [fwd sin] Float ((x : Float) (dx : Float)) (mul (cos x) dx))
(def [rev sin] Float ((x : Float) (d_dsin : Float)) (mul (cos x) d_dsin))
(def [suffwdpass sin] (Tuple Float Float) (x : Float) (tuple (cos x) x))
(def [sufrevpass [sin Float]] Float ((d_dcos : Float) (bog : Float))
     (neg (mul (cos bog) d_dcos)))
(edef [Dt sin] (Tuple Float (LM Float Float)) (Float))

(edef [D cos] (LM Float Float) (Float))
(def [fwd cos] Float ((x : Float) (dx : Float)) (neg (mul (sin x) dx)))
(def [rev cos] Float ((x : Float) (d_dcos : Float)) (neg (mul (sin x) d_dcos)))
(edef [Dt cos] (Tuple Float (LM Float Float)) (Float))
(def [suffwdpass cos] (Tuple Float Float) (x : Float) (tuple (cos x) x))
(def [sufrevpass [cos Float]] Float ((d_dcos : Float) (bog : Float))
     (neg (mul (sin bog) d_dcos)))

(edef cosh Float (Float))

(edef tanh Float (Float))
(def [fwd tanh] Float ((x : Float) (dx : Float))
     (let (cosh_x (cosh x))
     (let (cosh_x_2 (mul cosh_x cosh_x))
       (div dx cosh_x_2))))
(def [rev tanh] Float ((x : Float) (d_dr : Float))
     (let (cosh_x (cosh x))
     (let (cosh_x_2 (mul cosh_x cosh_x))
       (div d_dr cosh_x_2))))
(edef [D tanh] (LM Float Float) (Float))
(edef [Dt tanh] (Tuple Float (LM Float Float)) (Float))
(def [suffwdpass tanh] (Tuple Float Float) (x : Float)
     (tuple (tanh x) x))
(def [sufrevpass [tanh Float]] Float ((d_dtanh : Float) (x : Float))
     (let (cosh_x (cosh x))
     (let (cosh_x_2 (mul cosh_x cosh_x))
       (div d_dtanh cosh_x_2))))

(edef max Float (Tuple Float Float))
(edef [D max] (LM (Tuple Float Float) Float) (Tuple Float Float))
(edef [Dt max] (Tuple Float (LM (Tuple Float Float) Float)) (Tuple Float Float))

(edef imax Integer ((Tensor 1 Float)))
(edef max Float ((Tensor 1 Float)))
(edef [D max] (LM (Tensor 1 Float) Float) ((Tensor 1 Float)))
(edef [Dt max] (Tuple Float (LM (Tensor 1 Float) Float)) ((Tensor 1 Float)))
(def [fwd max] Float ((x : (Tensor 1 Float)) (dx : (Tensor 1 Float)))
  (index (imax x) dx))
(def [rev max] (Tensor 1 Float) ((x : (Tensor 1 Float)) (d_dr : Float))
  (deltaVec (size x) (imax x) d_dr))
(def [suffwdpass max] (Tuple Float (Tuple Integer Integer)) (x : (Tensor 1 Float))
       (tuple (max x) (tuple (imax x) (size x))))
(def [sufrevpass [max (Tensor 1 Float)]] (Tensor 1 Float) ((d_dr : Float) (imax_x_size_x : Tuple Integer Integer))
     (let ((imax_x size_x) imax_x_size_x)
       (deltaVec size_x imax_x d_dr)))

(edef $ranhashdoub Float (Integer))
(edef [D $ranhashdoub] (LM Integer Float) (Integer))
(def [fwd $ranhashdoub] Float ((x : Integer) (dx : (Tuple))) 0.0)
(def [rev $ranhashdoub] (Tuple) ((x : Integer) (d_dranhashdoub : Float)) (tuple))
(edef [Dt $ranhashdoub] (Tuple Float (LM Integer Float)) (Integer))

(def [suffwdpass $ranhashdoub] (Tuple Float (Tuple)) (x : Integer)
     (tuple ($ranhashdoub x) (tuple)))
(def [sufrevpass [$ranhashdoub Integer]] (Tuple) (t : Tuple Float (Tuple))
     (tuple))

(edef abs Float (Float))
(edef [D abs] (LM Float Float) (Float))
(def [fwd abs] Float ((x : Float) (dx : Float)) (if (gt x 0.0) dx (neg dx)))
(def [rev abs] Float ((x : Float) (d_dabs : Float))
     (if (gt x 0.0) d_dabs (neg d_dabs)))
(edef [Dt abs] (Tuple Float (LM Float Float)) (Float))

(edef lgamma Float (Float))
(edef [D lgamma] (LM Float Float) (Float))
(edef [fwd lgamma] Float (Tuple Float Float))
(edef [rev lgamma] Float (Tuple Float Float))
(edef [Dt lgamma] (Tuple Float (LM Float Float)) (Float))

(def [suffwdpass lgamma] (Tuple Float Float) (x : Float)
     (tuple (lgamma x) x))
(def [sufrevpass [lgamma Float]] Float ((d_dlgamma : Float) (x : Float))
     ([rev lgamma] x d_dlgamma))

(edef or Bool (Tuple Bool Bool))
(edef [D or] (LM (Tuple Bool Bool) Bool) (Tuple Bool Bool))
(edef [Dt or] (Tuple Bool (LM (Tuple Bool Bool) Bool)) (Tuple Bool Bool))
(def [fwd or] (Tuple)
     ((xt : Tuple Bool Bool) (dxt : Tuple Bool Bool))
     (tuple))
(def [rev or] (Tuple (Tuple) (Tuple))
     ((xt : Tuple Bool Bool) (d_dbool : (Tuple)))
     (tuple (tuple) (tuple)))

(def [suffwdpass or] (Tuple Bool (Tuple)) (t : Tuple Bool Bool)
     (tuple (or t) (tuple)))
(def [sufrevpass [or (Tuple Bool Bool)]] (Tuple (Tuple) (Tuple)) (t : Tuple (Tuple) (Tuple))
     (tuple (tuple) (tuple)))

(edef and Bool (Tuple Bool Bool))
(edef [D and] (LM (Tuple Bool Bool) Bool) (Tuple Bool Bool))
(edef [Dt and] (Tuple Bool (LM (Tuple Bool Bool) Bool)) (Tuple Bool Bool))
(def [fwd and] (Tuple)
     ((xt : Tuple Bool Bool) (dxt : Tuple (Tuple) (Tuple)))
     (tuple))
(def [rev and] (Tuple (Tuple) (Tuple))
     ((xt : Tuple Bool Bool) (d_dbool : (Tuple)))
     (tuple (tuple) (tuple)))

(def [suffwdpass and] (Tuple Bool (Tuple)) (t : Tuple Bool Bool)
     (tuple (and t) (tuple)))
(def [sufrevpass [and (Tuple Bool Bool)]] (Tuple (Tuple) (Tuple)) (t : Tuple (Tuple) (Tuple))
     (tuple (tuple) (tuple)))

(def pi Float () 3.14159265358979323846264338327950288419716939937510)

(edef sqrt Float Float)
(edef [D sqrt] (LM Float Float) (Float))
(def [fwd sqrt] Float ((x : Float) (dx : Float))
     (div dx (mul 2.0 (sqrt x))))
(def [rev sqrt] Float ((x : Float) (ddr : Float))
     (div ddr (mul 2.0 (sqrt x))))
(def [suffwdpass sqrt] (Tuple Float Float) (x : Float)
     (let (sqrt_x (sqrt x))
     (tuple sqrt_x sqrt_x)))
(def [sufrevpass [sqrt Float]] Float ((ddr : Float) (sqrt_x : Float))
     (div ddr (mul 2.0 sqrt_x)))

(edef erf Float Float)
(edef [D erf] (LM Float Float) (Float))
(def [fwd erf] Float ((x : Float) (dx : Float))
     (div (mul (mul 2.0 dx) (exp (neg (mul x x))))
          (sqrt (pi))))
(def [rev erf] Float ((x : Float) (ddr : Float))
     (div (mul (mul 2.0 ddr) (exp (neg (mul x x))))
          (sqrt (pi))))
(def [suffwdpass erf] (Tuple Float Float) (x : Float)
     (tuple (erf x) x))
(def [sufrevpass [erf Float]] Float ((ddr : Float) (x : Float))
     (div (mul (mul 2.0 ddr) (exp (neg (mul x x))))
          (sqrt (pi))))
