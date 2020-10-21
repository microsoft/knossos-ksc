(def not Bool (p : Bool) (if p false true))

;; neg :: Number -> Number
;; neg x = -x
(edef neg Float (Float))
(edef D$neg (LM Float Float) (Float))
(edef Dt$neg (Tuple Float (LM Float Float)) (Float))
(def fwd$neg Float ((x : Float) (dx : Float))
     (neg dx))
(def rev$neg Float ((x : Float) (d_dneg : Float))
     (neg d_dneg))

(edef neg Integer (Integer))
(edef D$neg (LM Integer Integer) (Integer))
(edef Dt$neg (Tuple Integer (LM Integer Integer)) (Integer))
(def fwd$neg (Tuple) ((x : Integer) (dx : (Tuple)))
     (tuple))
(def rev$neg (Tuple) ((x : Integer) (d_dneg : (Tuple)))
     (tuple))

;; add :: Number x Number -> Number
;; add (x, y) = x + y
(edef add Float (Float Float))
(edef D$add (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$add (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$add Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (add dx1 dx2)))
(def
 rev$add (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((d_dadd drt))
  (tuple d_dadd d_dadd)))

(edef add Integer (Integer Integer))
(edef D$add (LM (Tuple Integer Integer) Integer) (Integer Integer))
(edef Dt$add (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))
(def
 fwd$add (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$add (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; sub :: Number x Number -> Number
;; sub (x, y) = x - y
(edef sub Float (Float Float))
(edef D$sub (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$sub (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$sub Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (sub dx1 dx2)))
(def
 rev$sub (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((d_dsub drt))
  (tuple d_dsub (neg d_dsub))))

(edef sub Integer (Integer Integer))
(edef D$sub (LM (Tuple Integer Integer) Integer) (Integer Integer))
(edef Dt$sub (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))
(def
 fwd$sub (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$sub (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; mul :: Number x Number -> Number
;; mul (x, y) = x * y
(edef mul Float (Float Float))
(edef D$mul (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$mul (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$mul Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (add (mul x2 dx1) (mul x1 dx2))))
(def
 rev$mul (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (d_dmul drt))
  (tuple (mul d_dmul x2) (mul d_dmul x1))))

(edef mul Integer (Integer Integer))
(edef D$mul (LM (Tuple Integer Integer) Integer) (Integer Integer))
(edef Dt$mul (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))
(def
 fwd$mul (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$mul (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))


;; div :: Number x Number -> Number
;; div (x, y) = x / y
(edef div Float (Float Float))
(edef D$div (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$div (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$div Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (div (sub (mul x2 dx1)
                  (mul x1 dx2))
          (mul x2 x2))))
(def
 rev$div (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (d_ddiv drt))
  (tuple (div d_ddiv x2)
         (neg (div (mul x1 d_ddiv)
                         (mul x2 x2))))))
(edef div Integer (Integer Integer))
(edef D$div (LM (Tuple Integer Integer) Integer) (Integer Integer))
(edef Dt$div (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))
(def
 fwd$div (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$div (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))


;; eq :: Number x Number -> Bool
;; eq (x, y) = x > y
(edef eq Bool (Float Float))
(edef D$eq (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$eq (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$eq (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$eq (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef eq Bool (Integer Integer))
(edef D$eq (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$eq (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$eq (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$eq (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; gt :: Number x Number -> Bool
;; gt (x, y) = x > y
(edef gt Bool (Float Float))
(edef D$gt (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$gt (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$gt (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$gt (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef gt Bool (Integer Integer))
(edef D$gt (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$gt (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$gt (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$gt (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; lt :: Number x Number -> Bool
;; lt (x, y) = x < y
(edef lt Bool (Float Float))
(edef D$lt (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$lt (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$lt (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$lt (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef lt Bool (Integer Integer))
(edef D$lt (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$lt (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$lt (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$lt (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; lte :: Number x Number -> Bool
;; lte (x, y) = x <= y
(edef lte Bool (Float Float))
(edef D$lte (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$lte (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$lte (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$lte (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef lte Bool (Integer Integer))
(edef D$lte (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$lte (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$lte (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$lte (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; gte :: Number x Number -> Bool
;; gte (x, y) = x >= y
(edef gte Bool (Float Float))
(edef D$gte (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$gte (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$gte (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$gte (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef gte Bool (Integer Integer))
(edef D$gte (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$gte (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$gte (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$gte (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(edef log Float (Float))
(edef D$log (LM Float Float) (Float))
(def fwd$log Float ((x : Float) (dx : Float)) (div dx x))
(def rev$log Float ((x : Float) (d_dlog : Float)) (div d_dlog x))
(edef Dt$log (Tuple Float (LM Float Float)) (Float))

(edef exp Float (Float))
(edef D$exp (LM Float Float) (Float))
(def fwd$exp Float ((x : Float) (dx : Float)) (mul (exp x) dx))
(def rev$exp Float ((x : Float) (d_dexp : Float)) (mul (exp x) d_dexp))
(edef Dt$exp (Tuple Float (LM Float Float)) (Float))

(edef sin Float (Float))
(edef cos Float (Float))

(edef D$sin (LM Float Float) (Float))
(def fwd$sin Float ((x : Float) (dx : Float)) (mul (cos x) dx))
(def rev$sin Float ((x : Float) (d_dsin : Float)) (mul (cos x) d_dsin))
(edef Dt$sin (Tuple Float (LM Float Float)) (Float))

(edef D$cos (LM Float Float) (Float))
(def fwd$cos Float ((x : Float) (dx : Float)) (neg (mul (sin x) dx)))
(def rev$cos Float ((x : Float) (d_dcos : Float)) (neg (mul (sin x) d_dcos)))
(edef Dt$cos (Tuple Float (LM Float Float)) (Float))

(edef tanh Float (Float))
(def fwd$tanh Float ((x : Float) (dx : Float))
     (let ((tanh_x (tanh x))
           (tanh_x_2 (mul tanh_x tanh_x)))
       (mul tanh_x_2 dx)))
(def rev$tanh Float ((x : Float) (d_dr : Float))
     (let ((tanh_x (tanh x))
           (tanh_x_2 (mul tanh_x tanh_x)))
       (mul tanh_x_2 d_dr)))
(edef D$tanh (LM Float Float) (Float))
(edef Dt$tanh (Tuple Float (LM Float Float)) (Float))


(edef max Float (Float Float))
(edef D$max (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$max (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))

(edef $ranhashdoub Float (Integer))
(edef D$$ranhashdoub (LM Integer Float) (Integer))
(def fwd$$ranhashdoub Float ((x : Integer) (dx : (Tuple))) 0.0)
(def rev$$ranhashdoub (Tuple) ((x : Integer) (d_dranhashdoub : Float)) (tuple))
(edef Dt$$ranhashdoub (Tuple Float (LM Integer Float)) (Integer))

(edef abs Float (Float))
(edef D$abs (LM Float Float) (Float))
(def fwd$abs Float ((x : Float) (dx : Float)) (if (gt x 0.0) dx (neg dx)))
(def rev$abs Float ((x : Float) (d_dabs : Float))
     (if (gt x 0.0) d_dabs (neg d_dabs)))
(edef Dt$abs (Tuple Float (LM Float Float)) (Float))

(edef to_float Float (Integer))
(edef D$to_float (LM Integer Float) (Integer))
(def fwd$to_float Float ((x : Integer) (dx : (Tuple))) 0.0)
(def rev$to_float (Tuple) ((x : Integer) (d_dto_float : Float)) (tuple))
(edef Dt$to_float (Tuple Float (LM Integer Float)) (Integer))

(edef lgamma Float (Float))
(edef D$lgamma (LM Float Float) (Float))
(edef fwd$lgamma Float (Float Float))
(edef rev$lgamma Float (Float Float))
(edef Dt$lgamma (Tuple Float (LM Float Float)) (Float))

(edef or Bool (Bool Bool))
(edef D$or (LM (Tuple Bool Bool) Bool) (Bool Bool))
(edef Dt$or (Tuple Bool (LM (Tuple Bool Bool) Bool)) (Bool Bool))
(def fwd$or (Tuple)
     ((xt : Tuple Bool Bool) (dxt : Tuple Bool Bool))
     (tuple))
(def rev$or (Tuple (Tuple) (Tuple))
     ((xt : Tuple Bool Bool) (d_dbool : (Tuple)))
     (tuple (tuple) (tuple)))

(edef and Bool (Bool Bool))
(edef D$and (LM (Tuple Bool Bool) Bool) (Bool Bool))
(edef Dt$and (Tuple Bool (LM (Tuple Bool Bool) Bool)) (Bool Bool))
(def fwd$and (Tuple)
     ((xt : Tuple Bool Bool) (dxt : Tuple Bool Bool))
     (tuple))
(def rev$and (Tuple (Tuple) (Tuple))
     ((xt : Tuple Bool Bool) (d_dbool : (Tuple)))
     (tuple (tuple) (tuple)))
