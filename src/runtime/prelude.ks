;; add :: Number x Number -> Number
;; add (x, y) = x + y
(edef add@ff Float (Float Float))
(edef D$add@ff (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$add@ff (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$add@ff Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (add@ff dx1 dx2)))
(def
 rev$add@ff (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((d_dadd drt))
  (tuple d_dadd d_dadd)))

(edef add@ii Integer (Integer Integer))
(edef D$add@ii (LM (Tuple Integer Integer) Integer) (Integer Integer))
(edef Dt$add@ii (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))
(def
 fwd$add@ii (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$add@ii (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; sub :: Number x Number -> Number
;; sub (x, y) = x - y
(edef sub@ff Float (Float Float))
(edef D$sub@ff (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$sub@ff (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$sub@ff Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (sub@ff dx1 dx2)))
(def
 rev$sub@ff (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((d_dsub drt))
  (tuple d_dsub (neg@f d_dsub))))

(edef sub@ii Integer (Integer Integer))
(edef D$sub@ii (LM (Tuple Integer Integer) Integer) (Integer Integer))
(edef Dt$sub@ii (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))
(def
 fwd$sub@ii (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$sub@ii (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; div :: Number x Number -> Number
;; div (x, y) = x / y
(edef div@ff Float (Float Float))
(edef D$div@ff (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$div@ff (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$div@ff Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (div@ff (sub@ff (mul@ff x2 dx1)
                  (mul@ff x1 dx2))
          (mul@ff x2 x2))))
(def
 rev$div@ff (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (d_ddiv drt))
  (tuple (div@ff d_ddiv x2)
         (neg@f (div@ff (mul@ff x1 d_ddiv)
                         (mul@ff x2 x2))))))
(edef div@ii Integer (Integer Integer))
(edef D$div@ii (LM (Tuple Integer Integer) Integer) (Integer Integer))
(edef Dt$div@ii (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))
(def
 fwd$div@ii (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$div@ii (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; mul :: Number x Number -> Number
;; mul (x, y) = x * y
(edef mul@ff Float (Float Float))
(edef D$mul@ff (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$mul@ff (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$mul@ff Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (add@ff (mul@ff x2 dx1) (mul@ff x1 dx2))))
(def
 rev$mul@ff (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (d_dmul drt))
  (tuple (mul@ff d_dmul x2) (mul@ff d_dmul x1))))

(edef mul@ii Integer (Integer Integer))
(edef D$mul@ii (LM (Tuple Integer Integer) Integer) (Integer Integer))
(edef Dt$mul@ii (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))
(def
 fwd$mul@ii (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$mul@ii (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; neg :: Number -> Number
;; neg x = -x
(edef neg@f Float (Float))
(edef D$neg@f (LM Float Float) (Float))
(edef Dt$neg@f (Tuple Float (LM Float Float)) (Float))
(def fwd$neg@f Float ((x : Float) (dx : Float))
     (neg@f dx))
(def rev$neg@f Float ((x : Float) (d_dneg : Float))
     (neg@f d_dneg))

(edef neg@i Integer (Integer))
(edef D$neg@i (LM Integer Integer) (Integer))
(edef Dt$neg@i (Tuple Integer (LM Integer Integer)) (Integer))
(def fwd$neg@i (Tuple) ((x : Integer) (dx : (Tuple)))
     (tuple))
(def rev$neg@i (Tuple) ((x : Integer) (d_dneg : (Tuple)))
     (tuple))

;; gt :: Number x Number -> Bool
;; gt (x, y) = x > y
(edef gt@ff Bool (Float Float))
(edef D$gt@ff (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$gt@ff (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$gt@ff (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$gt@ff (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef gt@ii Bool (Integer Integer))
(edef D$gt@ii (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$gt@ii (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$gt@ii (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$gt@ii (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; lt :: Number x Number -> Bool
;; lt (x, y) = x < y
(edef lt@ff Bool (Float Float))
(edef D$lt@ff (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$lt@ff (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$lt@ff (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$lt@ff (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef lt@ii Bool (Integer Integer))
(edef D$lt@ii (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$lt@ii (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$lt@ii (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$lt@ii (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; lte :: Number x Number -> Bool
;; lte (x, y) = x <= y
(edef lte@ff Bool (Float Float))
(edef D$lte@ff (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$lte@ff (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$lte@ff (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$lte@ff (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef lte@ii Bool (Integer Integer))
(edef D$lte@ii (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$lte@ii (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$lte@ii (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$lte@ii (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; gte :: Number x Number -> Bool
;; gte (x, y) = x >= y
(edef gte@ff Bool (Float Float))
(edef D$gte@ff (LM (Tuple Float Float) Bool) (Float Float))
(edef Dt$gte@ff (Tuple Bool (LM (Tuple Float Float) Bool)) (Float Float))
(def
 fwd$gte@ff (Tuple)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple))
(def
 rev$gte@ff (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : (Tuple)))
  (tuple 0.0 0.0))

(edef gte@ii Bool (Integer Integer))
(edef D$gte@ii (LM (Tuple Integer Integer) Bool) (Integer Integer))
(edef Dt$gte@ii (Tuple Bool (LM (Tuple Integer Integer) Bool)) (Integer Integer))
(def
 fwd$gte@ii (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))
(def
 rev$gte@ii (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

(edef log Float (Float))
(edef D$log (LM Float Float) (Float))
(def fwd$log Float ((x : Float) (dx : Float)) (div@ff dx x))
(def rev$log Float ((x : Float) (d_dlog : Float)) (div@ff d_dlog x))
(edef Dt$log (Tuple Float (LM Float Float)) (Float))

(edef exp Float (Float))
(edef D$exp (LM Float Float) (Float))
(def fwd$exp Float ((x : Float) (dx : Float)) (mul@ff (exp x) dx))
(def rev$exp Float ((x : Float) (d_dexp : Float)) (mul@ff (exp x) d_dexp))
(edef Dt$exp (Tuple Float (LM Float Float)) (Float))

(edef sin Float (Float))
(edef cos Float (Float))

(edef D$sin (LM Float Float) (Float))
(def fwd$sin Float ((x : Float) (dx : Float)) (mul@ff (cos x) dx))
(def rev$sin Float ((x : Float) (d_dsin : Float)) (mul@ff (cos x) d_dsin))
(edef Dt$sin (Tuple Float (LM Float Float)) (Float))

(edef D$cos (LM Float Float) (Float))
(def fwd$cos Float ((x : Float) (dx : Float)) (neg@f (mul@ff (sin x) dx)))
(def rev$cos Float ((x : Float) (d_dcos : Float)) (neg@f (mul@ff (sin x) d_dcos)))
(edef Dt$cos (Tuple Float (LM Float Float)) (Float))

(edef max Float (Float Float))
(edef D$max (LM Float Float) (Float Float))
(edef Dt$max (Tuple Float (LM Float Float)) (Float Float))

(edef $ranhashdoub Float (Integer))
(edef D$$ranhashdoub (LM Integer Float) (Integer))
(def fwd$$ranhashdoub Float ((x : Integer) (dx : (Tuple))) (0.0))
(def rev$$ranhashdoub (Tuple) ((x : Integer) (d_dranhashdoub : Float)) (tuple))
(edef Dt$$ranhashdoub (Tuple Float (LM Integer Float)) (Integer))

(edef abs Float (Float))
(edef D$abs (LM Float Float) (Float))
(def fwd$abs Float ((x : Float) (dx : Float)) (if (gt@ff x 0.0) dx (neg@f dx)))
(def rev$abs Float ((x : Float) (d_dabs : Float))
     (if (gt@ff x 0.0) d_dabs (neg@f d_dabs)))
(edef Dt$abs (Tuple Float (LM Float Float)) (Float))

(edef to_float Float (Integer))
(edef D$to_float (LM Integer Float) (Integer))
(def fwd$to_float Float ((x : Integer) (dx : (Tuple))) 0.0)
(def rev$to_float (Tuple) ((x : Integer) (d_dto_float : Float)) (tuple))
(edef Dt$to_float (Tuple Float (LM Integer Float)) (Integer))
