;; Key of function names
;;
;; f :: S -> T
;; D$f :: S -> LM S T             Derivative via linar map
;; Dt$f :: S -> (S, LM S T)       Tupled derivative via linear map
;; fwd$f :: (S,dS) -> dT          Forward derivative
;; rev$f :: (S,dT) -> dS          Reverse derivative
;; fwdt$f :: (S,ds) -> (T,dT)     Forward derivative, tupled
;;
;; fwds$f :: S -> (T,X)           Forward split derivative
;;                                   X may be an empty tuple
;; revs$f :: (dT,X) -> dS         Reverse split derivative
;;        :: dT -> dS                If X=()

;; ------------------------------------------
;;         add :: (Float, Float) -> Float
;; ------------------------------------------

(edef add Float (Float Float))

(edef D$add (LM (Tuple Float Float) Float) (Float Float))

(edef Dt$add (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(def
 fwd$add Float
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (add (get$1$2 dxt) (get$2$2 dxt)))

; Tupled forward add
(def
 fwdt$add (Tuple Float Float)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple (add (get$1$2 xt) (get$2$2 xt))
         (add (get$1$2 dxt) (get$2$2 dxt))))

; Reverse add
(def
 rev$add (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((d_dadd drt))
  (tuple d_dadd d_dadd)))

; Split add
; fwds$add :: (F,F) -> (F, ())
; revs$add :: (dF, ()) -> (dF,dF)

(def
 fwds$add (Tuple Float (Tuple))
 (xt : (Tuple Float Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt)))
  (tuple (add x1 x2) (tuple))))

(def
 revs$add (Tuple Float Float)
 (dr : Float)
 (tuple dr dr))

;; ------------------------------------------
;;         sum :: Vec Float -> Float
;; ------------------------------------------

(edef sum Float ((Vec Float)))


(edef D$sum (LM (Vec Float) Float) ((Vec Float)))

(edef Dt$sum (Tuple Float (LM (Vec Float) Float)) ((Vec Float)))

(def
 fwd$sum Float ((xt : (Vec Float)) (dxt : (Vec Float)))
 (sum dxt))

; Tupled forward sum
(def
 fwdt$sum (Tuple Float Float) ((xt : (Vec Float)) (dxt : (Vec Float)))
  (tuple (sum xt) (sum dxt)))

; Reverse sum
(def
 rev$sum (Vec Float)
 ((xt : (Vec Float)) (drt : Float))
  (build (size xt) (lam (i : Integer) drt)))

; Split sum
; fwds$sum :: Vec F -> (F, Integer)
; revs$sum :: (dF, Integer) -> Vec dF

(def
 fwds$sum (Tuple Float Integer) (xt : (Vec Float))
  (tuple (sum xt) (size xt)))

(def
 revs$sum (Vec Float) ( (dr : Float) (n : Integer) )
   (build n (lam (i : Integer) dr)))


;; ------------------------------------------
;;         add :: (Integer, Integer) -> Integer
;; ------------------------------------------

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


; Tupled forward add
(def
 fwdt$add (Tuple Integer (Tuple))
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple (add (get$1$2 xt) (get$2$2 xt))
         (tuple)))

; Split add
; fwds$add :: (I,I) -> (F, ())
; revs$add :: ((), ()) -> (dF,dF)

(def
 fwds$add (Tuple Integer (Tuple))
 (xt : (Tuple Integer Integer))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt)))
  (tuple (add x1 x2) (tuple))))

(def
 revs$add (Tuple (Tuple) (Tuple))
 (dr : (Tuple))
 (tuple dr dr))


;; ------------------------------------------
;;         sub :: (Float, Float) -> Float
;; ------------------------------------------

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

(def
 fwdt$sub (Tuple Float Float)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
  (tuple (sub (get$1$2 xt) (get$2$2 xt))
         (sub dxt)))


;; ------------------------------------------
;;         sub :: (Integer, Integer) -> Integer
;; ------------------------------------------

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

(def
 fwdt$sub (Tuple Integer (Tuple))
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple (sub (get$1$2 xt) (get$2$2 xt))
         (tuple)))

(def
 fwds$sub (Tuple Integer (Tuple))
 (xt : (Tuple Integer Integer))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt)))
  (tuple (sub x1 x2) (tuple))))

(def
 revs$sub (Tuple (Tuple) (Tuple))
 (dr : (Tuple))
 (tuple dr dr))


;; ------------------------------------------
;;         div :: (Float, Float) -> Float
;; ------------------------------------------

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

; Tupled forward divide
(def
 fwdt$div (Tuple Float Float)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (tuple (div x1 x2)
         (div (sub (mul x2 dx1)
                         (mul x1 dx2))
                 (mul x2 x2)))))

; Reverse division
(def
 rev$div (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt)))
  (tuple (div drt x2)
         (neg (div (mul x1 drt)
                         (mul x2 x2))))))

; Split forward division
;    fwds$div :: (F,F) -> (F, (F,F))
;    revs$div :: (dF, (F,F)) -> (dF,dF)
(def
 fwds$div (Tuple Float (Tuple Float Float))
 (xt : (Tuple Float Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt)))
  (tuple (div x1 x2) (tuple x1 x2))))

(def
 revs$div (Tuple Float Float)
 ( (dr : Float) (x : (Tuple Float Float)) )
 (let
  ((x1 (get$1$2 x))
   (x2 (get$2$2 x)))
  (tuple (div dr x2)
         (neg (div (mul x1 dr) (mul x2 x2))))))

;; ------------------------------------------
;;         div :: (Integer, Integer) -> Integer
;; ------------------------------------------

(edef div Integer (Integer Integer))

(edef D$div (LM (Tuple Integer Integer) Integer) (Integer Integer))

(edef Dt$div (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))

(def
 fwd$div (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))

(def
 fwdt$div (Tuple Integer (Tuple))
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple (div xt) (tuple)))

(def
 rev$div (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; ------------------------------------------
;;         div :: (Float, Float) -> Float
;; ------------------------------------------

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

; Tupled forward multiply
(def
 fwdt$mul (Tuple Float Float)
 ((xt : (Tuple Float Float)) (dxt : (Tuple Float Float)))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (dx1 (get$1$2 dxt))
   (dx2 (get$2$2 dxt)))
  (tuple (mul x1 x2) (add (mul x2 dx1) (mul x1 dx2)))))

; Reverse multiply
(def
 rev$mul (Tuple Float Float)
 ((xt : (Tuple Float Float)) (drt : Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt))
   (d_dmul drt))
  (tuple (mul d_dmul x2) (mul d_dmul x1))))

; Split forward multiply
;    fwds$mul :: (F,F) -> (F, (F,F))
;    revs$mul :: (dF, (F,F)) -> (dF,dF)
(def
 fwds$mul (Tuple Float (Tuple Float Float))
 (xt : (Tuple Float Float))
 (let
  ((x1 (get$1$2 xt))
   (x2 (get$2$2 xt)))
  (tuple (mul x1 x2) (tuple x1 x2))))

(def
 revs$mul (Tuple Float Float)
 (xt : (Tuple Float (Tuple Float Float)))
 (let
  ((dr (get$1$2 xt))
   (x  (get$2$2 xt))
   (x1 (get$1$2 x))
   (x2 (get$2$2 x)))
  (tuple (mul dr x2) (mul dr x1))))

;; ------------------------------------------
;;         mul :: (Integer, Integer) -> Integer
;; ------------------------------------------

(edef mul Integer (Integer Integer))

(edef D$mul (LM (Tuple Integer Integer) Integer) (Integer Integer))

(edef Dt$mul (Tuple Integer (LM (Tuple Integer Integer) Integer)) (Integer Integer))

(def
 fwd$mul (Tuple)
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple))

(def
 fwdt$mul (Tuple Integer (Tuple))
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
 (tuple (mul xt) (tuple)))

(def
 rev$mul (Tuple (Tuple) (Tuple))
 ((xt : (Tuple Integer Integer)) (drt : (Tuple)))
  (tuple (tuple) (tuple)))

;; ------------------------------------------
;;         neg :: Float -> Float
;; ------------------------------------------

(edef neg Float (Float))

(edef D$neg (LM Float Float) (Float))

(edef Dt$neg (Tuple Float (LM Float Float)) (Float))

(def fwd$neg Float ((x : Float) (dx : Float))
     (neg dx))

(def fwdt$neg (Tuple Float Float) ((x : Float) (dx : Float))
     (tuple (neg dx) (neg dx)))

(def rev$neg Float ((x : Float) (d_dneg : Float))
     (neg d_dneg))

; Split negation
; fwds$neg :: F -> (F, ())
; revs$neg :: dF -> dF

(def fwds$neg (Tuple Float (Tuple)) (x : Float)
  (tuple (neg x) (tuple)))

(def revs$neg Float (dr : Float)
 (neg dr))


;; ------------------------------------------
;;         neg :: Integer -> Integer
;; ------------------------------------------

(edef neg Integer (Integer))
(edef D$neg (LM Integer Integer) (Integer))
(edef Dt$neg (Tuple Integer (LM Integer Integer)) (Integer))
(def fwd$neg (Tuple) ((x : Integer) (dx : (Tuple)))
     (tuple))
(def rev$neg (Tuple) ((x : Integer) (d_dneg : (Tuple)))
     (tuple))


;; ------------------------------------------
;;         gt :: (Float,Float) -> Bool
;; ------------------------------------------

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

;; ------------------------------------------
;;         gt :: (Intger,Integer) -> Bool
;; ------------------------------------------

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


;; ------------------------------------------
;;         lt :: (Float,Float) -> Bool
;; ------------------------------------------

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

;; ------------------------------------------
;;         lt :: (Intger,Integer) -> Bool
;; ------------------------------------------

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

(def fwdt$lt (Tuple Bool (Tuple)) ((xt : Tuple Integer Integer)
                                   (dxt : Tuple (Tuple) (Tuple)))
     (tuple (lt xt) (tuple)))

;; ------------------------------------------
;;         lte :: (Float,Float) -> Bool
;; ------------------------------------------

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

;; ------------------------------------------
;;         lte :: (Integer,Integer) -> Bool
;; ------------------------------------------

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

;; ------------------------------------------
;;         gte :: (Float,Float) -> Bool
;; ------------------------------------------

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

;; ------------------------------------------
;;         gte :: (Integer,Integer) -> Bool
;; ------------------------------------------

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
(def
 fwdt$gte (Tuple Bool (Tuple))
 ((xt : (Tuple Integer Integer)) (dxt : (Tuple (Tuple) (Tuple))))
  (tuple (gte xt) (tuple)))

;; ------------------------------------------
;;         and,or :: (Bool,Bool) -> Bool
;; ------------------------------------------

(edef or Bool (Bool Bool))
(edef D$or (LM (Tuple Bool Bool) Bool) (Bool Bool))
(edef Dt$or (Tuple Bool (LM (Tuple Bool Bool) Bool)) (Bool Bool))
(def fwd$or (Tuple)
     ((xt : Tuple Bool Bool) (dxt : Tuple Bool Bool))
     (tuple))
(def rev$or (Tuple (Tuple) (Tuple))
     ((xt : Tuple Bool Bool) (d_dbool : Tuple))
     (tuple (tuple) (tuple)))
(def fwdt$or (Tuple Bool (Tuple))
     ((xt : Tuple Bool Bool) (dxt : Tuple (Tuple) (Tuple)))
     (tuple (or xt) (tuple)))


(edef and Bool (Bool Bool))
(edef D$and (LM (Tuple Bool Bool) Bool) (Bool Bool))
(edef Dt$and (Tuple Bool (LM (Tuple Bool Bool) Bool)) (Bool Bool))
(def fwd$and (Tuple)
     ((xt : Tuple Bool Bool) (dxt : Tuple Bool Bool))
     (tuple))
(def rev$and (Tuple (Tuple) (Tuple))
     ((xt : Tuple Bool Bool) (d_dbool : Tuple))
     (tuple (tuple) (tuple)))


;; ------------------------------------------
;;         log :: Float -> Float
;; ------------------------------------------

(edef log Float (Float))
(edef D$log (LM Float Float) (Float))
(def fwd$log Float ((x : Float) (dx : Float)) (div dx x))
(def rev$log Float ((x : Float) (d_dlog : Float)) (div d_dlog x))
(edef Dt$log (Tuple Float (LM Float Float)) (Float))
(def fwdt$log (Tuple Float Float) ((x : Float) (dx : Float))
     (tuple (log x) (div dx x)))

(edef exp Float (Float))
(edef D$exp (LM Float Float) (Float))
(def fwd$exp Float ((x : Float) (dx : Float)) (mul (exp x) dx))
(def rev$exp Float ((x : Float) (d_dexp : Float)) (mul (exp x) d_dexp))
(edef Dt$exp (Tuple Float (LM Float Float)) (Float))
(def fwdt$exp (Tuple Float Float) ((x : Float) (dx : Float))
     (let (ex (exp x))
       (tuple ex (mul x ex))))


;; ------------------------------------------
;;         sin, cos, tanh :: Float -> Float
;; ------------------------------------------

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


;; ------------------------------------------
;;         max :: (Float,Float) -> Float
;; ------------------------------------------

(edef max Float (Float Float))
(edef D$max (LM Float Float) (Float Float))
(edef Dt$max (Tuple Float (LM Float Float)) (Float Float))

;; ------------------------------------------
;;         ranhashdoub :: (Float,Float) -> Float
;; ------------------------------------------

(edef $ranhashdoub Float (Integer))
(edef D$$ranhashdoub (LM Integer Float) (Integer))
(def fwd$$ranhashdoub Float ((x : Integer) (dx : (Tuple))) 0.0)
(def rev$$ranhashdoub (Tuple) ((x : Integer) (d_dranhashdoub : Float)) (tuple))
(edef Dt$$ranhashdoub (Tuple Float (LM Integer Float)) (Integer))
(def fwdt$$ranhashdoub (Tuple Float Float) ((x : Integer) (dx : (Tuple)))
     (tuple ($ranhashdoub x) 0.0))

;; ------------------------------------------
;;         abs :: Float -> Float
;; ------------------------------------------

(edef abs Float (Float))
(edef D$abs (LM Float Float) (Float))
(def fwd$abs Float ((x : Float) (dx : Float)) (if (gt x 0.0) dx (neg dx)))
(def rev$abs Float ((x : Float) (d_dabs : Float))
     (if (gt x 0.0) d_dabs (neg d_dabs)))
(edef Dt$abs (Tuple Float (LM Float Float)) (Float))

;; ------------------------------------------
;;         to_float :: Integer -> Float
;; ------------------------------------------

(edef to_float Float (Integer))
(edef D$to_float (LM Integer Float) (Integer))
(def fwd$to_float Float ((x : Integer) (dx : (Tuple))) 0.0)
(def fwdt$to_float (Tuple Float Float) ((x : Integer) (dx : (Tuple)))
     (tuple (to_float x) 0.0))
(def rev$to_float (Tuple) ((x : Integer) (d_dto_float : Float)) (tuple))
(edef Dt$to_float (Tuple Float (LM Integer Float)) (Integer))

;; ------------------------------------------
;;         lgamma :: Float -> Float
;; ------------------------------------------

(edef lgamma Float (Float))
(edef D$lgamma (LM Float Float) (Float))
(edef fwd$lgamma Float (Float Float))
(edef rev$lgamma Float (Float Float))
(edef Dt$lgamma (Tuple Float (LM Float Float)) (Float))
(def fwdt$lgamma (Tuple Float Float) ((x : Float) (dx : Float))
     (tuple (lgamma x) (fwd$lgamma x dx)))
