(def expv (Vec Float) (v : Vec Float)
 (let (n (size v))
   (build n (lam (i : Integer) (exp (index i v))))))

(def sum@V[[Float]] Float (v : Vec Float)
   (sum v))

(def log@Float Float (v : Float)
   (log v))

(edef maximum Float ((Vec Float)))
(edef D$maximum (LM (Vec Float) Float) ((Vec Float)))
(edef Dt$maximum (Tuple Float (LM (Vec Float) Float)) ((Vec Float)))
(edef fwd$maximum Float ((Vec Float) (Vec Float)))
(edef rev$maximum (Vec Float) ((Vec Float) Float))

(edef lgamma Float (Float))
(edef D$lgamma (LM Float Float) (Float))
(edef fwd$lgamma Float (Float Float))
(edef rev$lgamma Float (Float Float))
(edef Dt$lgamma (Tuple Float (LM Float Float)) (Float))

(def gammaLn Float (x : Float)
   (lgamma x))

(edef pow@Float Float ((Float) (Float)))
(edef D$pow@Float (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$pow@Float (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(edef fwd$pow@Float Float (Float Float Float Float))
(edef rev$pow@Float (Tuple Float Float) (Float Float Float))

(def add@Float,Float Float ((a : Float) (b : Float))
   (add a b))

(def add@Integer,Integer Integer ((a : Integer) (b : Integer))
   (add a b))

(def mul@Float,Float Float ((a : Float) (b : Float))
   (mul a b))

(def mul@Integer,Integer Integer ((a : Integer) (b : Integer))
   (mul a b))

(def div@Integer,Integer  Integer ((a : Integer) (b : Integer))
   (div a b))

(def sub@Integer,Integer  Integer ((a : Integer) (b : Integer))
   (sub a b))

(def sub@Float,Float Float ((a : Float) (b : Float))
   (sub a b))

(def sub@V[[Float]],Float (Vec Float) ((a : Vec Float) (b : Float))
  (let (n (size a))
    (build n (lam (i : Integer) (sub (index i a) b)))))

(def sub@V[[Float]],V[[Float]] (Vec Float) ((a : Vec Float) (b : Vec Float))
  (let (n (size a))
    (build n (lam (i : Integer) (sub (index i a) (index i b))))))

(def sqr Float (a : Float)
   (mul a a))

; mul Mat Vec
(edef mul (Vec Float) ((Vec (Vec Float)) (Vec Float)))
(def shape$mul (Vec (Tuple)) ((m : Vec (Vec Float)) (v : Vec Float))
    (constVec (size m) (tuple)))

(edef D$mul (LM (Tuple (Vec (Vec Float)) (Vec Float)) (Vec Float))
          ((Vec (Vec Float)) (Vec Float)))
(edef Dt$mul (Tuple (Vec Float) (LM (Tuple (Vec (Vec Float)) (Vec Float)) (Vec Float)))
          ((Vec (Vec Float)) (Vec Float)))

(edef R$mul (LM (Vec Float) (Tuple (Vec (Vec Float)) (Vec Float)))
          ((Vec (Vec Float)) (Vec Float)))

(def fwd$mul (Vec Float)
          ((M_v : (Tuple (Vec (Vec Float)) (Vec Float))) (dM_dv : (Tuple (Vec (Vec Float)) (Vec Float))))
     (let ((M  (get$1$2 M_v))
           (v  (get$2$2 M_v))
           (dM (get$1$2 dM_dv))
           (dv (get$2$2 dM_dv)))
    (ts_add (mul dM v) (mul M dv))))

(edef rev$mul (Tuple (Vec (Vec Float)) (Vec Float))
          ((Tuple (Vec (Vec Float)) (Vec Float)) (Vec Float)))
(def shape$rev$mul (Tuple (Vec (Vec (Tuple))) (Vec (Tuple)))
          ((M_v : (Tuple (Vec (Vec Float)) (Vec Float))) (dr : Vec Float))
    (shape M_v))

(def mvmul (Vec Float) ((a : Vec (Vec Float)) (b : Vec Float))
   (mul a b))

(def sqnorm Float (v : Vec Float)
  (let (n (size v))
   (sum (build n (lam (i : Integer) (sqr (index i v)))))))

;;(def map (Vec 'b) ((f : Lambda 'a 'b) (v : Vec 'a))
;;   (build n (lam (i : Integer) (f (index i v)))))

(def gt@Float Bool ((a : Float) (b : Float))
   (gt a b))
