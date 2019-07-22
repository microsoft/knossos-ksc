(def expv (Vec n Float) (v : Vec n Float)
   (build n (lam (i : Integer) (exp (index i v)))))

(def sum@V[[Float]] Float (v : Vec n Float)
   (sum v))

(def log@Float Float (v : Float)
   (log v))

(edef gammaLn Float (Float))
(edef D$gammaLn (LM Float Float) (Float))
(edef fwd$gammaLn Float (Float Float))
(edef rev$gammaLn Float (Float Float))
(edef Dt$gammaLn (Tuple Float (LM Float Float)) (Float))

(edef pow@Float Float ((Float) (Float)))
(edef D$pow@Float (LM (Tuple Float Float) Float) (Float Float))
(edef Dt$pow@Float (Tuple Float (LM (Tuple Float Float) Float)) (Float Float))
(edef fwd$pow@Float Float (Float Float Float Float))
(edef rev$pow@Float (Tuple Float Float) (Float Float Float))

(def add@Float,Float Float ((a : Float) (b : Float))
   (+ a b))

(def add@Integer,Integer Integer ((a : Integer) (b : Integer))
   (+ a b))

(def *@Float,Float Float ((a : Float) (b : Float))
   (* a b))

(def *@Float,Float Float ((a : Float) (b : Float))
   (* a b))

(def *@Integer,Integer Integer ((a : Integer) (b : Integer))
   (* a b))

(def /@Integer,Integer  Integer ((a : Integer) (b : Integer))
   (/ a b))

(def sub@Integer,Integer  Integer ((a : Integer) (b : Integer))
   (- a b))

(def sub@Float,Float Float ((a : Float) (b : Float))
   (- a b))

(def sub@V[[Float]],Float (Vec n Float) ((a : Vec n Float) (b : Float))
    (build n (lam (i : Integer) (- (index i a) b))))

(def sub@V[[Float]],V[[Float]] (Vec n Float) ((a : Vec n Float) (b : Vec n Float))
    (build n (lam (i : Integer) (- (index i a) (index i b)))))

(def sqr Float (a : Float)
   (* a a))

; mul Mat Vec
(edef mul$Mat$Vec (Vec m Float) ((Vec m (Vec n Float)) (Vec n Float)))

(edef D$mul$Mat$Vec (LM (Tuple (Vec m (Vec n Float)) (Vec n Float)) (Vec m Float))
          ((Vec m (Vec n Float)) (Vec n Float)))
(edef Dt$mul$Mat$Vec (Tuple (Vec m Float) (LM (Tuple (Vec m (Vec n Float)) (Vec n Float)) (Vec m Float)))
          ((Vec m (Vec n Float)) (Vec n Float)))

(edef R$mul$Mat$Vec (LM (Vec m Float) (Tuple (Vec m (Vec n Float)) (Vec n Float)))
          ((Vec m (Vec n Float)) (Vec n Float)))

(def fwd$mul$Mat$Vec (Vec m Float)
          ((M : Vec m (Vec n Float)) (v : Vec n Float) (dM : Vec m (Vec n Float)) (dv : Vec n Float))
    (+ (mul$Mat$Vec dM v) (mul$Mat$Vec M dv)))

(edef rev$mul$Mat$Vec (Tuple (Vec m (Vec n Float)) (Vec n Float))
          ((Vec m (Vec n Float)) (Vec n Float) (Vec m Float)))

(def mvmul (Vec m Float) ((a : Vec m (Vec n Float)) (b : Vec n Float))
   (mul$Mat$Vec a b))

(def sqnorm Float (v : Vec n Float)
   (sum (build n (lam (i : Integer) (sqr (index i v))))))

;;(def map (Vec n 'b) ((f : Lambda 'a 'b) (v : Vec n 'a))
;;   (build n (lam (i : Integer) (f (index i v)))))

(def >@Float Bool ((a : Float) (b : Float))
   (> a b))
