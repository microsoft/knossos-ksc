(def sub@1V[[Float]],Float,V[[Float]] (Vec n Float) ((a : Vec n Float) (b : Float))
    (build n (lam (i : Integer) (- (index i a) b))))

(def expv (Vec n Float) (v : Vec n Float)
   (build n (lam (i : Integer) (exp (index i v)))))

(def sum@1Float Float (v : Vec n Float)
   (sum v))

(def log@1Float Float (v : Float)
   (log v))

(def add@1Float,Float,Float Float ((a : Float) (b : Float))
   (+ a b))

(def *@1Integer,Integer,Integer Integer ((a : Float) (b : Float))
   (* a b))

(def sub@1Integer,Integer,Integer Integer ((a : Integer) (b : Integer))
   (- a b))


