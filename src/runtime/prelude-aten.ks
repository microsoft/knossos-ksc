;; Definitions for aten functions
;; edefs will go in prelude-aten.cpp

(def aten::item Float (x : Float)
    x)
(gdef fwd [aten::item Float])
(gdef rev [aten::item Float])
(gdef suffwdpass [aten::item Float])
(gdef sufrevpass [aten::item Float])
(gdef sufrev [aten::item Float])

(def aten::lt Bool ((a : Float) (b : Float))
    (lt a b))
(gdef fwd [aten::lt (Tuple Float Float)])
(gdef rev [aten::lt (Tuple Float Float)])
(gdef suffwdpass [aten::lt (Tuple Float Float)])
(gdef sufrevpass [aten::lt (Tuple Float Float)])
(gdef sufrev [aten::lt (Tuple Float Float)])

(def aten::lt Bool ((a : Integer) (b : Float))
    (lt (to_float a) b))
(gdef fwd [aten::lt (Tuple Integer Float)])
(gdef rev [aten::lt (Tuple Integer Float)])
(gdef suffwdpass [aten::lt (Tuple Integer Float)])
(gdef sufrevpass [aten::lt (Tuple Integer Float)])
(gdef sufrev [aten::lt (Tuple Integer Float)])

(def aten::lt Bool ((a : Integer) (b : Integer))
    (lt a b))
(gdef fwd [aten::lt (Tuple Integer Integer)])
(gdef rev [aten::lt (Tuple Integer Integer)])
(gdef suffwdpass [aten::lt (Tuple Integer Integer)])
(gdef sufrevpass [aten::lt (Tuple Integer Integer)])
(gdef sufrev [aten::lt (Tuple Integer Integer)])

;; mul
(def aten::mul Float ((a : Float) (b : Float))
    (mul a b))
(gdef fwd [aten::mul (Tuple Float Float)])
(gdef rev [aten::mul (Tuple Float Float)])
(gdef suffwdpass [aten::mul (Tuple Float Float)])
(gdef sufrevpass [aten::mul (Tuple Float Float)])
(gdef sufrev [aten::mul (Tuple Float Float)])

(def aten::mul Float ((a : Float) (b : Integer))
    (mul a (to_float b)))
(gdef fwd [aten::mul (Tuple Float Integer)])
(gdef rev [aten::mul (Tuple Float Integer)])
(gdef suffwdpass [aten::mul (Tuple Float Integer)])
(gdef sufrevpass [aten::mul (Tuple Float Integer)])
(gdef sufrev [aten::mul (Tuple Float Integer)])

;; add
(def aten::add Float ((a : Float) (b : Float))
    (add a b))
(gdef fwd [aten::add (Tuple Float Float)])
(gdef rev [aten::add (Tuple Float Float)])
(gdef suffwdpass [aten::add (Tuple Float Float)])
(gdef sufrevpass [aten::add (Tuple Float Float)])
(gdef sufrev [aten::add (Tuple Float Float)])

(def aten::add Integer ((a : Integer) (b : Integer))
    (add a b))
(gdef fwd [aten::add (Tuple Integer Integer)])
(gdef rev [aten::add (Tuple Integer Integer)])

;; sub
(def aten::sub Float ((a : Float) (b : Float))
    (sub a b))
(gdef fwd [aten::sub (Tuple Float Float)])
(gdef rev [aten::sub (Tuple Float Float)])
(gdef suffwdpass [aten::sub (Tuple Float Float)])
(gdef sufrevpass [aten::sub (Tuple Float Float)])
(gdef sufrev [aten::sub (Tuple Float Float)])

(def aten::sub Integer ((a : Integer) (b : Integer))
    (sub a b))
(gdef fwd [aten::sub (Tuple Integer Integer)])
(gdef rev [aten::sub (Tuple Integer Integer)])
(gdef suffwdpass [aten::sub (Tuple Integer Integer)])
(gdef sufrevpass [aten::sub (Tuple Integer Integer)])
(gdef sufrev [aten::sub (Tuple Integer Integer)])

;; div
(def aten::div Integer ((a : Integer) (b : Integer))
    (div  a b))
(gdef fwd [aten::div (Tuple Integer Integer)])
(gdef rev [aten::div (Tuple Integer Integer)])
(gdef suffwdpass [aten::div (Tuple Integer Integer)])
(gdef sufrevpass [aten::div (Tuple Integer Integer)])
(gdef sufrev [aten::div (Tuple Integer Integer)])

(def aten::div Float ((a : Float) (b : Float))
    (div  a b))
(gdef fwd [aten::div (Tuple Float Float)])
(gdef rev [aten::div (Tuple Float Float)])
(gdef suffwdpass [aten::div (Tuple Float Float)])
(gdef sufrevpass [aten::div (Tuple Float Float)])
(gdef sufrev [aten::div (Tuple Float Float)])

;; neg
(def aten::neg Float (a : Float)
    (neg a))
(gdef fwd [aten::neg Float])
(gdef rev [aten::neg Float])
(gdef suffwdpass [aten::neg Float])
(gdef sufrevpass [aten::neg Float])
(gdef sufrev [aten::neg Float])

;; sin
(def aten::sin Float (a : Float)
    (sin a))
(gdef fwd [aten::sin Float])
(gdef rev [aten::sin Float])
(gdef suffwdpass [aten::sin Float])
(gdef sufrevpass [aten::sin Float])
(gdef sufrev [aten::sin Float])

;; Float
(def aten::Float Float (a : Integer)
    (to_float a))
(gdef fwd [aten::Float Integer])
(gdef rev [aten::Float Integer])
(gdef suffwdpass [aten::Float Integer])
(gdef sufrevpass [aten::Float Integer])
(gdef sufrev [aten::Float Integer])

(def aten::Float Float (a : Float)
    a)
(gdef fwd [aten::Float Float])
(gdef rev [aten::Float Float])
(gdef suffwdpass [aten::Float Float])
(gdef sufrevpass [aten::Float Float])
(gdef sufrev [aten::Float Float])

(def aten::Bool Bool (a : Bool)
    a)
(gdef fwd [aten::Bool Bool])
(gdef rev [aten::Bool Bool])
(gdef suffwdpass [aten::Bool Bool])
(gdef sufrevpass [aten::Bool Bool])
(gdef sufrev [aten::Bool Bool])

;; Bool
(def aten::Bool Bool (a : Float)
    (not (eq a 0.0)))
(gdef fwd [aten::Bool Float])
(gdef rev [aten::Bool Float])
(gdef suffwdpass [aten::Bool Float])
(gdef sufrevpass [aten::Bool Float])
(gdef sufrev [aten::Bool Float])

;; a^n
(def aten::pow Float ((a : Float) (n : Integer))
    (pow a n))
(gdef fwd [aten::pow (Tuple Float Integer)])
(gdef rev [aten::pow (Tuple Float Integer)])
(gdef suffwdpass [aten::pow (Tuple Float Integer)])
(gdef sufrevpass [aten::pow (Tuple Float Integer)])
(gdef sufrev [aten::pow (Tuple Float Integer)])



;; prod
(def aten::prod Float (a : Tuple Float Float)
    (mul (get$1$2 a) (get$2$2 a)))
(gdef fwd [aten::prod (Tuple Float Float)])
(gdef rev [aten::prod (Tuple Float Float)])
(gdef suffwdpass [aten::prod (Tuple Float Float)])
(gdef sufrevpass [aten::prod (Tuple Float Float)])
(gdef sufrev [aten::prod (Tuple Float Float)])

(def aten::prod Integer (a : Tuple Integer Integer)
    (mul (get$1$2 a) (get$2$2 a)))
(gdef fwd [aten::prod (Tuple Integer Integer)])
(gdef rev [aten::prod (Tuple Integer Integer)])
(gdef suffwdpass [aten::prod (Tuple Integer Integer)])
(gdef sufrevpass [aten::prod (Tuple Integer Integer)])
(gdef sufrev [aten::prod (Tuple Integer Integer)])


(def [aten::erf Float] Float (x : Float) (erf x))
(def [suffwdpass [aten::erf Float]] (Tuple Float Float) (x : Float) ([suffwdpass erf] x))
(def [sufrevpass [aten::erf Float]] Float (t : Tuple Float Float) ([sufrevpass [erf Float]] t))
