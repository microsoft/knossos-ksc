;; Implementations taken from

;; examples/dl-activations/gelu.py
;; examples/dl-activations/relu3.py
;; src/ts2k/ts2ks/ts2ks.py

;; TODO: Use map instead of build, once it is implemented

(def mean Float (x : Vec Float) (div (sum x) (to_float (size x))))

(gdef fwd [mean (Vec Float)])
(gdef rev [mean (Vec Float)])
(gdef suffwdpass [mean (Vec Float)])
(gdef sufrevpass [mean (Vec Float)])
(gdef sufrev [mean (Vec Float)])

(def gelu Float (x : Float)
     (mul 0.5 (mul x (add 1.0 (erf (div x (sqrt 2.0)))))))

(gdef fwd [gelu Float])
(gdef rev [gelu Float])
(gdef suffwdpass [gelu Float])
(gdef sufrevpass [gelu Float])
(gdef sufrev [gelu Float])

(def vgelu (Vec Float) (v : Vec Float)
     (build (size v) (lam (i : Integer) (gelu (index i v)))))

(gdef fwd [vgelu (Vec Float)])
(gdef rev [vgelu (Vec Float)])
(gdef suffwdpass [vgelu (Vec Float)])
(gdef sufrevpass [vgelu (Vec Float)])
(gdef sufrev [vgelu (Vec Float)])

(def sqrl Float (x : Vec Float)
     (let (y (mean x))
     (let (t (if (lt y 0.0)
                 (build (size x) (lam (i : Integer) (mul -0.125 (index i x))))
               (build (size x) (lam (i : Integer) (mul 0.5 (mul (index i x) (index i x)))))))
     (let (r (build (size t) (lam (i : Integer) (mul (sin (index i t)) (index i t)))))
     (mean r)))))

(gdef fwd [sqrl (Vec Float)])
(gdef rev [sqrl (Vec Float)])
(gdef suffwdpass [sqrl (Vec Float)])
(gdef sufrevpass [sqrl (Vec Float)])
(gdef sufrev [sqrl (Vec Float)])

(def relu3 Float (x : Float)
     (if (lt x 0.0)
         0.0
     (if (lt x 1.0)
         (div (mul x (mul x x)) 3.0)
     (sub x (div 2.0 3.0)))))

(gdef fwd [relu3 Float])
(gdef rev [relu3 Float])
(gdef suffwdpass [relu3 Float])
(gdef sufrevpass [relu3 Float])
(gdef sufrev [relu3 Float])

(def vrelu3 (Vec Float) (v : Vec Float)
     (build (size v) (lam (i : Integer) (relu3 (index i v)))))

(gdef fwd [vrelu3 (Vec Float)])
(gdef rev [vrelu3 (Vec Float)])
(gdef suffwdpass [vrelu3 (Vec Float)])
(gdef sufrevpass [vrelu3 (Vec Float)])
(gdef sufrev [vrelu3 (Vec Float)])

(def main Integer () 0)
