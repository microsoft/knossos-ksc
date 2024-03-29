;; Implementations taken from

;; examples/dl-activations/gelu.py
;; examples/dl-activations/relu3.py
;; src/python/ksc/torch_frontend.py

;; TODO: Enable LM AD gdefs, once map is implemented for LM AD

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
     (map (lam (vi : Float) (gelu vi)) v))

;; (gdef fwd [vgelu (Vec Float)])
;; (gdef rev [vgelu (Vec Float)])
(gdef suffwdpass [vgelu (Vec Float)])
(gdef sufrevpass [vgelu (Vec Float)])
(gdef sufrev [vgelu (Vec Float)])

(def sqrl Float (x : Vec Float)
     (let (y (mean x))
     (let (t (if (lt y 0.0)
                 (map (lam (xi : Float) (mul -0.125 xi)) x)
               (map (lam (xi : Float) (mul 0.5 (mul xi xi))) x)))
     (let (r (map (lam (ti : Float) (mul (sin ti) ti)) t))
     (mean r)))))

;; (gdef fwd [sqrl (Vec Float)])
;; (gdef rev [sqrl (Vec Float)])
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
     (map (lam (vi : Float) (relu3 vi)) v))

;; (gdef fwd [vrelu3 (Vec Float)])
;; (gdef rev [vrelu3 (Vec Float)])
(gdef suffwdpass [vrelu3 (Vec Float)])
(gdef sufrevpass [vrelu3 (Vec Float)])
(gdef sufrev [vrelu3 (Vec Float)])

(def [vrelu3_inlined (Vec Float)] (Vec Float)
     (t : Vec Float)
     (map (lam (x : Float)
               (if (lt x 0.0)
                   0.0
                 (if (lt x 1.0)
                     (div (mul x (mul x x)) 3.0)
                   (sub x (div 2.0 3.0))))) t))

(def [sufrev [vrelu3_inlined (Vec Float)]] (Vec Float)
     ((t : Vec Float) (dret : Vec Float))
     ; TODO: should be multiplied by dret[i] - luckily we are called with dret==1.0
     (map (lam (x : Float)
               (if (lt x 0.0)
                   0.0
                 (if (lt x 1.0)
                     (mul x x)
                   1.0))) t))

(def [vrelu3_mask (Vec Float)] (Vec Float)
     (t : Vec Float)
     (map (lam (x : Float)
        (let (val0to1 (mul x (mul x (div x 3.0))))
        (let (val1up (sub x (div 2.0 3.0)))
        (let (in0to1 (lte x 1.0))
           (mul (bool_to_float (gt x 0.0))
                (add (mul (bool_to_float in0to1) val0to1)
                     (mul (bool_to_float (not in0to1)) val1up))))))) t))

(def [sufrev [vrelu3_mask (Vec Float)]] (Vec Float)
     ((t : Vec Float) (dret : Vec Float))
     (map2 (lam (x_ddri : Tuple Float Float)
        (let ((x ddri) x_ddri)
        (let (val0to1 (mul x x))
        (let (val1up 1.0)
        (let (in0to1 (lte x 1.0))
           (mul (mul (bool_to_float (gt x 0.0))
                     (add (mul (bool_to_float in0to1) val0to1)
                          (mul (bool_to_float (not in0to1)) val1up)))
                ddri)))))) t dret))


(def main Integer () 0)
