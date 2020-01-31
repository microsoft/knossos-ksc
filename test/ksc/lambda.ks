# Should be auto-translated from lambdas.jl one day

(def make_f (Lambda Float Float) (a : Float)
    (let ((b (mul 2.0 a)))
        (lam (t : Float) (sin (mul b t)))))

(def g Float ((a : Float) (x : Float))
    (let (f (lam (t : Float) (sin (mul a t))))
        (f (cos x))))


(def gf Float ((env : Float) (t : Float))
     (sin (mul env t)))

(def g1 Float ((a : Float) (x : Float))
     (gf a (cos x)))

(def main Integer ()
     (print "g=" (g1 5.0 7.0) "\n"
            "grad = " (rev$g1 5.0 7.0 1.0) "\n"
            ))
