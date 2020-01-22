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

(edef max Float (Float Float))
(edef D$max (LM Float Float) (Float Float))
(edef Dt$max (Tuple Float (LM Float Float)) (Float Float))

(edef $ranhashdoub Float (Integer))
(edef D$$ranhashdoub (LM Integer Float) (Integer))
(def fwd$$ranhashdoub Float ((x : Integer) (dx : (Tuple))) (0.0))
(def rev$$ranhashdoub (Tuple) ((x : Integer) (d_dranhashdoub : Float)) (tuple))
(edef Dt$$ranhashdoub (Tuple Float (LM Integer Float)) (Integer))
