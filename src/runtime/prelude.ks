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

(edef max Float (Float Float))
(edef D$max (LM Float Float) (Float Float))
(edef Dt$max (Tuple Float (LM Float Float)) (Float Float))
