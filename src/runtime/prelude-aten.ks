;; Definitions for aten functions
;; edefs will go in prelude-aten.cpp

(def aten::lt Bool ((a : Float) (b : Float))
    (lt a b))

(def aten::mul Float ((a : Float) (b : Float))
    (mul a b))

(def aten::add Float ((a : Float) (b : Float))
    (add a b))
