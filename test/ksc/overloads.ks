(def f Float ((a : Float) (b : Float))
    3.3)

(def f (Vec Float) ((a : Float) (b : Vec Float))
    (build (size b) (lam (i : Integer) (f a (index i b)))))

(def Integer_init Integer (a : Float)
    2)
(def Integer_init Integer ((b : Vec Float))
    3)

(def main Integer ()
    (print "I=" (Integer_init (f 1.2 2.3))))
