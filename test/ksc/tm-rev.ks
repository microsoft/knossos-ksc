(def foo Float ((x : Float) (y : Float))
    (let (a (add (mul 2.0 x) (mul 3.0 y)))
		(let (b (add (mul 4.0 a) (mul 5.0 a)))
		  (add (mul 6.0 b) (mul 7.0 b)))))

(def main Integer () 0)
