(def foo Float ((x : Float) (y : Float))
    (let (a (add@ff (mul@ff 2.0 x) (mul@ff 3.0 y)))
		(let (b (add@ff (mul@ff 4.0 a) (mul@ff 5.0 a)))
		  (add@ff (mul@ff 6.0 b) (mul@ff 7.0 b)))))
