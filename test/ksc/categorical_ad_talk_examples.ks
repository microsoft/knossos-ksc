(def f1 Float (x : Float) x)

(def f2 Float (x : Float) (add x x))

(def f3 Float ((x : Float) (y : Float)) y)

(def f4 Float ((x : Float) (y : Float)) (add x y))

(def f5 Float ((x : Float) (y : Float)) (add y x))

(def f5 Float ((x : Float) (y : Float)) (add x (mul y x)))

(def f6 Float ((x : Float) (y : Float)) (mul y y))

(def f7 Float ((x : Float) (y : Float)) (add x (mul y y)))
