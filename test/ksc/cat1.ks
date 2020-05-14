; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
;; (def g Float ( (x : Float) (y : Float) )
;;     x) ;;  (mul@ff y x))


(def g1 Float (x : Float)
  (let (y (add x x))
   (add y y)))

(def g2 Float (x : Float)
  (let (y (add x x))
   (add y x)))

(def g3 Float (x : Float)
  (let (y (add x x))
   (mul y x)))

(def g4 Float (x : Float)
  (let (y (mul x x))
   (mul y x)))

(def g5 Float (x : Float)
   (let (y (add x x))
   (let (z (mul y x))
     z)))

(def g6 Float( (x : Float) (y : Float) )
    x)
(def g7 Float( (x : Float) (y : Float) )
    y)

(def g8 Float( (y : Vec Float) )
    3.0)
