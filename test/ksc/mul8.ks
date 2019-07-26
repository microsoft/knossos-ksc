; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def xtimes Float ( (x1 : Float) (x2 : Float) ) (mul x1 x2))

(def times Float ( (x1 : Float) (x2 : Float) ) (mul x1 (mul x2 2.0)))

; (def h ( (x1 : Float) (x2 : Float) (x3 : Float) (x4 : Float)
;         (x5 : Float) (x6 : Float) (x7 : Float) (x8 : Float))
;       (mul x1 (mul x2 (mul x3 (mul x4 (mul x5 (mul x6 (mul x7 x8))))))))

(def h Float
       ( (x1 : Float) (x2 : Float) (x3 : Float) (x4 : Float)
         (x5 : Float) (x6 : Float) (x7 : Float) (x8 : Float))
       (times x1 (times x2 (times x3 (times x4 (times x5 (times x6 (times x7 x8))))))))

