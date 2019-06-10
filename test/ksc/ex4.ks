; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f5 Float ( (x : Float) (y : Float) )
        (let ((p (* 7.0 x))
              (r (/ 1.0 y))
              (q (* p (* x 5.0)))
              (v (+ (* 2.0 (* p q)) (* 3.0 r))))
        v))
