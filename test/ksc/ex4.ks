; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f5 Float ( (x : Float) (y : Float) )
        (let ((p (mul@ff 7.0 x))
              (r (div@ff 1.0 y))
              (q (mul@ff p (mul@ff x 5.0)))
              (v (add@ff (mul@ff 2.0 (mul@ff p q)) (mul@ff 3.0 r))))
        v))
