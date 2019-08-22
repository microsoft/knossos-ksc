; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.


(def f Float ((x : Float) (y : Float))
     (let ((p (mul 7.0 x))
           (r (div 1.0 y))
           (q (mul (mul p x) 5.0))
           (v (add (mul (mul 2.0 p) q) (mul 3.0 r))))
       v))
