; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f2a Float (x : Float)
     (let ((zt (let (y1 (* x x))
                   (tuple y1 (+ x y1))))
           (y2 (get$1$2 zt))
           (z  (get$2$2 zt)))
     (* y2 z)))
