; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f2a Float (x : Float)
     (let ((zt (let (y1 (mul@ff x x))
                   (tuple y1 (add@ff x y1))))
           (y2 (get$1$2 zt))
           (z  (get$2$2 zt)))
     (mul@ff y2 z)))
