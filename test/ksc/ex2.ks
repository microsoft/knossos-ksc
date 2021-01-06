; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f2a Float (x : Float)
     (let ((zt (let (y1 (mul x x))
                   (tuple y1 (add x y1))))
           (y2 (get$1$2 zt))
           (z  (get$2$2 zt)))
     (mul y2 z)))

(def main Integer () 0)
