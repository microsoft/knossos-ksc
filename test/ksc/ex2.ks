; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f2a Float (x : Float)
     (let ((zt (let (y1 (mul x x))
                   (tuple y1 (add x y1))))
           (y2 (get$1$2 zt))
           (z  (get$2$2 zt)))
     (mul y2 z)))

(gdef fwd [f2a Float])
(gdef rev [f2a Float])
(gdef suffwdpass [f2a Float])
(gdef sufrevpass [f2a Float])
(gdef sufrev [f2a Float])

(def main Integer () 0)
