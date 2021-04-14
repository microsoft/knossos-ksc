; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def xtimes Float ( (x1 : Float) (x2 : Float) ) (mul x1 x2))

(gdef fwd [xtimes (Tuple Float Float)])
(gdef rev [xtimes (Tuple Float Float)])
(gdef suffwdpass [xtimes (Tuple Float Float)])
(gdef sufrevpass [xtimes (Tuple Float Float)])
(gdef sufrev [xtimes (Tuple Float Float)])

(def times Float ( (x1 : Float) (x2 : Float) ) (mul x1 (mul x2 2.0)))

(gdef fwd [times (Tuple Float Float)])
(gdef rev [times (Tuple Float Float)])
(gdef suffwdpass [times (Tuple Float Float)])
(gdef sufrevpass [times (Tuple Float Float)])
(gdef sufrev [times (Tuple Float Float)])

; (def h ( (x1 : Float) (x2 : Float) (x3 : Float) (x4 : Float)
;         (x5 : Float) (x6 : Float) (x7 : Float) (x8 : Float))
;       (mul x1 (mul x2 (mul x3 (mul x4 (mul x5 (mul x6 (mul x7 x8))))))))

(def h Float
       ( (x1 : Float) (x2 : Float) (x3 : Float) (x4 : Float)
         (x5 : Float) (x6 : Float) (x7 : Float) (x8 : Float))
       (times x1 (times x2 (times x3 (times x4 (times x5 (times x6 (times x7 x8))))))))

(gdef fwd [h (Tuple Float Float Float Float Float Float Float Float)])
(gdef rev [h (Tuple Float Float Float Float Float Float Float Float)])
(gdef suffwdpass [h (Tuple Float Float Float Float Float Float Float Float)])
(gdef sufrevpass [h (Tuple Float Float Float Float Float Float Float Float)])
(gdef sufrev [h (Tuple Float Float Float Float Float Float Float Float)])

(def main Integer () 0)
