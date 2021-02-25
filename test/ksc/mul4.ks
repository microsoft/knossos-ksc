; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.


(def h Float ( (x1 : Float) (x2 : Float) (x3 : Float) (x4 : Float) )
       (mul x1 (mul x2 (mul x3 x4))))

(gdef fwd [h (Tuple Float Float Float Float)])
(gdef rev [h (Tuple Float Float Float Float)])
(gdef suffwdpass [h (Tuple Float Float Float Float)])
(gdef sufrevpass [h (Tuple Float Float Float Float)])
(gdef sufrev [h (Tuple Float Float Float Float)])

; (def times ( (x1 : Float) (x2 : Float) ) (mul x1 x2))
; (def h ( (x1 : Float) (x2 : Float) (x3 : Float) (x4 : Float) )
;       (times x1 (times x2 (times x3 x4))))

(def main Integer () 0)
