; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
; (def ix Float ( (x : Vec Float) (i : Integer) )
;    (index i x))

; (def ix Float ( x : Vec Float )
;   (index 1 x))

(def mulvec (Vec Float) ( (x : (Vec Float)) (y : (Vec Float)) )
     (build (size x) (lam (i : Integer) (index i x))))

(def f6 Float ( (x : (Vec Float)) (y : (Vec Float)) )
        (sum (mulvec x y)))
