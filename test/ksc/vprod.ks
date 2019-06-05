; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def vprod Float ( (i : Integer) (acc : Float) (v : Vec n Float) )
     (if (== i n) 1.0 (vprod (+ i 1) (* acc (index i v)) v)))

