; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def vsum Float ( (i : Integer) (v : Vec n Float) )
       (if (== i 0) 0.0 (+ (index i v) (vsum (+ i 1) v))))
