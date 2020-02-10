; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def vsum Float ( (i : Integer) (v : Vec Float) )
       (if (eq i 0) 0.0 (add@ff (index i v) (vsum (add@ii i 1) v))))
