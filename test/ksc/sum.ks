; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def vsum Float ( (i : Integer) (v : Vec Float) )
       (if (eq i 0) 0.0 (add (index i v) (vsum (add i 1) v))))

(gdef fwd [vsum (Tuple Integer (Vec Float))])
(gdef rev [vsum (Tuple Integer (Vec Float))])
; SUF/BOG-AD does not support recursion
; (gdef suffwdpass [vsum (Tuple Integer (Vec Float))])
; (gdef sufrevpass [vsum (Tuple Integer (Vec Float))])
; (gdef sufrev [vsum (Tuple Integer (Vec Float))])

(def main Integer () 0)
