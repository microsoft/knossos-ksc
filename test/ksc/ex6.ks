; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def f7 Float ( (x : Vec n Float) (y : Vec n Float) )
        (sum (build n (lam (i : Integer)
                           (mul (index i x) (index i y))))))
