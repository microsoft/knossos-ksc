; Copyright (c) Microsoft Corporation.
; Licensed under the MIT license.
(def h (Vec Float) (x : Vec Float)
    (build (size x) (lam (i : Integer) 1.0)))

(def g Float (x : Vec Float)
    (sum (h x)))
